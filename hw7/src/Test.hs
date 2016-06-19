module Main where

import Data.Maybe (fromJust)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import Set
import PowerSet

import MinimizeDfa
import Nfa
import NfaMatch
import NfaToDfa
import RegExp
import RegExpMatch
import RegExpToNfa

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = testNonRegexpNFAs
        ++
        testPowerSet
        ++
        testMatches ""
          [("", True),  ("a", False)]
        ++
        testMatches "a"
          [("", False), ("a", True), ("b", False)]
        ++
        testMatches "a*"
          [("", True), ("a", True), ("aa", True), ("aaa", True), ("b", False)]
        ++
        testMatches "a*a"
          [("", False), ("a", True), ("aa", True), ("aaa", True), ("b", False)]
        ++
        testMatches "ab"
          [("", False), ("ab", True), ("a", False), ("aa", False)]
        ++
        testMatches "(ab)*"
          [("", True), ("ab", True), ("a", False), ("aa", False), ("abab", True)]
        ++
        testMatches "a|b"
          [("", False), ("a", True), ("b", True), ("ab", False)]
        ++
        testMatches "a*c"
          [("c", True), ("ac", True), ("aaaac", True)]
        ++
        testMatches "(a|bc)d*"
          [("a", True), ("bc", True), ("ad", True), ("bcd", True), ("abc", False)]
        ++
        testMatches "a*(b|c)*"
          [("aaaabcbcccbb", True)]

testNonRegexpNFAs :: [Test]
testNonRegexpNFAs = [testCase "Non-regexp NFA" $ nfaMatch nfa "a" @?= True
                    ,testCase "Non-regexp NFA" $ nfaMatch nfa "b" @?= False]
  where
    nfa = Nfa (fromList [0..6])
              (fromList [Emove 0 1
                        ,Move 1 'b' 2
                        ,Emove 0 3
                        ,Move 3 'a' 4
                        ,Emove 0 5
                        ,Move 5 'a' 6])
              0
              (fromList [3,4])

testPowerSet :: [Test]
testPowerSet = [testCase "PowerSet empty" $
                powerSet (empty :: Set Int) @?= (singleton empty :: Set (Set Int))
               ,testCase "PowerSet singleton" $
                powerSet (singleton (1 :: Int)) @?= fromList [empty, singleton 1]
               ,testCase "PowerSet {1,2,3}" $
                powerSet s @?= p]
  where
    s :: Set Int
    s = fromList [1,2,3]

    p :: Set (Set Int)
    p = fromList [empty, singleton 1, singleton 2, singleton 3
                 ,fromList [1,2], fromList [1,3], fromList [2,3]
                 ,s]

testMatches :: String -> [(String, Bool)] -> [Test]
testMatches res = concat . map (uncurry (testMatch res))

testMatch :: String -> String -> Bool -> [Test]
testMatch res s doesMatch =
    [testCase (showTestCase "regExpMatch")     $ regExpMatch re s @?= doesMatch
    ,testCase (showTestCase "nfaMatch on NFA") $ nfaMatch nfa s @?= doesMatch
    ,testCase (showTestCase "nfaMatch on DFA") $ nfaMatch minDfa s @?= doesMatch]
  where
    re :: RegExp
    re = fromJust $ parseRegExp res

    nfa :: Nfa Int
    nfa = regExpToNfa re

    dfa :: Dfa Int
    dfa = nfaToDfa nfa

    minDfa :: Dfa Int
    minDfa = numberNfaFrom 0 (minimizeDfa dfa)

    showTestCase :: String -> String
    showTestCase desc = desc ++ " " ++ show res ++ " " ++ show s
