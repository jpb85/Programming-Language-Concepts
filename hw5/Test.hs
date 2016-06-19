module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import HW05

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [prob1Tests, prob2Tests]

prob1Tests :: Test
prob1Tests = testGroup "Problem 1: Implementing the Luhn Algorithm"
    [ testCase "lastDigit 123 == 3" $ lastDigit 123 @?= 3
    , testCase "lastDigit 0 == 0" $ lastDigit 0 @?= 0
    , testCase "dropLastDigit 123 == 12" $ dropLastDigit 123 @?= 12
    , testCase "dropLastDigit 5 == 0" $ dropLastDigit 5 @?= 0
    , testCase "toDigits 1234 == [1,2,3,4]" $ toDigits 1234 @?= [1,2,3,4]
    , testCase "toDigits 0 == []" $ toDigits 0 @?= []
    , testCase "toDigits (-17) == []" $ toDigits (-17) @?= []
    , testCase "doubleEveryOther [8,7,6,5] == [16,7,12,5]" $
               doubleEveryOther [8,7,6,5] @?= [16,7,12,5]
    , testCase "doubleEveryOther [1,2,3] == [1,4,3]" $
               doubleEveryOther [1,2,3] @?= [1,4,3]
    , testCase "sumDigits [16,7,12,5] == 22" $
               sumDigits [16,7,12,5] @?= 22
    , testCase "validate 4012888888881881 = True" $
               validate 4012888888881881 @?= True
    , testCase "validate 4012888888881882 = False" $
               validate 4012888888881882 @?= False
    ]

prob2Tests :: Test
prob2Tests =  testGroup "Problem 2: Tower of Hanoi"
    [ testCase "hanoi 2 \"a\" \"b\" \"c\" == [(\"a\",\"c\"), (\"a\",\"b\"), (\"c\",\"b\")]" $
               hanoi 2 "a" "b" "c" @?= [("a","c"), ("a","b"), ("c","b")]
    ]
