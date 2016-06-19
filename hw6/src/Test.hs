module Main where

import Data.List (sort)
import Data.Maybe (fromJust)

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@?=))

import While

import qualified Interpreter as I
import qualified MonadicInterpreter as M
import qualified OutputInterpreter as O

type Z = Integer

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    testInterpreters "skip" "skip" ([], [])
    ++
    testInterpreters "assignment" "a := 3" ([("a", 3)], [])
    ++
    testInterpreters "seq" "a := 3; b := 4" ([("a", 3), ("b", 4)], [])
    ++
    testInterpreters "if" "a := 3; if a = 3 then b := 5 else b := 6" ([("a", 3), ("b", 5)], [])
    ++
    testInterpreters "while" "sum := 0; i := 1; while i <= 10 (sum := sum + i; i := i + 1)" ([("i", 11), ("sum", 55)], [])
    ++
    testInterpreters "output" "sum := 0; i := 1; while i <= 10 (sum := sum + i; i := i + 1); out sum" ([("i", 11), ("sum", 55)], [55])

testInterpreters :: String -> String -> ([(Var, Z)], [Z]) -> [Test]
testInterpreters desc s (vs, out) =
    [testCase (showTestCase "initial interpreter") $
         let vs' = I.sStep stm I.emptyState
         in
           sort vs' @?= sort vs
    ,testCase (showTestCase "monadic interpreter") $
         let ((), vs') = M.runI (M.sStep stm) M.emptyState
         in
           sort vs' @?= sort vs
    ,testCase (showTestCase "output interpreter") $
         let ((), out', vs') = O.runI (O.sStep stm) O.emptyState
         in
           (out', sort vs') @?= (out, sort vs)]
  where
    stm :: Stm
    stm = fromJust $ parseWhile s

    showTestCase :: String -> String
    showTestCase interpDesc = desc ++ " (" ++ interpDesc ++ ")"
