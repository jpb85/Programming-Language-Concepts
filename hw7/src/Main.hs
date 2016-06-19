{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import System.Console.GetOpt
import System.Environment

import RegExp
import RegExpMatch
import RegExpToNfa
import Nfa
import NfaMatch
import NfaToDot
import NfaToDfa
import MinimizeDfa
import DfaToC

main :: IO ()
main = do
  (flags, reString) <- getArgs >>= parseOpts
  re <- case parseRegExp reString of
          Just re -> return re
          Nothing -> fail $ "Cannot parse regular expression: " ++ reString
  let nfa = regExpToNfa re
  writeDot (nfaToDot nfa) (nfaDotFile flags)
  let dfa = nfaToDfa nfa
  writeDot (nfaToDot dfa) (dfaDotFile flags)
  let minDfa = numberNfaFrom 0 (minimizeDfa dfa)
  writeDot (nfaToDot minDfa) (minDfaDotFile flags)
  writeDot (dfaToC minDfa) (cFile flags)
  printMatchResult "naive matcher" reString (regExpMatch re) (matchString flags)
  printMatchResult "nfa matcher"   reString (nfaMatch nfa)   (matchString flags)
  printMatchResult "dfa matcher"   reString (nfaMatch dfa)   (matchString flags)

writeDot :: String -> Maybe FilePath -> IO ()
writeDot _   Nothing     = return ()
writeDot dot (Just path) = writeFile path dot

printMatchResult :: String -> String -> (String -> Bool) -> Maybe String -> IO ()
printMatchResult _ _ _ Nothing = return ()
printMatchResult desc re match (Just cs) = do
    let didMatch = match cs
    putStrLn $ re ++ " " ++ (if didMatch then "matched" else "did not match") ++ " " ++
               show cs ++ " using the " ++ desc

options :: [OptDescr Flag]
options =
 [ Option [] ["nfa-dot"]     (ReqArg NfaDotFile "FILE")    "dump NFA to specified dot file"
 , Option [] ["dfa-dot"]     (ReqArg DfaDotFile "FILE")    "dump DFA to specified dot file"
 , Option [] ["min-dfa-dot"] (ReqArg MinDfaDotFile "FILE") "dump minimized DFA to specified dot file"
 , Option [] ["to-c"]        (ReqArg CFile "FILE")         "dump minimized DFA to specified dot file"
 , Option [] ["match"]       (ReqArg Match "STRING")       "attempt to match STRING"
 ]

parseOpts :: [String] -> IO ([Flag], String)
parseOpts argv =
    case getOpt Permute options argv of
      (o,[re],[]) -> return (o,re)
      (_,_,errs)  -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: fsm [OPTION...] REGEXP"

data Flag = NfaDotFile FilePath
          | DfaDotFile FilePath
          | MinDfaDotFile FilePath
          | CFile FilePath
          | Match String
  deriving (Eq, Show)

nfaDotFile :: [Flag] -> Maybe FilePath
nfaDotFile fs =
    case [path | NfaDotFile path <- fs] of
      []     -> Nothing
      path:_ -> Just path

dfaDotFile :: [Flag] -> Maybe FilePath
dfaDotFile fs =
    case [path | DfaDotFile path <- fs] of
      []     -> Nothing
      path:_ -> Just path

minDfaDotFile :: [Flag] -> Maybe FilePath
minDfaDotFile fs =
    case [path | MinDfaDotFile path <- fs] of
      []     -> Nothing
      path:_ -> Just path

cFile :: [Flag] -> Maybe FilePath
cFile fs =
    case [path | CFile path <- fs] of
      []     -> Nothing
      path:_ -> Just path

matchString :: [Flag] -> Maybe String
matchString fs =
    case [path | Match path <- fs] of
      []     -> Nothing
      path:_ -> Just path
