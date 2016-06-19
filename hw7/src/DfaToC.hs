module DfaToC (dfaToC) where

import Data.List (intersperse)

import Nfa
import Set

dfaToC :: Dfa Int -> String
dfaToC nfa@(Nfa _ moves q0 f) =
    stack $
    ["int match(const char* cs)"
    ,"{"
    ,"  int state = " ++ show q0 ++ ";"
    ,"  int accept = " ++ showIsFinalState q0 ++ ";"
    ,"  while (1) {"
    ,"    switch (*(cs++)) {"
    ]
    ++
    map genCharCase (alphabet nfa)
    ++
    ["      case '\\0': return accept;"
    ,"      default: return 0;"
    ,"    }"
    ,"  }"
    ,"}"
    ,""]
  where
    -- Generate a case statement for state transitions on the character c.

    genCharCase :: Char -> String
    genCharCase c = 
        stack $
        ["      case " ++ show c ++ ":","        switch (state) {"] ++ map genTransitionCase [(q1, q2) | Move q1 c' q2 <- toList moves, c' == c]           ++["          default: return 0;" ,"        }","        break;" ]

    -- Generate a case statement for the given transition. We must set the C
    -- variables state and accept to the proper values.
    genTransitionCase :: (Int,Int) -> String
    genTransitionCase (q,q') = 
        stack $["          case " ++ show q ++ ":","            state = " ++ show q' ++";","            accept = " ++ showIsFinalState q' ++ ";","            break;"]    

   
    moveList :: [Move Int]
    moveList = toList moves

    isFinalState :: Int -> Bool
    isFinalState q = q `member` f

    showIsFinalState :: Int -> String
    showIsFinalState = show . boolToInt . isFinalState

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

-- Separate a list of strings by newlines
stack :: [String] -> String
stack = concat . intersperse "\n"


--2 hours
