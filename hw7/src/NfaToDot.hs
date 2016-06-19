module NfaToDot (
    nfaToDot
  ) where

import Data.List (intersperse)

import Nfa
import Set

nfaToDot :: Ord q => Nfa q -> String
nfaToDot = toDot . numberNfaFrom 0

toDot :: Nfa Int -> String
toDot (Nfa qs moves q0 fin) =
    stack $
    ["digraph fsm {"
    ,"rankdir=\"LR\";"
    ,"start [shape=\"plaintext\",label=\"start\"];"
    ,startEdge q0]
    ++
    map stateToDot (toList qs)
    ++
    map moveToDot (toList moves)
    ++
    ["}"] 
 where
    startEdge :: Int -> String
    startEdge q = "start->" ++ show q ++ ";"

    -- Given a state, return a line of dot syntax that will create a node for
    -- the state.
    stateToDot :: Int -> String
    stateToDot q | q == (maximum (toList qs)) =  show q ++  "[shape="  ++ "\"doublecircle\"" ++ ",label=\"q" ++ show q ++"\"];"
                 | otherwise = show q ++  "[shape=" ++ "\"circle\""  ++ ",label=\"q" ++  show q ++ "\"];"
    -- Given a move between states, return a line of dot syntax that will create
    -- an edge for the transition.
    moveToDot :: Move Int -> String
    moveToDot (Move x z y) = show x ++ "->"  ++ show y   ++ "[label=\"" ++ (z:"") ++  "\"];"
    moveToDot (Emove x y) = show x ++ "->" ++  show y  ++ "[label=\"" ++ epsilon  ++ "\"];"

epsilon :: String
epsilon = "ε"

-- Separate a list of strings by newlines
stack :: [String] -> String
stack = concat . intersperse "\n"

--1.5 hours
