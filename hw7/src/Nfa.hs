module Nfa (
    Nfa(..),
    Dfa,
    Move(..),

    alphabet,
    nfaSize,

    epsilonAccessible,
    epsilonClosure,

    onemove,
    onetrans,

    mapNfa,
    filterNfa,
    numberNfaFrom
  ) where

import Data.List (nub, sort)

import Fixpoint
import Set

-- | An NFA where states have type @q@
data Nfa q = Nfa { -- | The set of all NFA states
                   allStates       :: Set q
                   -- | The set of all NFA transitions
                 , allMoves        :: Set (Move q)
                   -- | The (single) NFA start state
                 , startState      :: q
                   -- | The set of accepting states
                 , acceptingStates :: Set q
                 }
  deriving (Eq, Show, Read)

-- | An NFA transition
data Move q = Emove q q     -- ^ An epsilon transition between two states
            | Move q Char q -- ^ A transition between two states on the
                            -- character @c@
  deriving (Eq, Ord, Show, Read)

type Dfa q = Nfa q

-- | Return the number of states in the 'Nfa'
nfaSize :: Nfa q -> Int
nfaSize (Nfa qs _ _ _) = size qs

-- | Return the alphabet of and NFA by finding a list of the characters mentioned
-- in the NFA's moves.
alphabet :: Nfa q -> [Char]
alphabet (Nfa _ moves _ _) =
    nub $ sort [c | Move _ c _ <- toList moves]

-- | Given an NFA and a set of states @qs@, return the set of states that are
-- accessible from @qs@ via a single epsilon transition.
epsilonAccessible :: Ord q => Nfa q -> Set q -> Set q
epsilonAccessible (Nfa _ moves _ _) qs =
    fromList accessible
  where
    accessible = [r | q <- toList qs,
                      Emove q' r <- toList moves,
                      q == q']

-- | Given an NFA and a set of states @qs@, return the set of states that are
-- accessible from @qs@ via zero or more epsilon transitions.
epsilonClosure :: Ord q => Nfa q -> Set q -> Set q
epsilonClosure nfa qs0 =
    fixpoint addEpsilonAccessible qs0
  where
    addEpsilonAccessible qs = qs `union` epsilonAccessible nfa qs

-- | Return the set of states accessible from the set of states @qs@ by a single transition on the
-- character @c@.
onemove :: Ord q => Nfa q -> Set q -> Char -> Set q
onemove (Nfa _ moves _ _) qs c =
    fromList [s | q <- toList qs,
                  Move r c' s <- toList moves,
                  r == q,
                  c == c']

-- | Return the set of states accessible from the set of states @qs@ by a single transition on the
-- character @c@ followed by zero or more epsilon transitions.
onetrans :: Ord q => Nfa q -> Set q -> Char -> Set q
onetrans nfa q c = epsilonClosure nfa (onemove nfa q c)

-- | Map a function across the states of an 'Nfa'
mapNfa :: (Ord q, Ord q') => (q -> q') -> Nfa q -> Nfa q'
mapNfa f (Nfa qs moves q0 fin) =
    Nfa (mapSet f qs) (mapSet (mapMove f) moves) (f q0) (mapSet f fin)

-- | Map a function across the states of a 'Move'
mapMove :: (q -> q') -> Move q -> Move q'
mapMove f (Emove q r)  = Emove (f q) (f r)
mapMove f (Move q c r) = Move (f q) c (f r)

-- | Filter the states of an 'Nfa'
filterNfa :: Ord q => (q -> Bool) -> Nfa q -> Nfa q
filterNfa f (Nfa qs moves q0 fin) =
    Nfa (filterSet f qs) (filterSet (filterMove f) moves) q0 (filterSet f fin)

-- | Filter the states of a 'Move'
filterMove :: (q -> Bool) -> Move q -> Bool
filterMove f (Emove q r)  = f q && f r
filterMove f (Move q _ r) = f q && f r

-- | Re-number the states of the 'Nfa' @nfa@ starting from @i@
numberNfaFrom :: Ord q => Int -> Dfa q -> Dfa Int
numberNfaFrom i nfa@(Nfa qs _ _ _) = mapNfa number nfa
  where
    stateMap = toList qs `zip` [i..]
    number q = look q stateMap

    look :: Eq a => a -> [(a,b)] -> b
    look _ []                       = error "look"
    look k ((k',v):kvs) | k == k'   = v
                        | otherwise = look k kvs
