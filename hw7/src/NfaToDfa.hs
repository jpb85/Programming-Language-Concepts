module NfaToDfa (nfaToDfa) where

import Fixpoint
import Nfa
import Set

-- Convert an Nfa into a Dfa
nfaToDfa ::  Nfa Int -> Dfa Int
nfaToDfa = numberNfaFrom 0 . makeDfa
  where
    makeDfa :: Nfa Int -> Dfa (Set Int)
    makeDfa nfa = deterministic nfa (alphabet nfa)

--  deterministic nfa alpha is the result of forming the dfa based on sets of
--  states of nfa using the alphabet alpha.
deterministic :: Nfa Int -> [Char] -> Dfa (Set Int)
deterministic nfa@(Nfa _ _ q0 f) alpha =
    fixpoint (addstep nfa alpha) (Nfa (singleton q0') empty q0' f')
  where
    q0' = epsilonClosure nfa (singleton q0)
    f' | isEmpty (f `intersection` q0') = empty
       | otherwise                      = singleton q0'

-- addmove nfa q c dfa will add to dfa the moves from state set q on symbol c.
addmove :: Ord q => Nfa q -> Set q -> Char -> Dfa (Set q) -> Dfa (Set q)
addmove nfa@(Nfa _ _ _ f0) q c (Nfa qs moves q0 f) =
    Nfa qs' moves' q0 f'
  where
    qs'                                  = qs    `union` singleton new
    moves'                               = moves `union` singleton (Move q c new)
    f' | isEmpty (f0 `intersection` new) = f
       | otherwise                       = f `union` singleton new
    new                                  = onetrans nfa q c

-- addmoves nfa q alpha dfa will add to dfa all the moves from state q of nfa
-- over the alphabet alpha.
addmoves :: Ord q => Nfa q -> Set q -> [Char] -> Dfa (Set q) -> Dfa (Set q)
addmoves _   _ []     dfa = dfa
addmoves nfa q (c:cs) dfa = addmoves nfa q cs (addmove nfa q c dfa)

-- addstep adds all the new states which can be made by a single transition on
-- one of the characters of the alphabet.
addstep :: Ord q => Nfa q -> [Char] -> Dfa (Set q) -> Dfa (Set q)
addstep nfa alpha dfa0@(Nfa qs _ _ _) =
    add dfa0 (toList qs)
  where
    add dfa []     = dfa
    add dfa (r:rs) = add (addmoves nfa r alpha dfa) rs
