{-# LANGUAGE ScopedTypeVariables #-}

module MinimizeDfa where

import Data.List (nub, sort)

import Fixpoint
import Nfa
import Set

data Part q = Part (Set (Set q)) (q -> q -> Bool)

instance Eq q => Eq (Part q) where
    Part s _ == Part s' _ = s == s'

-- Take a predicate and a set and partition the set into equivalence classes
-- under the predicate.
partition :: forall a . Ord a => (a -> a -> Bool) -> Set a -> Set (Set a)
partition f s =
    fromList (part (toList s))
  where
    part :: Ord a => [a] -> [Set a]
    part = foldr addtoclass []

    addtoclass :: Ord a => a -> [Set a] -> [Set a]
    addtoclass a []                          = [singleton a]
    addtoclass a (c:r) | f a (firstMember c) = insert a c:r
                       | otherwise           = c : addtoclass a r

    firstMember :: Set a -> a
    firstMember = head . toList

minimizeDfa :: Ord q => Nfa q -> Nfa q
minimizeDfa nfa =
    removeDeadStates (mapNfa mini nfa)
  where
    mini a         = minimum (toList (eqClass a))
    eqClass a      = head [ b | b <- toList classes, a `member` b]
    Part classes _ = equivClasses nfa

-- Remove all dead states from an NFA
removeDeadStates :: forall q . Ord q => Nfa q -> Nfa q
removeDeadStates nfa = filterNfa (not . deadState nfa) nfa

-- A dead state is a state that has transitions to itself on all symbols in the
-- alphabet. All states will be marked dead in an NFA that only matches the
-- empty string (the alphabet of this NFA will be empty), so we never mark the
-- start state as dead. We also never mark an accepting state as dead.
deadState :: Ord q => Nfa q -> q -> Bool
deadState nfa@(Nfa _ moves q0 accepting) q =
    q0 /= q &&
    not (member q accepting) &&
    sort (alphabet nfa) == (nub . sort) [c | Move r c s <- toList moves,
                                             r == q, s == q]

equivClasses :: forall q . Ord q => Nfa q -> Part q
equivClasses (Nfa states moves _ fin) =
    fixpoint step start
  where
    start :: Part q
    start = Part firstpart firstpartfun
      where
        firstpart = partition firstpartfun states
        firstpartfun a b = member a fin == member b fin

    step :: Part q -> Part q
    step (Part _ partfun) =
        Part newpart newpartfun
      where
        newpart = partition newpartfun states
        newpartfun q r =
            partfun q r &&
            all (uncurry partfun) [(s, t) | Move q' c  s <- movelist, q == q',
                                            Move r' c' t <- movelist, r == r',
                                            c == c']
    movelist :: [Move q]
    movelist = toList moves
