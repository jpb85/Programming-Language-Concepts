module NfaMatch (nfaMatch) where

import Nfa
import Set

nfaMatch :: Nfa Int -> String -> Bool
nfaMatch (Nfa xs move x0 f) string = not (empty == (helperFun (Nfa xs move x0 f) string `intersection` f))



helperFun :: Ord a => Nfa a -> String -> Set a
helperFun (Nfa xs move x0 f) string = foldNFA step1 mainset string
		 where
		 step1 setx chx = onetrans (Nfa xs move x0 f) setx chx
		 mainset = epsilonClosure (Nfa xs move x0 f) (singleton x0)

foldNFA :: (Set x  -> Char -> Set x) ->  Set x  ->  String -> Set x
foldNFA f a [] =  a
foldNFA f a (x:xs) = foldNFA  f (f a x) xs

-- 2 hours
