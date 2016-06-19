module RegExpMatch (regExpMatch) where

import RegExp

regExpMatch :: RegExp -> String -> Bool
regExpMatch Empty _ =
    False

regExpMatch Epsilon "" =
    True

regExpMatch (Lit c) [c'] =
    c == c'

regExpMatch (Cat r s) cs =
    or [regExpMatch r cs1 && regExpMatch s cs2 | (cs1,cs2) <- splits cs]

regExpMatch (Alt r s) cs =
    regExpMatch r cs || regExpMatch s cs

regExpMatch (Star r) cs =
    regExpMatch Epsilon cs ||
    or [regExpMatch r cs1 && regExpMatch (Star r) cs2 | (cs1,cs2) <- frontSplits cs]

regExpMatch _ _ =
    False

-- Returns all the ways of splitting a list into two halves.
splits :: [a] -> [([a], [a])]
splits st = [splitAt n st | n <- [0 .. length st]]

-- Returns all the ways of splitting a list into two halves where the first
-- split is non-trivial
frontSplits :: [a] -> [([a], [a])]
frontSplits st = [splitAt n st | n <- [1.. length st]]
