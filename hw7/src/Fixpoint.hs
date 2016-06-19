module Fixpoint (fixpoint) where

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x | x == x'   = x
             | otherwise = fixpoint f x'
  where
    x' = f x
