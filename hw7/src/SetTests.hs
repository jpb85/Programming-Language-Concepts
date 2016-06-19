{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List ((\\), nub, sort)
import qualified Data.List as L

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit ((@?=))
import Test.QuickCheck

import Set

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary = (Set . nub . sort) `fmap` arbitrary

    shrink (Set xs) = map Set (shrink xs)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "invariant" prop_invariant
        , testProperty "empty" prop_empty
        , testProperty "isEmpty" prop_isEmpty
        , testProperty "singleton" prop_singleton
        , testProperty "size" prop_size
        , testProperty "fromList" prop_fromList
        , testProperty "member" prop_member
        , testProperty "insert" prop_insert
        , testProperty "delete" prop_delete
        , testProperty "union" prop_union
        , testProperty "intersection" prop_intersection
        , testProperty "difference" prop_difference
        , testProperty "isSubsetOf" prop_isSubsetOf
        ]

invariant :: Ord a => Set a -> Bool
invariant (Set xs) = xs == (nub . sort) xs

infix 4 ==?
(==?) :: Ord a => Set a -> [a] -> Bool
s ==? xs = invariant s && toList s == (nub . sort) xs

prop_invariant :: Set Int -> Bool
prop_invariant s = invariant s

prop_empty :: Bool
prop_empty = empty ==? ([] :: [Int])

prop_isEmpty :: Set Int -> Bool
prop_isEmpty s = isEmpty s == null (toList s)

prop_singleton :: Int -> Bool
prop_singleton x = singleton x ==? [x]

prop_size :: Set Int -> Property
prop_size s = cover (size s >= 15) 60 "large" $
              size s == length (toList s)

prop_fromList :: [Int] -> Bool
prop_fromList xs = fromList xs ==? xs

prop_member :: Int -> [Int] -> Bool
prop_member x xs = member x (fromList xs) == (x `elem` xs)

prop_insert :: Int -> Set Int -> Bool
prop_insert x s = insert x s ==? x : toList s

prop_delete :: Int -> Set Int -> Bool
prop_delete x s = delete x s ==? toList s \\ [x]

prop_union :: Set Int -> Set Int -> Bool
prop_union s1 s2 = s1 `union` s2 ==? toList s1 ++ toList s2

prop_intersection :: Set Int -> Set Int -> Bool
prop_intersection s1 s2 = s1 `intersection` s2 ==? [x | x <- toList s1, x `elem` toList s2]

prop_difference :: Set Int -> Set Int -> Bool
prop_difference s1 s2 = s1 `difference` s2 ==? toList s1 \\ toList s2

prop_isSubsetOf :: Set Int -> Set Int -> Bool
prop_isSubsetOf s1 s2 = s1 `isSubsetOf` s2 == and [x `elem` toList s2 | x <- toList s1]
