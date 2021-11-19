module P6 where
import Data.List (sort)
import Test.QuickCheck

propRetainsLength :: (Ord a, Arbitrary a) => [a] -> Bool
propRetainsLength xs = length (sort xs) == length xs

propFirstSmallest :: (Ord a, Arbitrary a) => [a] -> Bool
propFirstSmallest xs = all (>= head sorted) sorted
  where sorted = sort xs

propIncreasing :: (Ord a, Arbitrary a) => [a] -> Bool
propIncreasing xs = and $ zipWith (>=) sorted (tail sorted)
  where sorted = sort xs