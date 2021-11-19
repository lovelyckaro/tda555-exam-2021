module P8 where
import Test.QuickCheck
import Data.List (permutations, sort)
import qualified Data.Map as M
import Data.Map (Map)

factorial :: Int -> Int
factorial n = product [1..n]

newtype SmallList a = SmallList [a]
  deriving Show

instance Arbitrary a => Arbitrary (SmallList a) where
  arbitrary = do
    xs <- arbitrary
    let good = take 8 xs
    return (SmallList good)

propCorrectLength :: SmallList a -> Bool
propCorrectLength (SmallList xs) = length (permutations xs) == factorial (length xs)

isPerm :: (Eq a, Ord a )=> [a] -> [a] -> Bool
isPerm xs ys = sort xs == sort ys

propPermCorrect :: Ord a => SmallList a -> Bool
propPermCorrect (SmallList xs) = all (isPerm xs) (permutations xs)