module Lib
  ( someFunc,
    filterGroupPoints,
    distance,
    minIndex,
    sel1,
    kRandomUniqIndices,
    pickPoints,
  )
where

import Data.List (nub, unfoldr)
import System.Random
  ( RandomGen,
    StdGen,
    UniformRange,
    mkStdGen,
    uniformR,
  )

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Get list of points belonging to a particular group
filterGroupPoints :: [a] -> [Int] -> Int -> [a]
filterGroupPoints points groups group =
  [x | (x, y) <- zip points groups, y == group]

-- calculate distance between two points
distance :: RealFloat a => a -> a -> a
distance a b
  | a > b = a - b
  | otherwise = b - a

-- NOTE : Use binary search?
-- get index of minimum element and minimum element in an array
-- Return value : (idx, value_idx)
minIndex :: (Num a1, Ord a2) => [a2] -> (a1, a2)
minIndex [] = error "empty list"
minIndex (x : xs)
  | null xs = (0, x)
  | x <= xs_min = (0, x)
  | otherwise = (1 + xs_min_idx, xs_min)
  where
    (xs_min_idx, xs_min) = minIndex xs

sel1 :: (a, b) -> a
sel1 (a, _) = a

-- Get initial k random values in range [0,k-1]
pureGen :: StdGen
pureGen = mkStdGen 42

oneRandom :: (RandomGen g, UniformRange a, Num a) => a -> g -> (a, g)
oneRandom k = uniformR (0, k - 1)

infRandoms :: (RandomGen b, UniformRange a, Num a) => a -> b -> [a]
infRandoms k = unfoldr (Just . oneRandom k)

kRandomUniqIndices :: Int -> Int -> [Int]
kRandomUniqIndices n k = take k (nub ((infRandoms n) pureGen))

-- Pick values from points whose index is in positions
pickPoints :: [a] -> [Int] -> [a]
pickPoints points positions = [points !! i | i <- positions]