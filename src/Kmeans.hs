module Kmeans (kmeans) where

-- nub weeds duplicate items from a list
import Data.List (nub, sort)
import Lib (distance, filterGroupPoints, kRandomUniqIndices, minIndex, pickPoints, sel1)

-- NOTE : We want "Num" type instead of "RealFloat" type. Investigate later

-- calculate distance of each point from each centroid
distanceFromCentroids :: RealFloat a => a -> [a] -> [a]
distanceFromCentroids point = map (distance point)

-- given a list of points and a list of centroid positions return a new mapping that describes which
-- cluster each point belongs to
getClusterMapping :: RealFloat a => [a] -> [a] -> [Int]
getClusterMapping points centroidPositions = [sel1 (minIndex (distanceFromCentroids point centroidPositions)) | point <- points]

-- calculate new centroid for a given cluster
getNewCentroidPosition :: RealFloat a => [a] -> a
getNewCentroidPosition clusterPoints = sum clusterPoints / fromIntegral (length clusterPoints)

-- Given points and the cluster mapping calculate the new centroids
-- New centroid of a given group will be the mean of all points in that group
getNewCentroidPositions :: RealFloat a => [a] -> [Int] -> [a]
getNewCentroidPositions points clusterMapping = [getNewCentroidPosition (filterGroupPoints points clusterMapping cluster) | cluster <- sort (nub clusterMapping)]

kmeansHelper :: RealFloat a => [a] -> Int -> Int -> [a] -> ([a], [Int])
kmeansHelper points _ 0 clusterPos = (clusterPos, getClusterMapping points clusterPos)
kmeansHelper points nClusters nIters clusterPos = kmeansHelper points nClusters (nIters - 1) newCentroidPositions
  where
    newCentroidPositions = getNewCentroidPositions points (getClusterMapping points clusterPos)

-- Returns the centroid positions(total k) and the mapping of which cluster each point
-- belongs to ( a list of length n and each element between 0 to n_cluster - 1)
kmeans :: RealFloat a => [a] -> Int -> Int -> ([a], [Int])
kmeans points nClusters nIters = kmeansHelper points nClusters nIters initialClusterPos
  where
    initialClusterPos = pickPoints points (kRandomUniqIndices nClusters)