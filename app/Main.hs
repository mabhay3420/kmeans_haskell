module Main (main) where

import Kmeans (kmeans)
import Vector(Vector(Vec))

main :: IO ()
main = do
          print (kmeans [Vec [1,2,4,5,6], Vec[2,3,4,5,6], Vec[3,4,3,4,5], Vec[4,5,4,5,6]] 3 4)