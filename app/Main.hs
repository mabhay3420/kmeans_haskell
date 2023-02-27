module Main (main) where

import Kmeans (kmeans)

main :: IO ()
main = do
  print (kmeans [1, 2, 3] 2 4 :: ([Float], [Int]))