module Main where

import PageRank (createEdges, process)
import Paths_stack_project (getDataFileName)

main :: IO ()
main = do
  fName <- getDataFileName "files/input.txt"
  f <- readFile fName
  forwardName <- getDataFileName "files/forward2.txt"
  forward <- readFile forwardName
  -- damping factor defaults to 0.85
  writeFile "output2.txt" $ show createEdges
  writeFile "output.txt" $ show $ process f 3 0.85
