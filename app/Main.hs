module Main where


import Paths_stack_project (getDataFileName)
import PageRank (process)

main :: IO ()
main = do
  putStrLn "How many iters?"
  numIters <- getLine
  fName <- getDataFileName "files/input.txt"
  f <- readFile fName
  -- damping factor defaults to 0.85
  writeFile "output.txt" $ show $ process f (read numIters :: Int) 0.85
