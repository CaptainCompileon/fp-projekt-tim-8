module Main where

-- import PageRank (process)
import PageRank (createEdges, position, process')
import Paths_stack_project (getDataFileName)

main :: IO ()
main = do
  let allLinks = [["bing", "lol"], ["google", "lol"], ["google"]]
  let urlList = ["google", "bing", "lol"]

  writeFile "output2.txt" $ show $ createEdges (map (map (`position` urlList)) allLinks)
  writeFile "output.txt" $ show $ process' (map (map (`position` urlList)) allLinks) 3 0.85
