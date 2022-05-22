module Main where

import Paths_stack_project (getDataFileName)
-- import PageRank (process)
import Indexer (computeIndexesTest, indexPages, invertedIndex)
import Data.List

main :: IO ()
main = do
  --  ocakavam na vstupe data vo formate: [(pageId1,[word1,word2,...]),(pageId2,[word3,...]),...]
  --  napr.:
  --  [(1,["and","first","here","is","page","something","the","there","this","written"]),
  --   (2,["a","about","and","be","here","is","it","nice","no","page","really","rocks","second","should","so","something","the","there","theres","this","written"]),
  --   (3,["cat","dog","giraffe","labradudl","pelican"]),
  --   (4,["apple","lemon","orange","strawberry","watermelon"])]
  --  treba zavolat metodu indexPages

  --  vystupom su data vo formate: [(word1,[pageId1,pageId2]),(word2,[pageId2]).......]
  --  napr.:
  --  [("and",[1,2]),("first",[1]),("here",[1,2]),.....]

  -- vypocet indexov
    -- let wordPagePairs = indexPages (zip [1,3,5] [["a", "b"],["c"],["d"]])
    let wordPagePairs = indexPages [(1,["and","first","here","is","page","something","the","there","this","written"]),
                                    (2,["a","about","and","be","here","is","it","nice","no","page","really","rocks","second","should","so","something","the","there","theres","this","written"]),
                                    (3,["cat","dog","giraffe","labradudl","pelican"]),
                                    (4,["apple","lemon","orange","strawberry","watermelon"])]
        finalList = invertedIndex wordPagePairs
    writeFile "files/indexerOutput.txt" $ show $ finalList




  -- putStrLn "How many iters?"
  -- numIters <- getLine
  -- fName <- getDataFileName "files/input.txt"
  -- f <- readFile fName
  -- -- damping factor defaults to 0.85
  -- writeFile "output.txt" $ show $ process f (read numIters :: Int) 0.85