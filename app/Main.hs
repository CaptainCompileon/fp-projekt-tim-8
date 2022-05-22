module Main where

import Data.List
import Helpers (position)
import Indexer (computeIndexesTest, indexPages, invertedIndex)
import PageRank (createEdges, process')
import Paths_stack_project (getDataFileName)
import Search (searchExpressions)
import System.IO

main :: IO ()
main =
  do
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

    -- REVERSE INDEX
    let wordPagePairs =
          indexPages
            [ (1, ["and", "first", "here", "is", "page", "something", "the", "there", "this", "written"]),
              (2, ["a", "about", "and", "be", "here", "is", "it", "nice", "no", "page", "really", "rocks", "second", "should", "so", "something", "the", "there", "theres", "this", "written"]),
              (3, ["cat", "dog", "giraffe", "labradudl", "pelican"]),
              (4, ["apple", "lemon", "orange", "strawberry", "watermelon"])
            ]
        finalList = invertedIndex wordPagePairs
    writeFile "files/indexerOutput.txt" $ show finalList

    -- -- PAGE RANK
    -- let allLinks = [["bing", "lol"], ["google", "lol"], ["google"]]
    -- let urlList = ["google", "bing", "lol"]

    -- writeFile "files/edgesOutput.txt" $ show $ createEdges (map (map (`position` urlList)) allLinks)
    -- writeFile "files/pageRankOutput.txt" $ show $ process' (map (map (`position` urlList)) allLinks) 3 0.85

    -- SEARCH
    -- handle <- openFile "files/indexerOutput.txt" ReadMode
    -- coord <- hGetLine handle
    -- print (map read <$> lines <$> hGetContents handle :: IO [(String, [Int])])
    -- invIndex <- (read coord :: [(String, [Int])])

    putStrLn "Type expression to search:"
    searchExpression <- getLine
    print (searchExpressions finalList searchExpression)
