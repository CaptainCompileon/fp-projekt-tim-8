module Indexer
  (indexPages, invertedIndex)
where

import System.IO
import Data.List
import Data.Maybe
import Data.Char (isAlpha, toLower, isSpace)
import qualified Data.Set
import Paths_stack_project (getDataFileName)
import Debug.Trace (traceShow)

indexPages :: [(Int, [String])] -> [(String, Int)]
indexPages [] = []
indexPages (page:restOfPages) =
    (indexSinglePage page) ++ (indexPages restOfPages)

indexSinglePage :: (Int, [String]) -> [(String, Int)]
indexSinglePage (pageNum, []) = []
indexSinglePage (pageNum, (firstWord:restOfWords)) = 
    (firstWord, pageNum) : indexSinglePage (pageNum, restOfWords)

invertedIndex :: [(String, Int)] -> [(String, [Int])]
invertedIndex [] = []
invertedIndex fullList@(x:xs) = 
  -- traceShow (x, xs)
    (processSingleWord x) : (invertedIndex $ deleteWord x xs)
        where processSingleWord (word, pageNum) =
                  let newList = filter (\(currWord, currPageNum) -> currWord == word) fullList
                      newWordList = nub $ map (\(str, n) -> n) newList
                  in (word, newWordList)
              deleteWord _ [] = []
              deleteWord word wordList =
                  filter (\(currWord, currPageNum) -> currWord /= (fst word)) wordList