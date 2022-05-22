{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import qualified Data.ByteString.Internal as BS (c2w)
import qualified Data.ByteString.Lazy as LB
import Data.List
import GHC.Base
import Indexer (indexPages, invertedIndex)
import PageRank (createEdges, position, process')
import Paths_stack_project (getDataFileName)
import Text.HTML.TagSoup

data WebPageData = WebPageData {url :: String, htmlContent :: String} deriving (Show)

instance FromJSON WebPageData where
  parseJSON (Object v) = do
    url <- v .: "url"
    htmlContent <- v .: "html_content"
    return (WebPageData {url = url, htmlContent = htmlContent})
  parseJSON _ = empty

justifyList :: [Maybe WebPageData] -> [WebPageData]
justifyList xs = [x | Just x <- xs]

main :: IO ()
main = do
  -- PARSER
  res <- LB.readFile "./files/collection_10.jl"
  let line = LB.split (BS.c2w '\n') res
  let decoded = map (\x -> decode x :: Maybe WebPageData) line
  let filtered = justifyList decoded

  let urls = map (\x -> url x) filtered
  let htmls = map (\x -> htmlContent x) filtered

  let words = map (\x -> Prelude.words . innerText . dropWhile (~/= ("<body>" :: String)) . takeWhile (~/= ("</body>" :: String)) $ parseTags x) htmls
  let anchors = map (\x -> (filter (/= "") $ (fromAttrib ("href" :: String) <$> (dropWhile (~/= ("<body>" :: String)) . takeWhile (~/= ("</body>" :: String)) $ filter isTagOpen $ parseTags x)))) htmls

  let indexedUrls = (zip [1 .. (length urls)] urls)
  let indexedWords = (zip [1 .. (length words)] words)
  let indexedAnchors = (zip [1 .. (length anchors)] anchors)

  -- REVERSE INDEX
  let wordPagePairs =
        indexPages indexedWords
      reverseIndexList = invertedIndex wordPagePairs
  writeFile "files/indexerOutput.txt" $ show reverseIndexList

  -- PAGE RANK
  let allLinks = [["bing", "lol"], ["google", "lol"], ["google"]]
  let urlList = ["google", "bing", "lol"]

  writeFile "files/edgesOutput.txt" $ show $ createEdges (map (map (`position` urlList)) allLinks)
  writeFile "files/pageRankOutput.txt" $ show $ process' (map (map (`position` urlList)) allLinks) 3 0.85

  -- SEARCH
  -- handle <- openFile "files/indexerOutput.txt" ReadMode
  -- coord <- hGetLine handle
  -- print (map read <$> lines <$> hGetContents handle :: IO [(String, [Int])])
  -- invIndex <- (read coord :: [(String, [Int])])

  putStrLn "Type expression to search:"
  searchExpression <- getLine
  print (searchExpressions finalList searchExpression)
