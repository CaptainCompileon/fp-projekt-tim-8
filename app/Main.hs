{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Indexer (computeIndexesTest, indexPages, invertedIndex)
import PageRank (createEdges, position, process')
import Paths_stack_project (getDataFileName)
import Data.Aeson
import Text.HTML.TagSoup
import GHC.Base
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Internal as BS (c2w)

data WebPageData = WebPageData { url :: String, htmlContent :: String } deriving Show

instance FromJSON WebPageData where
  parseJSON (Object v) = do
      url <- v .: "url"
      htmlContent <- v .: "html_content"
      return (WebPageData { url = url, htmlContent = htmlContent })
  parseJSON _ = empty

justifyList :: [Maybe WebPageData] -> [WebPageData]
justifyList xs = [ x | Just x <- xs ]

main :: IO ()
main = do
  res <- LB.readFile "./files/collection_1.jl"
  let line = LB.split (BS.c2w '\n') res
  let decoded = map(\x -> decode x :: Maybe WebPageData) line
  let filtered = justifyList decoded

  let urls = map(\x -> url x) filtered
  let htmls = map(\x -> htmlContent x) filtered

  let words = map(\x -> Prelude.words . innerText . dropWhile (~/= ("<body>"::String)) . takeWhile (~/= ("</body>"::String)) $ parseTags x) htmls
  let anchors = map(\x -> (filter (/= "") $ (fromAttrib ("href"::String) <$> (dropWhile (~/= ("<body>"::String)) . takeWhile (~/= ("</body>"::String)) $ filter isTagOpen $ parseTags x)))) htmls
  
  print urls
  print words
  print anchors
  print "done"
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

  -- PAGE RANK
  let allLinks = [["bing", "lol"], ["google", "lol"], ["google"]]
  let urlList = ["google", "bing", "lol"]

  writeFile "files/edgesOutput.txt" $ show $ createEdges (map (map (`position` urlList)) allLinks)
  writeFile "files/pageRankOutput.txt" $ show $ process' (map (map (`position` urlList)) allLinks) 3 0.85
