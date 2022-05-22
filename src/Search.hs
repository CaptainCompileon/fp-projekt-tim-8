module Search
  ( searchExpressions,
    searchWord,
  )
where

import Data.List (find)
import Data.Maybe (fromJust, fromMaybe)
import Helpers (position)

type Page = String

searchExpressions :: [(String, [Int])] -> String -> [[[Int]]]
searchExpressions invertedIndex expression =
  let wordss = words expression --pridat rozseknutie stringu na viac slov
   in map (`searchWord` invertedIndex) wordss

searchWord :: (Eq a) => a -> [(a, b)] -> [b]
searchWord x = map snd . filter ((== x) . fst)
