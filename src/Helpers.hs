module Helpers
  ( position,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromJust)

position :: Eq a => a -> [a] -> Int
position i xs = fromJust (i `elemIndex` xs)
