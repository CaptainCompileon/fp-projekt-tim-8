{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module PageRankOld
  ( process,
  )
where

import qualified Control.Monad
import Data.Functor ((<&>))
import qualified Data.Functor
import Data.List (elemIndex)
import Data.Map
  ( Map,
    empty,
    insert,
    insertWith,
    lookup,
    mapWithKey,
    member,
    size,
  )
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import GHC.Base (Any, VecElem (Int16ElemRep), join)
import Paths_stack_project (getDataFileName)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Prelude hiding (lookup)

type PageId = Int

type Rank = Double

type PageRank = Map PageId Rank

type Edge = (PageId, PageId)

type InLinks = Map PageId [PageId]

type OutLinks = InLinks

parseLine :: (InLinks, OutLinks, PageId) -> String -> (InLinks, OutLinks, PageId)
parseLine (iEdges, oEdges, maxNode) line =
  let ws = words line
      (from, to) = (read $ head ws, read $ ws !! 1)
   in ( insertWith plusNode to [from] iEdges,
        insertWith plusNode from [to] oEdges,
        max to (max maxNode from)
      )
  where
    plusNode :: [PageId] -> [PageId] -> [PageId]
    plusNode new_node old_node =
      new_node ++ old_node

parseEdges :: (InLinks, OutLinks, PageId) -> (Int, Int) -> (InLinks, OutLinks, PageId)
parseEdges (iEdges, oEdges, maxNode) line =
  ( insertWith plusNode (snd line) [fst line] iEdges,
    insertWith plusNode (fst line) [snd line] oEdges,
    max (snd line) (max maxNode (fst line))
  )
  where
    plusNode :: [PageId] -> [PageId] -> [PageId]
    plusNode new_node old_node =
      new_node ++ old_node

newPageRank :: Int -> PageRank
newPageRank n =
  let v :: Double
      v = 1 / fromIntegral n
   in go n v empty
  where
    go :: Int -> Double -> PageRank -> PageRank
    go 0 _ pr = pr
    go n v pr =
      go (n - 1) v $ insert (n - 1) v pr

-- The goal of postProcess is to deal with the nodes that have no outbound
-- edges, in which case they should be treated like they have outbound edges
-- to every other node.
postProcess :: (InLinks, OutLinks, PageId) -> (InLinks, OutLinks)
postProcess (iEdges, oEdges, maxNode) =
  let numNodes = maxNode + 1
      newIEdges = addAllNodes (numNodes - 1) iEdges
   in loop (numNodes - 1) newIEdges oEdges
  where
    loop :: Int -> InLinks -> OutLinks -> (InLinks, OutLinks)
    loop n iEdges oEdges
      | n < 0 = (iEdges, oEdges)
      | otherwise =
        if member n oEdges
          then loop (n - 1) iEdges oEdges
          else
            let numNodes = maxNode + 1
                newOEdges = insert n (filter (/= n) [0 .. maxNode]) oEdges
                newIEdges = mapWithKey (\k v -> if k /= n then v ++ [n] else v) iEdges
             in loop (n - 1) newIEdges newOEdges

    -- This function makes sure that every node is a key in the InLinks map
    addAllNodes :: Int -> InLinks -> InLinks
    addAllNodes n iEdges
      | n < 0 = iEdges
      | otherwise =
        addAllNodes (n - 1) $ insertWith (\new old -> new ++ old) n [] iEdges

parseGraph :: String -> (InLinks, OutLinks, PageRank)
parseGraph input =
  let ls = lines input
      (iEdges, oEdges) = postProcess $ foldl parseLine (empty, empty, 0) ls
      numNodes = size iEdges
   in (iEdges, oEdges, newPageRank numNodes)

createEdges :: [Edge]
createEdges =
  let outLinks = [[2, 3], [5, 6], [0, 1]] :: [[Int]]
   in -- headd line = read (head $ words line) :: Int
      -- taill line = f $ tail line
      edgeList outLinks

edgeList outLinks = join (zipWith (\as i -> as <&> (,) i) outLinks [0 ..])

loopProcess :: Int -> Double -> InLinks -> OutLinks -> PageRank -> PageRank
loopProcess 0 _ _ _ pageRank = pageRank
loopProcess n dampingFactor iEdges oEdges pageRank =
  let newPageRank = loop' (size pageRank - 1) empty
   in loopProcess (n - 1) dampingFactor iEdges oEdges newPageRank
  where
    loop' :: Int -> PageRank -> PageRank
    loop' n pr
      | n < 0 = pr
      | otherwise =
        let inbounds = fromJust $ lookup n iEdges
            newPrValue =
              ((1 - dampingFactor) / fromIntegral (size iEdges))
                + (dampingFactor * foldl calc 0 inbounds)
         in loop' (n - 1) $ insert n newPrValue pr
      where
        calc acc node =
          let outbounds = fromJust $ lookup node oEdges
              prValue = fromJust $ lookup node pageRank
           in acc + prValue / fromIntegral (length outbounds)

process :: String -> Int -> Double -> PageRank
process input numIters dampingFactor =
  let (iEdges, oEdges, pageRank) = parseGraph input
   in loopProcess numIters dampingFactor iEdges oEdges pageRank

-- vkladam iEdges, OEdges, pageRank ako do loopProcess, ktore sme
-- dostali z funkcie parseGraph