module Day12
  ( solve,
  )
where

import Data.Char (isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Link = (String, String)

type Links = [Link]

type Graph = M.Map String [String]

parseInput :: String -> Links
parseInput = map makePair . lines
  where
    makePair line = let parts = splitOn "-" line in (head parts, last parts)

makeGraph :: Links -> Graph
makeGraph = foldr (\ele acc -> M.insertWith (++) (snd ele) [fst ele] (M.insertWith (++) (fst ele) [snd ele] acc)) M.empty

findPaths :: Int -> Int -> Graph -> Int
findPaths maxTimesSeeNode maxNodesSeeMultiple g = let (g', seen') = goingTo g "start" M.empty in search g' "start" seen'
  where
    goingTo g node seen
      | all isUpper node = (g, seen)
      | node == "start" = (filteredGraph, addedSeen)
      | maxNodesAtMax && nodeGoingPastMax = (deadEndGraph, addedSeen)
      | nodeGoingPastMax = (filteredGraph, addedSeen)
      | maxNodesAtMax = (filteredGraph, addedSeen)
      | otherwise = (g, addedSeen)
      where
        filteredGraph = M.map (filter (/= node)) g
        deadEndGraph = M.insert node [] g
        addedSeen = M.insertWith (\new old -> old + 1) node 1 seen
        nodeSeenCount = M.findWithDefault 0 node seen
        nodePastMax = nodeSeenCount > maxTimesSeeNode
        nodeGoingPastMax = nodeSeenCount + 1 == maxTimesSeeNode
        maxNodesAtMax = M.foldl (\acc ele -> acc + if ele == maxTimesSeeNode then 1 else 0) 0 seen == maxNodesSeeMultiple
    search g node seen
      | node == "end" = 1
      | null (g M.! node) = 0
      | otherwise =
        sum $
          let next = (g M.! node)
           in map
                ( \ele ->
                    let (g', seen') = goingTo g ele seen
                     in search g' ele seen'
                )
                next

part1 :: Links -> Int
part1 = findPaths 1 0 . makeGraph

part2 :: Links -> Int
part2 = findPaths 2 1 . makeGraph

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
