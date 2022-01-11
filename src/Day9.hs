module Day9
  ( solve,
  )
where

import Data.Array (Array, assocs, bounds, listArray, (!))
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set (Set, empty, insert, member)

type Index = (Int, Int)

type HeightMap = Array Index Int

parseInput :: String -> HeightMap
parseInput input = listArray ((0, 0), (maxX, maxY)) $ map digitToInt $ concat l
  where
    l = lines input
    maxX = length l - 1
    maxY = length (head l) - 1

getNeighbours :: (Index, Index) -> Index -> [Index]
getNeighbours bounds idx
  | x == minX && y == minY = [(x + 1, y), (x, y + 1)]
  | x == maxX && y == maxY = [(x - 1, y), (x, y - 1)]
  | x == minX && y == maxY = [(x + 1, y), (x, y - 1)]
  | x == maxX && y == minY = [(x - 1, y), (x, y + 1)]
  | x == minX = [(x + 1, y), (x, y -1), (x, y + 1)]
  | y == minY = [(x -1, y), (x + 1, y), (x, y + 1)]
  | x == maxX = [(x - 1, y), (x, y -1), (x, y + 1)]
  | y == maxY = [(x -1, y), (x + 1, y), (x, y - 1)]
  | otherwise = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]
  where
    (x, y) = idx
    ((minX, minY), (maxX, maxY)) = bounds

getSinks :: HeightMap -> [Index]
getSinks hmap = map fst $ filter (\ele -> all ((\neigh -> neigh > snd ele) . (hmap !)) (getNeighbours (bounds hmap) (fst ele))) (assocs hmap)

expandSink :: HeightMap -> Index -> Set Index
expandSink hmap = aux empty
  where
    aux seen idx
      | hmap ! idx == 9 = seen
      | member idx seen = seen
      | otherwise =
        let seen' = insert idx seen
         in foldl aux seen' $ getNeighbours (bounds hmap) idx

part1 :: HeightMap -> Int
part1 hmap = (sum . map ((+ 1) . (hmap !)) . getSinks) hmap

part2 :: HeightMap -> Int
part2 hmap = (product . take 3 . reverse . sort . map (length . expandSink hmap) . getSinks) hmap

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
