module Day11
  ( solve,
  )
where

import Common (debug)
import Data.Array (Array, array, assocs, bounds, elems, listArray, (!))
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Set (Set, empty, insert, member)
import Debug.Trace (trace)

type Index = (Int, Int)

type EnergyMap = Array Index Int

parseInput :: String -> EnergyMap
parseInput input = listArray ((0, 0), (maxX, maxY)) $ map digitToInt $ concat l
  where
    l = lines input
    maxX = length l - 1
    maxY = length (head l) - 1

getNeighbours :: (Index, Index) -> Index -> [Index]
getNeighbours bounds idx
  | x == minX && y == minY = [(x + 1, y), (x, y + 1), (x + 1, y + 1)]
  | x == maxX && y == maxY = [(x - 1, y), (x, y - 1), (x - 1, y - 1)]
  | x == minX && y == maxY = [(x + 1, y), (x, y - 1), (x + 1, y - 1)]
  | x == maxX && y == minY = [(x - 1, y), (x, y + 1), (x - 1, y + 1)]
  | x == minX = [(x + 1, y), (x + 1, y - 1), (x + 1, y + 1), (x, y - 1), (x, y + 1)]
  | y == minY = [(x - 1, y), (x + 1, y), (x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
  | x == maxX = [(x - 1, y), (x - 1, y - 1), (x - 1, y + 1), (x, y - 1), (x, y + 1)]
  | y == maxY = [(x - 1, y), (x + 1, y), (x, y - 1), (x - 1, y - 1), (x + 1, y - 1)]
  | otherwise = [(x - 1, y), (x - 1, y - 1), (x - 1, y + 1), (x, y - 1), (x, y + 1), (x + 1, y), (x + 1, y - 1), (x + 1, y + 1)]
  where
    (x, y) = idx
    ((minX, minY), (maxX, maxY)) = bounds

toGrid emap = concat $ zipWith (\idx ele -> if idx `mod` 10 == 0 then show ele ++ "\n" else show ele ++ "\t") [1 ..] $ elems emap

flashStep :: (EnergyMap, Int) -> Bool -> (EnergyMap, Int)
flashStep inp shouldIncrement =
  if shouldFlash inc
    then flashStep (flash inc, curFlashCount + flashCount inc) False
    else (reset inc, curFlashCount)
  where
    emap = fst inp
    curFlashCount = snd inp
    inc = if shouldIncrement then listArray (bounds emap) (map (+ 1) (elems emap)) else emap
    flashCount emap = length $ filter (> 9) (elems emap)
    shouldFlash emap = any (> 9) (elems emap)
    flash emap =
      let b = bounds emap
          a = assocs emap
          toInc = foldr (\ele acc -> if snd ele > 9 then acc ++ getNeighbours b (fst ele) else acc) [] a
          count ele = length $ filter (== ele) toInc
       in array b [(i, if e < 0 then e else if e > 9 then -1 else e + count i) | (i, e) <- a]
    reset emap = listArray (bounds emap) (map (\ele -> if ele < 0 then 0 else ele) (elems emap))

part1 :: EnergyMap -> Int
part1 emap = snd $ foldr (\ele acc -> flashStep acc True) (emap, 0) [1 .. 100]

part2 :: EnergyMap -> Int
part2 emap = length $ takeWhile (\(item, _) -> not (all (== 0) (elems item))) $ scanl (\acc ele -> flashStep acc True) (emap, 0) [1 ..]

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
