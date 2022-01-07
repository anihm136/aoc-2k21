module Day6
  ( solve,
  )
where

import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

getNumFish :: Int -> [Int] -> Int
-- Models the fish themselves, inefficient
-- getNumFish days init = aux 0 days init
--   where
--     aux curDays days curFish
--       | days - curDays < minLife + 1 = length curFish
--       | otherwise =
--         aux
--           (curDays + minLife + 1)
--           days
--           $ map
--             ( \ele ->
--                 if ele == minLife
--                   then 6
--                   else ele - (minLife + 1)
--             )
--             curFish
--             ++ replicate (length (filter (== minLife) curFish)) 8
--       where
--         minLife = minimum curFish

getInitialCounts :: [Int] -> [Int]
getInitialCounts init = [count num init | num <- [0 .. 8]]
  where
    count num list = length $ filter (== num) list

getNumFish days init = sum $ modelGenerations days $ getInitialCounts init
  where
    rotate (x : xs) = xs ++ [x]
    rotate [] = []
    modelDay generation = let r = rotate generation in take 6 r ++ [r !! 8 + r !! 6] ++ drop 7 r
    modelGenerations days curGeneration
      | days == 0 = curGeneration
      | otherwise = modelGenerations (days - 1) $ modelDay curGeneration

part1 :: [Int] -> Int
part1 = getNumFish 80

part2 :: [Int] -> Int
part2 = getNumFish 256

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
