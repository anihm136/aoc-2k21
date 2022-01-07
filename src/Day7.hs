module Day7
  ( solve,
  )
where

import Data.List.Split (splitOn)

parseInput :: String -> [Int]
parseInput input = map read $ splitOn "," input

absError :: Int -> Int -> Int
absError a b = abs $ a - b

incError :: Int -> Int -> Int
incError a b = sum $ getInc a b
  where
    getInc a b = take (abs (a-b)) [1..]

getError :: (Int -> Int -> Int) -> [Int] -> Int -> Int
getError errfunc input ans = sum $ map (errfunc ans) input

part1 :: [Int] -> Int
part1 x = minimum $ map (getError absError x) [minimum x .. maximum x]

part2 :: [Int] -> Int
part2 x = minimum $ map (getError incError x) [minimum x .. maximum x]

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
