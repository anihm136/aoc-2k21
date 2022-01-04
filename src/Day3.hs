module Day3
  ( solve,
  )
where

import Common (charToInt)

parseInput :: String -> [[Int]]
parseInput x = [map charToInt i | i <- lines x]

sumLists :: [[Int]] -> [Int]
sumLists x = foldr (zipWith (+)) (replicate (length (head x)) 0) x

getMostFreq :: [[Int]] -> [Int]
getMostFreq x =
  [ if i > j then 1 else 0
    | let j = length x `quot` 2,
      i <- sumLists x
  ]

getLeastFreq :: [[Int]] -> [Int]
getLeastFreq = flipBits . getMostFreq

flipBits :: [Int] -> [Int]
flipBits = map (\ele -> if ele == 1 then 0 else 1)

binToInt :: [Int] -> Int
binToInt = foldl (\acc ele -> acc * 2 + ele) 0

filterByBit :: [[Int]] -> Int -> Int -> [[Int]]
filterByBit x pos bit = [i | i <- x, i !! pos == bit]

getBitVal :: [[Int]] -> Int -> Int -> Int
getBitVal x pos defaultBit
  | defaultBit == 1 =
    let s = sumLists x
        g = getMostFreq x
     in if s !! pos * 2 == length x then defaultBit else g !! pos
  | defaultBit == 0 =
    let s = sumLists x
        g = getLeastFreq x
     in if s !! pos * 2 == length x then defaultBit else g !! pos
  | otherwise = error "invalid bit value"

filterAll :: [[Int]] -> Int -> Int -> [Int]
filterAll x pos defaultBit
  | length x == 1 = head x
  | otherwise = filterAll (filterByBit x pos (getBitVal x pos defaultBit)) (pos + 1) defaultBit

getO2Rating :: [[Int]] -> [Int]
getO2Rating x = filterAll x 0 1

getCO2Rating :: [[Int]] -> [Int]
getCO2Rating x = filterAll x 0 0

part1 :: [[Int]] -> Int
part1 x = binToInt m * binToInt n
  where
    m = getMostFreq x
    n = flipBits m

part2 :: [[Int]] -> Int
part2 x = binToInt m * binToInt n
  where
    m = getO2Rating x
    n = getCO2Rating x

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
