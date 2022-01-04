module Day1
  ( solve,
  )
where

compareValues :: [Int] -> Int
compareValues (x : y : xs) = if y > x then 1 + compareValues (y : xs) else compareValues (y : xs)
compareValues (x : xs) = 0
compareValues [] = 0

compareWindows :: [Int] -> Int
compareWindows (w : x : y : z : xs) = if x + y + z > w + x + y then 1 + compareWindows (x : y : z : xs) else compareWindows (x : y : z : xs)
compareWindows (x : xs) = 0
compareWindows [] = 0

part1 :: String -> Int
part1 = compareValues . map read . lines

part2 :: String -> Int
part2 = compareWindows . map read . lines

solve :: String -> (Int, Int)
solve input = (part1 input, part2 input)
