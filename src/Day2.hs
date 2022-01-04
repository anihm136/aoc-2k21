module Day2
  ( solve,
  )
where

parseInput :: String -> [(String, Int)]
parseInput x = [(x, read y) | (x : y : xs) <- (map words . lines) x]

data Position = Position {distance :: Int, aim :: Int, depth :: Int}

getDistance :: [(String, Int)] -> Int
getDistance [] = 0
getDistance (x : xs) = if fst x == "forward" then snd x + getDistance xs else getDistance xs

getDepth :: [(String, Int)] -> Int
getDepth [] = 0
getDepth (x : xs)
  | fst x == "down" = snd x + getDepth xs
  | fst x == "up" = getDepth xs - snd x
  | otherwise = getDepth xs

changePosition :: Position -> (String, Int) -> Position
changePosition p x
  | fst x == "down" = Position {distance = distance p, aim = aim p + snd x, depth = depth p}
  | fst x == "up" = Position {distance = distance p, aim = aim p - snd x, depth = depth p}
  | fst x == "forward" = Position {distance = distance p + snd x, aim = aim p, depth = depth p + aim p * snd x}
  | otherwise = error "invalid command"

getFinalPosition :: [(String, Int)] -> Position
getFinalPosition = foldl changePosition Position {distance = 0, aim = 0, depth = 0}

part1 :: [(String, Int)] -> Int
part1 x = getDistance x * getDepth x

part2 :: [(String, Int)] -> Int
part2 x = depth p * distance p
  where
    p = getFinalPosition x

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
