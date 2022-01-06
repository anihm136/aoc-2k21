module Day5
  ( solve,
  )
where

import Data.List.Split (splitOn)

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq)

data LineSegment = LineSegment Point Point deriving (Show)

makePoint :: String -> Point
makePoint s = Point {x = a, y = b}
  where
    parts = splitOn "," s
    a = (read . head) parts
    b = (read . head . tail) parts

makeLine :: String -> LineSegment
makeLine s = LineSegment (makePoint l) (makePoint r)
  where
    parts = words s
    l = head parts
    r = (head . tail . tail) parts

parseInput :: String -> [LineSegment]
parseInput = map makeLine . lines

filterForPart1 :: [LineSegment] -> [LineSegment]
filterForPart1 = filter (\(LineSegment Point {x = lx, y = ly} Point {x = rx, y = ry}) -> lx == rx || ly == ry)

intersect :: Point -> LineSegment -> Bool
intersect (Point x y) (LineSegment (Point a b) (Point c d))
  | a == c && x /= a = False
  | b == d && y /= b = False
  | a == c && y <= yMax && y >= yMin = True
  | b == d && x <= xMax && x >= xMin = True
  | otherwise = False
  where
    xMax = max a c
    xMin = min a c
    yMax = max b d
    yMin = min b d

intersectWithDiag :: Point -> LineSegment -> Bool
intersectWithDiag p@(Point x y) ls@(LineSegment l@(Point a b) r@(Point c d))
  | p `intersect` ls = True
  | p == l || p == r = True
  | and [y <= yMax, y >= yMin, x <= xMax, x >= xMin, abs (xMax - xMin) == abs (y2 - y1), abs (x - xMin) == abs (y - y1), sgn (y - y1) == sgn (y2 - y1)] = True
  | otherwise = False
  where
    xMax = max a c
    xMin = min a c
    yMax = max b d
    yMin = min b d
    y1 = if xMin == a then b else d
    y2 = if xMax == a then b else d
    sgn v = if v >= 0 then 1 else (-1)

checkIntersection :: (Point -> LineSegment -> Bool) -> [LineSegment] -> Point -> Int
checkIntersection pred ls p = length $ take 2 $ filter (pred p) ls

getAllPoints :: Int -> [Point]
getAllPoints gridSize = [Point {x = x, y = y} | x <- [0 .. gridSize], y <- [0 .. gridSize]]

countDangerous :: (Point -> LineSegment -> Bool) -> [LineSegment] -> Int
countDangerous pred ls = length $ filter (== 2) $ map (checkIntersection pred ls) allPoints
  where
    allPoints = getAllPoints 1000

part1 :: [LineSegment] -> Int
part1 input = countDangerous intersect filtered
  where
    filtered = filterForPart1 input

part2 :: [LineSegment] -> Int
part2 = countDangerous intersectWithDiag

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
