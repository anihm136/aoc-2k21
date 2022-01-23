module Day13
  ( solve,
  )
where

import Common (debug)
import Data.List.Split (splitOn)
import qualified Data.Set as S (Set, findMax, foldl, fromList, map, size)

type Point = (Int, Int)

type Points = S.Set Point

type Fold = (Char, Int)

type Folds = [Fold]

data Input = Input Points Folds deriving (Show)

parseInput :: String -> Input
parseInput input = Input points folds
  where
    (pointLines, foldLines) = let parts = splitOn [""] (lines input) in (head parts, last parts)
    points = S.fromList $ map (\ele -> let parts = map read (splitOn "," ele) in (head parts, last parts)) pointLines
    folds = map (\ele -> let parts = splitOn "=" (last (splitOn " " ele)) in (head (head parts), read (last parts))) foldLines

makeFolds :: Int -> Input -> Points
makeFolds numFolds (Input points folds) = foldl applyFold points $ take numFolds folds
  where
    maxX points = S.findMax $ S.map fst points
    maxY points = S.findMax $ S.map snd points
    applyFold points (t, idx)
      | t == 'y' && idx >= my `div` 2 = S.map (\ele -> let y = snd ele in (fst ele, if y > idx then 2 * idx - y else y)) points 
      | t == 'y' && idx < my `div` 2 = S.map (\ele -> let y = snd ele in (fst ele, if y < idx then my - 2*idx + y else my - y)) points
      | t == 'x' && idx >= mx `div` 2 = S.map (\ele -> let x = fst ele in (if x > idx then 2 * idx - x else x, snd ele)) points 
      | t == 'x' && idx < mx `div` 2 = S.map (\ele -> let x = fst ele in (if x < idx then mx - 2*idx + x else mx - x, snd ele)) points
      | otherwise = error "Invalid fold type"
      where
        (mx, my) = (maxX points, maxY points)

drawPoints :: Points -> String
drawPoints p = foldl (\acc ele -> acc ++ ele ++ "\n") "" $ S.foldl (flip placeAtPoint) canvas p
  where
    maxX = S.findMax $ S.map fst p
    maxY = S.findMax $ S.map snd p
    canvas = replicate (maxY+1) $ replicate (maxX+1) '.'
    placeAtPoint (x, y) c = take y c ++ [let r = c !! y in take x r ++ "O" ++ drop (x + 1) r] ++ drop (y + 1) c

part1 :: Input -> Int
part1 = S.size . makeFolds 1

part2 :: Input -> Int
part2 i@(Input points folds) = let points = makeFolds (length folds) i in debug (drawPoints points) $ S.size points

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
