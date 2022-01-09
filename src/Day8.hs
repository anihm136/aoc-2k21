module Day8
  ( solve,
  )
where

import Data.List (elemIndex, find, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set, difference, empty, fromList, isSubsetOf, singleton, toList)

data Input = Input [Set Char] [Set Char] deriving (Show)

parseInput :: String -> [Input]
parseInput input = map (\ele -> Input (allDigits ele) (outDigits ele)) $ lines input
  where
    parts line = splitOn "|" line
    allDigits line = map fromList $ words $ head $ parts line
    outDigits line = map fromList $ words $ head $ tail $ parts line

getDigitRepresentations :: [Set Char] -> [Set Char]
getDigitRepresentations orig =
  foldl
    (\acc ele -> replace ele (findMatch ele acc orig) acc)
    (replicate 10 empty)
    [1, 4, 7, 8, 0, 2, 3, 5, 6, 9]
  where
    replace idx replacement list = take idx list ++ [replacement] ++ drop (idx + 1) list
    findMatch ele acc orig =
      let get pred = fromJust (find pred orig)
       in case ele of
            1 -> get ((== 2) . length)
            4 -> get ((== 4) . length)
            7 -> get ((== 3) . length)
            8 -> get ((== 7) . length)
            0 ->
              get
                ( \ele ->
                    ((== 6) . length) ele
                      && ((acc !! 1) `isSubsetOf` ele)
                      && not ((acc !! 4) `isSubsetOf` ele)
                )
            2 ->
              get
                ( \ele ->
                    ((== 5) . length) ele
                      && length (difference (acc !! 4) ele) == 2
                )
            3 ->
              get
                ( \ele ->
                    ((== 5) . length) ele
                      && ((acc !! 1) `isSubsetOf` ele)
                )
            5 ->
              get
                ( \ele ->
                    ((== 5) . length) ele
                      && length (difference (acc !! 4) ele) == 1
                      && length (difference (acc !! 7) ele) == 1
                )
            6 ->
              get
                ( \ele ->
                    ((== 6) . length) ele
                      && not ((acc !! 1) `isSubsetOf` ele)
                      && not ((acc !! 4) `isSubsetOf` ele)
                )
            9 ->
              get
                ( \ele ->
                    ((== 6) . length) ele
                      && (acc !! 4) `isSubsetOf` ele
                )
            _ -> error "Invalid digit"

getSegmentMapping :: [Set Char] -> String
getSegmentMapping digitRepresentations = map (`getSegment` digitRepresentations) [0 .. 6]
  where
    get s = head $ toList s
    getSegment idx dr = case idx of
      0 -> get $ difference (dr !! 7) (dr !! 1)
      1 -> get $ difference (dr !! 9) (dr !! 3)
      2 -> get $ difference (dr !! 8) (dr !! 6)
      3 -> get $ difference (dr !! 8) (dr !! 0)
      4 -> get $ difference (dr !! 8) (dr !! 9)
      5 -> get $ difference (difference (dr !! 6) (dr !! 2)) $ singleton $ getSegment 1 dr
      6 -> get $ difference (difference (dr !! 9) (dr !! 4)) $ singleton $ getSegment 0 dr
      _ -> error "Invalid segment"

fixNumberDisplay :: String -> Set Char -> String
fixNumberDisplay fixMap broken = fixed
  where
    getIdx c = fromJust $ elemIndex c fixMap
    getChar c = case c of
      0 -> 'a'
      1 -> 'b'
      2 -> 'c'
      3 -> 'd'
      4 -> 'e'
      5 -> 'f'
      6 -> 'g'
      _ -> error "Invalid segment index"
    idxs = map getIdx $ toList broken
    fixed = sort $ map getChar idxs

getDigit :: String -> Int
getDigit display = case display of
  "abcefg" -> 0
  "cf" -> 1
  "acdeg" -> 2
  "acdfg" -> 3
  "bcdf" -> 4
  "abdfg" -> 5
  "abdefg" -> 6
  "acf" -> 7
  "abcdefg" -> 8
  "abcdfg" -> 9
  _ -> error "Invalid display sequence"

getNumber :: [Int] -> Int
getNumber = foldl (\acc ele -> acc * 10 + ele) 0

part1 :: [Input] -> Int
part1 input = sum $ map (\(Input _ y) -> length $ filter (\ele -> length ele `elem` [2, 3, 4, 7]) y) input

part2 :: [Input] -> Int
part2 input = sum fixedNums
  where
    inputDigits = map (\(Input x y) -> x) input
    outputDigits = map (\(Input x y) -> y) input
    digitRepresentations = map getDigitRepresentations inputDigits
    segmentMaps = map getSegmentMapping digitRepresentations
    fixedOutputs = zipWith (\broken fixMap -> map (fixNumberDisplay fixMap) broken) outputDigits segmentMaps
    fixedNums = map (getNumber . map getDigit) fixedOutputs

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
