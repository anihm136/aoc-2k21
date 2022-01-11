module Day10
  ( solve,
  )
where

import Data.List (sort)

parseInput :: String -> [String]
parseInput = lines

getMatchingParen :: Char -> Char
getMatchingParen open
  | open == '(' = ')'
  | open == '[' = ']'
  | open == '{' = '}'
  | open == '<' = '>'
  | otherwise = error "Invalid paren"

isMatching :: Char -> Char -> Bool
isMatching open close = close == getMatchingParen open

getWrongCharScore :: Char -> Int
getWrongCharScore c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> error "Not a wrong character"

getLineMistakeScore :: String -> Int
getLineMistakeScore = aux []
  where
    opens = "([{<"
    aux [] (x : xs) =
      if x `elem` opens
        then aux [x] xs
        else getWrongCharScore x
    aux (t : ts) (x : xs)
      | x `elem` opens = aux (x : t : ts) xs
      | isMatching t x = aux ts xs
      | otherwise = getWrongCharScore x
    aux _ [] = 0

getAutocompleteCharScore :: Char -> Int
getAutocompleteCharScore c = case c of
  ')' -> 1
  ']' -> 2
  '}' -> 3
  '>' -> 4
  _ -> error "Not an autocompletable character"

getAutocompleteStringScore :: String -> Int
getAutocompleteStringScore = foldl (\acc ele -> acc * 5 + getAutocompleteCharScore ele) 0

getAutocompleteString :: String -> String
getAutocompleteString = map getMatchingParen

getLineAutocompleteScore :: String -> Int
getLineAutocompleteScore = aux []
  where
    aux [] (x : xs) = aux [x] xs
    aux (t : ts) (x : xs)
      | isMatching t x = aux ts xs
      | otherwise = aux (x : t : ts) xs
    aux stack [] = (getAutocompleteStringScore . getAutocompleteString) stack

part1 :: [String] -> Int
part1 = sum . map getLineMistakeScore

part2 :: [String] -> Int
part2 input = (sort . map getLineAutocompleteScore) filtered !! (length filtered `div` 2)
  where
    filtered = filter (\ele -> getLineMistakeScore ele == 0) input

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
