module Day4
  ( solve,
  )
where

import Data.List (elemIndex, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

newtype Board = Board [[(Int, Bool)]]

data Input = Input {inputNums :: [Int], boards :: [Board], lastNum :: Int}

getInputNums :: String -> [Int]
getInputNums x = map read $ splitOn "," x

makeBoard :: [String] -> Board
makeBoard x = Board [[(read i, False) | i <- words j] | j <- x]

getBoards :: [String] -> [Board]
getBoards x = map makeBoard g
  where
    g = splitOn [[]] x

parseInput :: String -> Input
parseInput x = Input {inputNums = n, boards = b, lastNum = 0}
  where
    l = lines x
    n = getInputNums $ head l
    b = getBoards $ tail $ tail l

markNumberOnBoard :: Int -> Board -> Board
markNumberOnBoard n (Board b) = Board [[(fst i, (fst i == n) || snd i) | i <- j] | j <- b]

checkWin :: Board -> Bool
checkWin (Board b) = any (all snd) b || any (all snd) (transpose b)

calculateBoardScore :: Board -> Int
calculateBoardScore (Board b) = sum $ map (sum . (\row -> [fst i | i <- row, not $ snd i])) b

playToWin :: Input -> Int
playToWin i
  | any checkWin (boards i) = lastNum i * calculateBoardScore (boards i !! fromJust (elemIndex True $ map checkWin $ boards i))
  | otherwise =
    playToWin $
      let currentNum = head $ inputNums i
       in Input
            { inputNums = tail $ inputNums i,
              boards = map (markNumberOnBoard currentNum) $ boards i,
              lastNum = currentNum
            }

playToLose :: Input -> Int
playToLose i
  | length (boards i) == 1 && checkWin (head $ boards i) = lastNum i * calculateBoardScore (head $ boards i)
  | null (boards i) = error "Number of boards should not be 0"
  | otherwise =
    playToLose $
      let currentNum = head $ inputNums i
       in Input
            { inputNums = tail $ inputNums i,
              boards = [b | b <- map (markNumberOnBoard currentNum) $ boards i, length (boards i) == 1 || not (checkWin b)],
              lastNum = currentNum
            }

part1 :: Input -> Int
part1 = playToWin

part2 :: Input -> Int
part2 = playToLose

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
