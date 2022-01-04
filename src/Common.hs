module Common
  ( charToInt,
    getInputFilename,
    formatOutput,
  )
where

charToInt :: Char -> Int
charToInt c = case c of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  _ -> error "invalid character"

getInputFilename :: Int -> String
getInputFilename x = "inputs/" ++ show x ++ ".txt"

formatOutput :: (Show a, Show b) => (Int, a, b) -> IO ()
formatOutput (day, part1, part2) = do
  print $ "Day " ++ show day ++ " part 1: " ++ show part1
  print $ "Day " ++ show day ++ " part 2: " ++ show part2
