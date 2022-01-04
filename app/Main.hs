module Main where

import Common (formatOutput, getInputFilename)
import qualified Day1 (solve)
import qualified Day2 (solve)
import qualified Day3 (solve)
import System.Environment (getArgs)

main :: IO ()
main = do
  let solutions = [(1, Day1.solve), (2, Day2.solve), (3, Day3.solve)]
  args <- getArgs
  let solutionsToRun = if null args then solutions else [solutions !! (i -1) | i <- [1 .. 25], show i `elem` args]
  mapM_
    ( \elem -> do
        let inputFilename = getInputFilename $ fst elem
        input <- readFile inputFilename
        let sol = snd elem input
        formatOutput (fst elem, fst sol, snd sol)
    )
    solutionsToRun
