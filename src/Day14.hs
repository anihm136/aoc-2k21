module Day14
  ( solve,
  )
where

import Common (debug)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Debug.Trace (trace)

-- Brute force approach, does not work for part 2
-- type Insertions = M.Map String Char

-- data Input = Input String Insertions

-- parseInput :: String -> Input
-- parseInput input = Input baseString insertionMap
--   where
--     parts = splitOn "\n\n" input
--     baseString = head parts
--     insertionMap =
--       M.fromList $
--         map (\line -> let p = splitOn " -> " line in (head p, head $ last p)) $
--           lines $ last parts

-- insertionPass :: Input -> Input
-- insertionPass (Input base imap) = let (_, newBase) = doPass in Input newBase imap
--   where
--     doPass =
--       foldl
--         ( \(lastchar, acc) ele -> case M.lookup [lastchar, ele] imap of
--             Just a -> (ele, acc ++ [a, ele])
--             Nothing -> (ele, acc ++ [ele])
--         )
--         ('\0', "")
--         base

-- findCharCounts :: String -> M.Map Char Int
-- findCharCounts = foldr (\ele acc -> M.insertWith (\_ old -> old + 1) ele 1 acc) M.empty

type Insertions = M.Map String Char

type Polymer = M.Map String Int

type CharCounts = M.Map Char Int

data Input = Input Polymer Insertions CharCounts deriving (Show)

insertOrInc :: Ord a => a -> Int -> M.Map a Int -> M.Map a Int
insertOrInc k v = M.insertWith (\_ old -> old + v) k v

parseInput :: String -> Input
parseInput input = Input basePolymer insertionMap cc
  where
    parts = splitOn "\n\n" input
    baseString = head parts
    basePolymer = snd $ foldl (\(lastchar, acc) ele -> if lastchar == '\0' then (ele, acc) else (ele, insertOrInc [lastchar, ele] 1 acc)) ('\0', M.empty) baseString
    insertionMap =
      M.fromList $
        map
          ( \line ->
              let p = splitOn " -> " line
                  middleChar = head $ last p
                  firstChar = head $ head p
                  secondChar = last $ head p
               in ([firstChar, secondChar], middleChar)
          )
          $ lines $ last parts
    cc = findCharCounts baseString

insertionPass :: Input -> Input
insertionPass (Input base imap cc) = Input newBase imap newcc
  where
    newBase =
      foldl
        ( \acc (key, count) -> case M.lookup key imap of
            Just c -> foldl (\acc insertion -> insertOrInc insertion count acc) (M.adjust (flip (-) count) key acc) [[head key, c], [c, last key]]
            Nothing -> acc
        )
        base
        $ M.assocs base
    newcc =
      foldl
        ( \acc (key, count) -> case M.lookup key imap of
            Just c -> insertOrInc c count acc
            Nothing -> acc
        )
        cc
        $ M.assocs base

doNPasses :: Int -> Input -> Input
doNPasses n input = iterate insertionPass input !! n

findCharCounts :: String -> M.Map Char Int
findCharCounts = foldr (\ele acc -> M.insertWith (\_ old -> old + 1) ele 1 acc) M.empty

part1 :: Input -> Int
part1 i =
  let (Input _ _ cc) = doNPasses 10 i
      counts = M.elems cc
   in maximum counts - minimum counts

part2 :: Input -> Int
part2 i =
  let (Input _ _ cc) = doNPasses 40 i
      counts = M.elems cc
   in maximum counts - minimum counts

solve :: String -> (Int, Int)
solve input = let parsed = parseInput input in (part1 parsed, part2 parsed)
