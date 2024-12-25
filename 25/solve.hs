module Main (main) where

import Data.List (groupBy, partition, transpose)

type Schematic = [Int]

parseInput :: String -> ([Schematic], [Schematic])
parseInput input = (map parseLock locks, map parseKey keys)
  where
    schematics =
      filter (not . any null) $ groupBy (\a b -> null a == null b) $ lines input

    (locks, keys) = partition (all (== '#') . head) schematics

    parseLock = map (length . dropWhile (== '#')) . transpose
    parseKey = map (length . dropWhile (== '.')) . transpose

part1 :: ([Schematic], [Schematic]) -> Int
part1 (locks, keys) = sum [1 | lock <- locks, key <- keys, and $ zipWith (<=) key lock]

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
