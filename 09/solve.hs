module Main (main) where

import Data.Char (digitToInt)
import Data.List (partition)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)

type Inode = Int

type Input = [(Int, Maybe Inode)]

parseInput :: String -> Input
parseInput =
  zipWith
    (\i c -> (digitToInt c, if even i then Just (i `div` 2) else Nothing))
    [0 ..]
    . takeWhile (/= '\n')

type FlatFS = [Maybe Inode]

flatten :: Input -> FlatFS
flatten = concatMap (replicate <$> fst <*> snd)

fillEmptySpaces :: FlatFS -> FlatFS
fillEmptySpaces fs = take (length inodes) $ moveToFront fs filesToMove
  where
    filesToMove = take (length spaces) $ reverse $ map fromJust inodes
    (inodes, spaces) = partition isJust fs

    moveToFront :: FlatFS -> [Inode] -> FlatFS
    moveToFront (Nothing : fs') (inode : inodes) = Just inode : moveToFront fs' inodes
    moveToFront (f : fs') inodes = f : moveToFront fs' inodes
    moveToFront [] inodes = map Just inodes

checksum :: FlatFS -> Int
checksum = sum . zipWith (\i f -> i * fromMaybe 0 f) [0 ..]

part1 :: Input -> Int
part1 = checksum . fillEmptySpaces . flatten

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
