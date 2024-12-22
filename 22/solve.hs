module Main (main) where

import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.List (foldl', maximumBy)
import Data.Map qualified as M

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (.&. 0xffffff)

nextSecretNumber :: Int -> Int
nextSecretNumber n = step3
  where
    step1 = prune $ mix n (shiftL n 6)
    step2 = prune $ mix step1 (shiftR step1 5)
    step3 = prune $ mix step2 (shiftL step2 11)

nthSecretNumber :: Int -> Int -> Int
nthSecretNumber n initial = iterate nextSecretNumber initial !! (n - 1)

type ChangeSequence = (Int, Int, Int, Int)

mapChangeSequencesToPrices :: [Int] -> M.Map ChangeSequence Int
mapChangeSequencesToPrices prices =
  M.fromList $ reverse $ zip (changeSequences changes) (drop 4 prices)
  where
    changes = zipWith (flip (-)) prices (tail prices)

    changeSequences :: [Int] -> [ChangeSequence]
    changeSequences xs@(w : x : y : z : _) = (w, x, y, z) : changeSequences (tail xs)
    changeSequences _ = []

first2001Prices :: Int -> [Int]
first2001Prices = take 2001 . map (`mod` 10) . iterate nextSecretNumber

mapAllChangeSequences :: [Int] -> M.Map ChangeSequence Int
mapAllChangeSequences = foldl' (M.unionWith (+)) M.empty . csmaps
  where
    csmaps = map (mapChangeSequencesToPrices . first2001Prices)

bestChangeSequence :: M.Map ChangeSequence Int -> (ChangeSequence, Int)
bestChangeSequence = maximumBy (\(_, a) (_, b) -> compare a b) . M.toList

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 = sum . map (nthSecretNumber 2001)

part2 :: [Int] -> Int
part2 = snd . bestChangeSequence . mapAllChangeSequences

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
