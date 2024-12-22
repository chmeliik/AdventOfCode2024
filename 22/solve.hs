module Main (main) where

import Data.Bits (shiftL, shiftR, xor, (.&.))

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

parseInput :: String -> [Int]
parseInput = map read . lines

part1 :: [Int] -> Int
part1 = sum . map (nthSecretNumber 2001)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
