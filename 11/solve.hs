module Main (main) where

parseInput :: String -> [Int]
parseInput = map read . words

rule :: Int -> [Int]
rule 0 = [1]
rule n =
  if even nd
    then let (d, m) = divMod n (10 ^ (nd `div` 2)) in [d, m]
    else [n * 2024]
  where
    nd = nDigits n

nDigits :: Int -> Int
nDigits = length . takeWhile (> 0) . iterate (`div` 10)

step :: [Int] -> [Int]
step = concatMap rule

part1 :: [Int] -> Int
part1 = length . (!! 25) . iterate step

-- part2 :: [Int] -> Int
-- part2 = length . (!! 75) . iterate step

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input

-- print $ part2 input
