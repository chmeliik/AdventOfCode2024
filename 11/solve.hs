module Main (main) where

import Control.Monad.State
import Data.Map qualified as M

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

type Cached a = State (M.Map (Int, Int) Int) a

lengthAfterNSteps :: Int -> Int -> Cached Int
lengthAfterNSteps 0 _ = pure 1
lengthAfterNSteps steps num = do
  cache <- get
  case M.lookup (steps, num) cache of
    Just len -> pure len
    Nothing -> do
      res <- sum <$> mapM (lengthAfterNSteps (steps - 1)) (rule num)
      modify (M.insert (steps, num) res)
      pure res

evalLength :: Int -> [Int] -> Int
evalLength steps nums =
  flip evalState M.empty $ sum <$> mapM (lengthAfterNSteps steps) nums

parseInput :: String -> [Int]
parseInput = map read . words

part1 :: [Int] -> Int
part1 = evalLength 25

part2 :: [Int] -> Int
part2 = evalLength 75

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
