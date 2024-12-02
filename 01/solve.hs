module Main (main) where

import Control.Arrow ((&&&))
import Control.Monad (mapAndUnzipM)
import Data.IntMap qualified as IM (IntMap, fromList, lookup)
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import GHC.Base (failIO)
import Text.Read (readEither, readMaybe)

parseInput :: String -> Either String ([Int], [Int])
parseInput =
  mapAndUnzipM parse . lines
  where
    parse :: String -> Either String (Int, Int)
    parse s = case words s of
      [a, b] -> (,) <$> parseInt a <*> parseInt b
      _ -> Left $ "invalid line: " ++ s

    parseInt s = case readMaybe s of
      Nothing -> Left $ "not an integer: " ++ s
      Just n -> Right n

part1 :: ([Int], [Int]) -> Int
part1 (as, bs) = sum $ map abs $ zipWith (-) (sort as) (sort bs)

part2 :: ([Int], [Int]) -> Int
part2 (as, bs) = sum $ map ((*) <*> getcount) as
  where
    getcount :: Int -> Int
    getcount n = fromMaybe 0 $ IM.lookup n counts

    counts :: IM.IntMap Int
    counts = IM.fromList $ map (head &&& length) $ group $ sort bs

main :: IO ()
main = do
  inputOrError <- parseInput <$> readFile "input.txt"
  case inputOrError of
    Left e -> fail $ "input.txt is invalid: " ++ e
    Right input -> do
      print $ part1 input
      print $ part2 input
