module Main (main) where

import Data.List (groupBy, nub, sortBy)

type Pos = (Int, Int)

getVec :: Pos -> Pos -> (Int, Int)
getVec (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

addVec :: (Int, Int) -> Pos -> Pos
addVec (dx, dy) (x, y) = (dx + x, dy + y)

negVec :: (Int, Int) -> (Int, Int)
negVec (dx, dy) = (-dx, -dy)

inBounds :: (Int, Int) -> Pos -> Bool
inBounds (bx, by) (x, y) = 0 <= x && x < bx && 0 <= y && y < by

type Antenna = (Pos, Char)

type Input = ([Antenna], (Int, Int))

parseInput :: String -> Input
parseInput input = (antennas, (nRows, nCols))
  where
    antennas =
      filter ((/= '.') . snd) $
        zipWith (\i c -> (toPos i, c)) [0 ..] (concat lines')

    toPos = (`divMod` nCols)

    lines' = lines input
    nRows = length lines'
    nCols = length $ head lines'

groupByFrequency :: [Antenna] -> [[Antenna]]
groupByFrequency =
  groupBy (\a b -> snd a == snd b) . sortBy (\a b -> compare (snd a) (snd b))

antinodesOnEitherSide :: (Int, Int) -> Pos -> Pos -> [Pos]
antinodesOnEitherSide bounds p1 p2 =
  let v = getVec p1 p2
   in filter (inBounds bounds) [addVec v p2, addVec (negVec v) p1]

antinodesLine :: (Int, Int) -> Pos -> Pos -> [Pos]
antinodesLine bounds p1 p2 =
  let v = getVec p1 p2
      antis1 = takeWhile (inBounds bounds) (iterate (addVec v) p2)
      antis2 = takeWhile (inBounds bounds) (iterate (addVec $ negVec v) p1)
   in antis1 ++ antis2

allAntinodes :: (Pos -> Pos -> [Pos]) -> [Antenna] -> [Pos]
allAntinodes antinodesF =
  concatMap
    ( \antennas ->
        [ antinode
          | (a, _) <- antennas,
            (b, _) <- antennas,
            a /= b,
            antinode <- antinodesF a b
        ]
    )
    . groupByFrequency

part1 :: Input -> Int
part1 (antennas, bounds) =
  length $ nub $ allAntinodes (antinodesOnEitherSide bounds) antennas

part2 :: Input -> Int
part2 (antennas, bounds) =
  length $ nub $ allAntinodes (antinodesLine bounds) antennas

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
