module Main (main) where

import Data.Array.IArray (Array, Ix (inRange, range), bounds, indices, listArray, (!))
import Data.Bool (bool)
import Data.Maybe (catMaybes)

type Input = Array (Int, Int) Char

parseInput :: String -> Input
parseInput input = listArray ((1, 1), (maxRow, maxCol)) $ concat lines'
  where
    lines' = lines input
    maxRow = length lines'
    maxCol = length (head lines')

type Direction = (Int, Int)

move :: Direction -> (Int, Int) -> (Int, Int)
move (dx, dy) (x, y) = (x + dx, y + dy)

neg :: Direction -> Direction
neg (dx, dy) = (-dx, -dy)

octaDirectionalXMASSearch :: Input -> (Int, Int) -> Int
octaDirectionalXMASSearch arr pos =
  sum $
    [ bool 0 1 (xmasInDirection (dx, dy))
      | dx <- [-1 .. 1],
        dy <- [-1 .. 1],
        (dx, dy) /= (0, 0)
    ]
  where
    xmasInDirection :: (Int, Int) -> Bool
    xmasInDirection (dx, dy) = s == "XMAS"
      where
        s = map (arr !) ixs
        ixs = takeWhile inBounds $ take 4 $ iterate (move (dx, dy)) pos
        inBounds = inRange (bounds arr)

crossMASSearch :: Input -> (Int, Int) -> Bool
crossMASSearch arr pos = arr ! pos == 'A' && isMS (1, 1) && isMS (1, -1)
  where
    isMS d = [arr ! move d pos, arr ! move (neg d) pos] `elem` ["MS", "SM"]

part1 :: Input -> Int
part1 arr = sum $ map (octaDirectionalXMASSearch arr) (indices arr)

part2 :: Input -> Int
part2 arr = sum $ map (bool 0 1 . crossMASSearch arr) ixs
  where
    ixs = range ((minX + 1, minY + 1), (maxX - 1, maxY - 1))
    ((minX, minY), (maxX, maxY)) = bounds arr

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
