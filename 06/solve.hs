module Main (main) where

import Data.Bool (bool)
import Data.List (foldl')
import Data.Maybe (fromJust)
import Data.Set qualified as S

type Pos = (Int, Int)

data Direction = N | E | S | W deriving (Eq, Ord, Enum)

turn :: Direction -> Direction
turn W = N
turn d = succ d

move :: Direction -> Pos -> Pos
move N (x, y) = (x - 1, y)
move E (x, y) = (x, y + 1)
move S (x, y) = (x + 1, y)
move W (x, y) = (x, y - 1)

-- obstacle positions, bounds
type Map = (S.Set Pos, Pos)

type GuardPos = (Direction, Pos)

parseInput :: String -> (Map, GuardPos)
parseInput input = ((obstacles, bounds), fromJust guardPos)
  where
    (obstacles, bounds, guardPos) =
      foldl' go (S.empty, (0, 0), Nothing) positionedChars

    positionedChars :: [(Pos, Char)]
    positionedChars =
      concat $
        zipWith
          (\x indexedLine -> map (\(y, c) -> ((x, y), c)) indexedLine)
          [0 ..]
          (map (zip [0 ..]) $ lines input)

    go (set, bounds, guardPos) (pos, c) = (set', max bounds pos, guardPos')
      where
        set' = (if c == '#' then S.insert pos else id) set
        guardPos' = case c of
          '^' -> Just (N, pos)
          '>' -> Just (E, pos)
          'v' -> Just (S, pos)
          '<' -> Just (W, pos)
          _ -> guardPos

mapPath :: Map -> GuardPos -> [GuardPos]
mapPath (obstacles, bounds) guardPos = takeWhile inBounds $ iterate moveGuard guardPos
  where
    inBounds (_, (x, y)) =
      let (maxX, maxY) = bounds
       in 0 <= x && x <= maxX && 0 <= y && y <= maxY

    moveGuard (d, pos) =
      let pos' = move d pos
       in if pos' `S.member` obstacles then (turn d, pos) else (d, pos')

isCycle :: Map -> GuardPos -> Bool
isCycle m g = go S.empty $ mapPath m g
  where
    go :: S.Set GuardPos -> [GuardPos] -> Bool
    go _ [] = False
    go s (g : gs)
      | g `S.member` s = True
      | otherwise = go (S.insert g s) gs

addObstacle :: Map -> Pos -> Map
addObstacle (obstacles, bounds) pos = (S.insert pos obstacles, bounds)

part1 :: (Map, GuardPos) -> Int
part1 (m, g) = S.size $ S.fromList $ map snd (mapPath m g)

part2 :: (Map, GuardPos) -> Int
part2 (m, g) = sum $ map (bool 0 1 . flip isCycle g . addObstacle m) positions
  where
    positions = S.toList $ S.fromList $ tail $ map snd $ mapPath m g

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
