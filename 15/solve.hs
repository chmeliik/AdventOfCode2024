module Main (main) where

import Data.Functor ((<&>))
import Data.List (find, foldl')
import Data.Map qualified as M
import Data.Maybe (fromJust, isNothing)

type Vec = (Int, Int)

type Pos = Vec

type Direction = Vec

move :: Direction -> Pos -> Pos
move (dx, dy) (x, y) = (x + dx, y + dy)

data Object = Box | Wall deriving (Eq)

data Warehouse = Warehouse {wRobot :: Pos, wObjects :: M.Map Pos Object}

moveThing :: Pos -> Direction -> M.Map Pos Object -> Maybe (M.Map Pos Object)
moveThing pos d objects = case M.lookup pos objects of
  Nothing -> Just objects
  Just Wall -> Nothing
  Just Box -> do
    let pos' = move d pos
    moveThing pos' d objects <&> (M.insert pos' Box . M.delete pos)

moveRobot :: Direction -> Warehouse -> Warehouse
moveRobot d w@(Warehouse robot objects) = case moveThing robot' d objects of
  Nothing -> w
  Just objects' -> w {wRobot = robot', wObjects = objects'}
  where
    robot' = move d robot

coordinatesValue :: Pos -> Int
coordinatesValue (x, y) = 100 * x + y

type Input = (Warehouse, [Direction])

parseInput :: String -> Input
parseInput input =
  ( foldl' updateWarehouse emptyWarehouse warehouseData,
    map parseDirection directionsData
  )
  where
    (warehouseLines, directionsLines) = break null $ lines input
    directionsData = concat directionsLines
    warehouseData =
      [ ((x, y), c)
        | (x, line) <- zip [0 ..] warehouseLines,
          (y, c) <- zip [0 ..] line
      ]
    emptyWarehouse = Warehouse (-1, -1) M.empty

    updateWarehouse :: Warehouse -> (Pos, Char) -> Warehouse
    updateWarehouse w (p, c) = case c of
      '@' -> w {wRobot = p}
      '#' -> w {wObjects = M.insert p Wall (wObjects w)}
      'O' -> w {wObjects = M.insert p Box (wObjects w)}
      _ -> w

    parseDirection :: Char -> Direction
    parseDirection '^' = (-1, 0)
    parseDirection 'v' = (1, 0)
    parseDirection '<' = (0, -1)
    parseDirection '>' = (0, 1)

part1 :: Input -> Int
part1 (w, ds) =
  let (Warehouse _ objects') = foldl' (flip moveRobot) w ds
   in sum $ M.mapWithKey (\k v -> if v == Box then coordinatesValue k else 0) objects'

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
