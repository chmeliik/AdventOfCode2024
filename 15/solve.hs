module Main (main) where

import Data.Functor ((<&>))
import Data.List (find, foldl')
import Data.Map qualified as M
import Data.Maybe (fromJust, isNothing)

type Pos = (Int, Int)

data Direction = U | D | L | R

move :: Direction -> Pos -> Pos
move d (x, y) = case d of
  U -> (x - 1, y)
  D -> (x + 1, y)
  L -> (x, y - 1)
  R -> (x, y + 1)

data Object = BoxL | BoxR | Wall deriving (Eq)

data Warehouse = Warehouse {wRobot :: Pos, wObjects :: M.Map Pos Object}

moveThing :: Pos -> Direction -> M.Map Pos Object -> Maybe (M.Map Pos Object)
moveThing pos d objects = case M.lookup pos objects of
  Nothing -> Just objects
  Just Wall -> Nothing
  Just BoxL -> moveBoxes pos BoxL (move R pos) BoxR
  Just BoxR -> moveBoxes pos BoxR (move L pos) BoxL
  where
    moveBoxes pos box adjacentPos adjacentBox = do
      objects' <- moveObj box pos objects
      case M.lookup adjacentPos objects' of
        Just o | o == adjacentBox -> moveObj adjacentBox adjacentPos objects'
        _ -> pure objects'

    moveObj obj pos objects = do
      let pos' = move d pos
      moveThing pos' d objects <&> (M.insert pos' obj . M.delete pos)

moveRobot :: Direction -> Warehouse -> Warehouse
moveRobot d w@(Warehouse robot objects) = case moveThing robot' d objects of
  Nothing -> w
  Just objects' -> w {wRobot = robot', wObjects = objects'}
  where
    robot' = move d robot

coordinatesValue :: Pos -> Int
coordinatesValue (x, y) = 100 * x + y

type Input = (Warehouse, [Direction])

parseInput :: (String -> String) -> String -> Input
parseInput modWarehouseLine input =
  ( foldl' updateWarehouse emptyWarehouse warehouseData,
    map parseDirection directionsData
  )
  where
    (warehouseLines, directionsLines) = break null $ lines input
    directionsData = concat directionsLines
    warehouseData =
      [ ((x, y), c)
        | (x, line) <- zip [0 ..] warehouseLines,
          (y, c) <- zip [0 ..] (modWarehouseLine line)
      ]
    emptyWarehouse = Warehouse (-1, -1) M.empty

    updateWarehouse :: Warehouse -> (Pos, Char) -> Warehouse
    updateWarehouse w (p, c) = case c of
      '@' -> w {wRobot = p}
      '#' -> w {wObjects = M.insert p Wall (wObjects w)}
      'O' -> w {wObjects = M.insert p BoxL (wObjects w)}
      '[' -> w {wObjects = M.insert p BoxL (wObjects w)}
      ']' -> w {wObjects = M.insert p BoxR (wObjects w)}
      _ -> w

    parseDirection :: Char -> Direction
    parseDirection '^' = U
    parseDirection 'v' = D
    parseDirection '<' = L
    parseDirection '>' = R

processInput :: (String -> String) -> String -> Int
processInput modWarehouseLine input =
  let (w, ds) = parseInput modWarehouseLine input
      (Warehouse _ objects') = foldl' (flip moveRobot) w ds
   in sum $ M.mapWithKey (\k v -> if v == BoxL then coordinatesValue k else 0) objects'

part1 :: String -> Int
part1 = processInput id

part2 :: String -> Int
part2 = processInput (concatMap modify)
  where
    modify '#' = "##"
    modify 'O' = "[]"
    modify '.' = ".."
    modify '@' = "@."

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ part1 input
  print $ part2 input
