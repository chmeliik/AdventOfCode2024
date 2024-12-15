module Main (main) where

import Data.List (find, foldl')
import Data.Map qualified as M
import Data.Maybe (fromJust, isNothing)

type Vec = (Int, Int)

type Pos = Vec

type Direction = Vec

move :: Direction -> Pos -> Pos
move (dx, dy) (x, y) = (x + dx, y + dy)

line :: Direction -> Pos -> [Pos]
line d = iterate (move d)

data Object = Box | Wall deriving (Eq)

data Warehouse = Warehouse {wRobot :: Pos, wObjects :: M.Map Pos Object}

moveRobot :: Direction -> Warehouse -> Warehouse
moveRobot d w@(Warehouse robot objects) = case M.lookup robot' objects of
  Nothing -> w {wRobot = robot'}
  Just Wall -> w
  Just Box -> case findEmptyPos (line d robot') objects of
    Nothing -> w
    Just emptyPos ->
      w
        { wRobot = robot',
          wObjects = (M.insert emptyPos Box . M.delete robot') objects
        }
  where
    robot' = move d robot

    -- the first non-box position has to be an empty space
    findEmptyPos :: [Pos] -> M.Map Pos Object -> Maybe Pos
    findEmptyPos line objects =
      let pos = fromJust $ find ((/= Just Box) . flip M.lookup objects) line
       in if isNothing (M.lookup pos objects) then Just pos else Nothing

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
