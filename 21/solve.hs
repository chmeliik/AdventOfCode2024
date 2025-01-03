module Main (main) where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Char (digitToInt)
import Data.Map qualified as M

type Pos = (Int, Int)

data KeypadType = NumPad | ArrowPad deriving (Eq, Ord)

illegalPos :: KeypadType -> Pos
illegalPos NumPad = (3, 0)
illegalPos ArrowPad = (0, 0)

getPos :: KeypadType -> Char -> Pos
getPos NumPad 'A' = (3, 2)
getPos NumPad '0' = (3, 1)
getPos NumPad n = let (d, m) = divMod (digitToInt n - 1) 3 in (2 - d, m)
getPos ArrowPad 'A' = (0, 2)
getPos ArrowPad '^' = (0, 1)
getPos ArrowPad '<' = (1, 0)
getPos ArrowPad 'v' = (1, 1)
getPos ArrowPad '>' = (1, 2)

goAndPress :: KeypadType -> Pos -> Pos -> [Char]
goAndPress kpt (x1, y1) (x2, y2)
  -- left and _: left first, then vertically
  -- right and down: down first, then right
  -- right and up: doesn't matter
  | dy < 0 && (x1, y1 + dy) /= illegalPos kpt = horizontally ++ vertically ++ "A"
  | (x1 + dx, y1) /= illegalPos kpt = vertically ++ horizontally ++ "A"
  | otherwise = horizontally ++ vertically ++ "A"
  where
    dx = x2 - x1
    dy = y2 - y1
    keyX = if dx > 0 then 'v' else '^'
    keyY = if dy > 0 then '>' else '<'
    vertically = replicate (abs dx) keyX
    horizontally = replicate (abs dy) keyY

indirectlyType :: KeypadType -> [Char] -> [[Char]]
indirectlyType kpt chars =
  zipWith (goAndPress kpt) (initialPos : positions) positions
  where
    initialPos = getPos kpt 'A'
    positions = map (getPos kpt) chars

type Cached i o = State (M.Map i o) o

lenAfterIndirections ::
  Int -> KeypadType -> [Char] -> Cached (Int, KeypadType, [Char]) Int
lenAfterIndirections 0 _ chars = pure $ length chars
lenAfterIndirections n kpt chars = do
  maybeCached <- gets (M.lookup (n, kpt, chars))
  case maybeCached of
    Just len -> pure len
    Nothing -> do
      len <-
        sum <$> mapM (lenAfterIndirections (n - 1) ArrowPad) (indirectlyType kpt chars)
      modify (M.insert (n, kpt, chars) len)
      pure len

complexity :: Int -> [Char] -> Int
complexity n chars =
  evalState (lenAfterIndirections n NumPad chars) M.empty * read (init chars)

part1 :: [[Char]] -> Int
part1 = sum . map (complexity 3)

part2 :: [[Char]] -> Int
part2 = sum . map (complexity 26)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
