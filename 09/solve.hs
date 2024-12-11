module Main (main) where

import Data.Char (digitToInt)
import Data.List (foldl', partition, scanl')
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)

type Inode = Int

type Input = [(Int, Maybe Inode)]

parseInput :: String -> Input
parseInput =
  zipWith
    (\i c -> (digitToInt c, if even i then Just (i `div` 2) else Nothing))
    [0 ..]
    . takeWhile (/= '\n')

type FlatFS = [Maybe Inode]

flatten :: Input -> FlatFS
flatten = concatMap (replicate <$> fst <*> snd)

fillEmptySpaces :: FlatFS -> FlatFS
fillEmptySpaces fs = take (length inodes) $ moveToFront fs filesToMove
  where
    filesToMove = take (length spaces) $ reverse $ map fromJust inodes
    (inodes, spaces) = partition isJust fs

    moveToFront :: FlatFS -> [Inode] -> FlatFS
    moveToFront (Nothing : fs') (inode : inodes) = Just inode : moveToFront fs' inodes
    moveToFront (f : fs') inodes = f : moveToFront fs' inodes
    moveToFront [] inodes = map Just inodes

checksum :: FlatFS -> Int
checksum = sum . zipWith (\i f -> i * fromMaybe 0 f) [0 ..]

data Block = Block {ixStart :: Int, size :: Int, inode :: Maybe Inode}

toBlocks :: Input -> [Block]
toBlocks = tail . scanl' f (Block 0 0 Nothing)
  where
    f :: Block -> (Int, Maybe Inode) -> Block
    f (Block prevStart prevSize _) (size, inode) = Block (prevStart + prevSize) size inode

flattenBlocks :: [Block] -> FlatFS
flattenBlocks = concatMap (replicate <$> size <*> inode)

moveBlocksToEmptySpace :: [Block] -> [Block]
moveBlocksToEmptySpace blocks =
  foldl' moveBlockToEmptySpace blocks $ reverse (filter (isJust . inode) blocks)

moveBlockToEmptySpace :: [Block] -> Block -> [Block]
moveBlockToEmptySpace [] _ = []
moveBlockToEmptySpace (b@(Block i1 s1 ino1) : bs) b2@(Block i2 s2 ino2)
  | i1 >= i2 = b : bs
  | isNothing ino1 && s1 >= s2 =
      Block i1 s2 ino2
        : Block (i1 + s2) (s1 - s2) Nothing
        : map (\b -> if ixStart b == i2 then b {inode = Nothing} else b) bs
  | otherwise = b : moveBlockToEmptySpace bs b2

part1 :: Input -> Int
part1 = checksum . fillEmptySpaces . flatten

part2 :: Input -> Int
part2 = checksum . flattenBlocks . moveBlocksToEmptySpace . toBlocks

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
