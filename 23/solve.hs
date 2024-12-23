module Main (main) where

import Data.List (foldl')
import Data.Map qualified as M
import Data.Set qualified as S

type Node = String

type Graph = M.Map Node (S.Set Node)

allNodes :: Graph -> [Node]
allNodes = M.keys

hasEdge :: Graph -> (Node, Node) -> Bool
hasEdge g (a, b) = case M.lookup a g of
  Just bs -> S.member b bs
  Nothing -> False

len3cliques :: Graph -> [[Node]]
len3cliques g =
  [ [a, b, c]
    | (a, i) <- zip nodes [0 ..],
      (b, j) <- zip nodes [0 ..],
      (c, k) <- zip nodes [0 ..],
      j > i && k > j,
      hasEdge g (a, b) && hasEdge g (b, c) && hasEdge g (c, a)
  ]
  where
    nodes = allNodes g

parseInput :: String -> Graph
parseInput = foldl' addEdge M.empty . map parseEdge . lines
  where
    parseEdge s = let (a, b) = break (== '-') s in (a, tail b)

    addEdge g (a, b) =
      M.insertWith S.union a (S.singleton b) $
        M.insertWith S.union b (S.singleton a) g

part1 :: Graph -> Int
part1 = length . filter (any ((== 't') . head)) . len3cliques

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
