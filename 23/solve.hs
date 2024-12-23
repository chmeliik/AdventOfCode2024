module Main (main) where

import Data.List (foldl', intercalate, maximumBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
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

maximalCliques :: Graph -> [S.Set Node]
maximalCliques g = bronkerbosch S.empty (S.fromList $ allNodes g) S.empty
  where
    -- https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm
    bronkerbosch :: S.Set Node -> S.Set Node -> S.Set Node -> [S.Set Node]
    bronkerbosch clique candidates nonCandidates =
      if S.null candidates && S.null nonCandidates
        then clique : cliques
        else cliques
      where
        (cliques, _, _) =
          foldl' step ([], candidates, nonCandidates) (S.elems candidates)

        step (cliques, candidates, nonCandidates) node =
          let neighbors = fromMaybe S.empty $ M.lookup node g
           in ( cliques
                  ++ bronkerbosch
                    (S.insert node clique)
                    (S.intersection candidates neighbors)
                    (S.intersection nonCandidates neighbors),
                S.delete node candidates,
                S.insert node nonCandidates
              )

parseInput :: String -> Graph
parseInput = foldl' addEdge M.empty . map parseEdge . lines
  where
    parseEdge s = let (a, b) = break (== '-') s in (a, tail b)

    addEdge g (a, b) =
      M.insertWith S.union a (S.singleton b) $
        M.insertWith S.union b (S.singleton a) g

part1 :: Graph -> Int
part1 = length . filter (any ((== 't') . head)) . len3cliques

part2 :: Graph -> String
part2 =
  intercalate ","
    . S.elems
    . maximumBy (\a b -> compare (S.size a) (S.size b))
    . maximalCliques

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  putStrLn $ part2 input
