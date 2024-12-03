module Main (main) where

import Data.Maybe (isNothing)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (low, high) n = n >= low && n <= high

isSafe :: [Int] -> Bool
isSafe report =
  all (inRange (1, 3)) differences || all (inRange (-3, -1)) differences
  where
    differences = zipWith (-) report (tail report)

isSafeWithDampener :: [Int] -> Bool
isSafeWithDampener report = areSafeWithDampener diffs
  where
    diffs = zipWith (-) report (tail report)

    areSafeWithDampener :: [Int] -> Bool
    areSafeWithDampener [] = True
    areSafeWithDampener [_] = True -- can drop one number, thus dropping the diff
    -- Input:
    --
    -- n1               n2               n3
    --     a = n2 - n1      b = n3 - n2
    --
    -- Possible operations:
    --  keep all
    --  drop n1 -> a disappears
    --  drop n2 -> the resulting diff is a + b ((n3 - n2) + (n2 - n1) = n3 - n1)
    --  drop n3 -> b disappears
    areSafeWithDampener [a, b] = safe a || safe b || safe (a + b)
    -- Input:
    --
    -- n1               n2               n3               n4   [              n5 ...
    --     a = n2 - n1      b = n3 - n2      c = n4 - n3       [ d = n5 - n4 ...
    --
    -- Possible operations:
    --  keep all
    --  drop n1 -> [b, c]
    --  drop n2 -> [a + b, c]
    --  drop n3 -> [a, b + c]
    --  drop n4 -> Can't do! Don't know how it would affect n5
    --             (compare to areSafeWithDampener [a,b], where we know there's no n4).
    areSafeWithDampener (a : b : c : rest)
      -- a and b are safe, don't drop anything and continue
      | all safe [a, b] = areSafeWithDampener (b : c : rest)
      -- a or b are unsafe, try all possible operations to fix them and check
      -- the rest of the report directly (can't drop any more numbers)
      | any (all safe) [[b, c], [a + b, c], [a, b + c]] = all safe rest
      | otherwise = False

    direction = case diffs of
      -- the first three diffs unambigously determine the direction
      (a : b : c : _) -> Just $ length (filter (> 0) [a, b, c]) >= 2
      _ -> Nothing

    safe diff =
      (isNothing direction || direction == Just (diff > 0))
        && inRange (1, 3) (abs diff)

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

part2 :: [[Int]] -> Int
part2 = length . filter isSafeWithDampener

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
