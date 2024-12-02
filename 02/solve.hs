module Main (main) where

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
isSafeWithDampener report = any isSafe allPossibleReports
  where
    allPossibleReports = report : shorterReports

    shorterReports = map (`removeNth` report) [0 .. length report - 1]

    removeNth :: Int -> [a] -> [a]
    removeNth n (x : xs)
      | n > 0 = x : removeNth (n - 1) xs
      | otherwise = xs
    removeNth _ [] = error "n greater than size of list"

part1 :: [[Int]] -> Int
part1 = length . filter isSafe

part2 :: [[Int]] -> Int
part2 = length . filter isSafeWithDampener

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
