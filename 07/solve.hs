module Main (main) where

type Operand = Int

type Equation = (Int, [Operand])

parseInput :: String -> [Equation]
parseInput = map parseLine . lines
  where
    parseLine :: String -> Equation
    parseLine l =
      let (result' : operands) = words l
          result = takeWhile (/= ':') result'
       in (read result, map read operands)

type Operator = Operand -> Operand -> Operand

allPossibleResults :: [Operator] -> [Operand] -> [Int]
allPossibleResults _ [] = []
allPossibleResults _ [x] = [x]
allPossibleResults ops (x : y : xs) =
  [result | op <- ops, result <- allPossibleResults ops (op x y : xs)]

sumPossiblyCorrectEquations :: [Operator] -> [Equation] -> Int
sumPossiblyCorrectEquations operators =
  sum
    . map fst
    . filter
      ( \(result, operands) ->
          result `elem` allPossibleResults operators operands
      )

(.||.) :: Int -> Int -> Int
(.||.) a b =
  let n = length $ takeWhile (> 0) $ iterate (`div` 10) b
   in 10 ^ n * a + b

part1 :: [Equation] -> Int
part1 = sumPossiblyCorrectEquations [(+), (*)]

part2 :: [Equation] -> Int
part2 = sumPossiblyCorrectEquations [(+), (*), (.||.)]

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  print $ part2 input
