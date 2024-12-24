module Main (main) where

import Data.List (foldl', isPrefixOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)

type Wire = String

data Gate
  = And Wire Wire
  | Or Wire Wire
  | Xor Wire Wire

type Inputs = M.Map Wire Bool

type Circuit = M.Map Wire Gate

evalWire :: Inputs -> Circuit -> Wire -> Bool
evalWire inputs circuit wire =
  case M.lookup wire inputs of
    Just b -> b
    Nothing -> evalGate inputs circuit $ fromJust $ M.lookup wire circuit

evalGate :: Inputs -> Circuit -> Gate -> Bool
evalGate inputs circuit gate = case gate of
  And w1 w2 -> ev w1 && ev w2
  Or w1 w2 -> ev w1 || ev w2
  Xor w1 w2 -> ev w1 /= ev w2
  where
    ev = evalWire inputs circuit

parseInput :: String -> (Inputs, Circuit)
parseInput input = (foldl' addInput M.empty inputs, foldl' addGate M.empty gates)
  where
    (inputs, rest) = break null (lines input)
    gates = tail rest

    addInput :: Inputs -> String -> Inputs
    addInput inputs line = M.insert wire value inputs
      where
        (wire, value') = break (== ':') line
        value = read (tail value') == 1

    addGate :: Circuit -> String -> Circuit
    addGate circuit line = M.insert w3 (gate w1 w2) circuit
      where
        [w1, g, w2, "->", w3] = words line
        gate = case g of
          "AND" -> And
          "OR" -> Or
          "XOR" -> Xor

part1 :: (Inputs, Circuit) -> Int
part1 (inputs, circuit) = sum $ map evalZ zs
  where
    zs = filter ("z" `isPrefixOf`) (M.keys circuit)
    evalZ z = if evalWire inputs circuit z then 2 ^ read (tail z) else 0

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
