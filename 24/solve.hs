module Main (main) where

import Control.Monad.State (State, execState, gets, modify)
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

evalWire :: Circuit -> Wire -> State Inputs Bool
evalWire circuit wire = do
  maybeVal <- gets (M.lookup wire)
  case maybeVal of
    Just b -> pure b
    Nothing -> do
      b <- evalGate circuit $ fromJust $ M.lookup wire circuit
      modify (M.insert wire b)
      pure b

evalGate :: Circuit -> Gate -> State Inputs Bool
evalGate circuit gate = case gate of
  And w1 w2 -> (&&) <$> ev w1 <*> ev w2
  Or w1 w2 -> (||) <$> ev w1 <*> ev w2
  Xor w1 w2 -> (/=) <$> ev w1 <*> ev w2
  where
    ev = evalWire circuit

evalAll :: Inputs -> Circuit -> Inputs
evalAll inputs circuit = execState evalWires inputs
  where
    evalWires = mapM (evalWire circuit) (M.keys circuit)

toNumber :: Inputs -> String -> Int
toNumber inputs wiretype = sum $ map eval wires
  where
    wires = filter (wiretype `isPrefixOf`) (M.keys inputs)
    eval w = if M.lookup w inputs == Just True then 2 ^ read (tail w) else 0

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
part1 (inputs, circuit) = toNumber (evalAll inputs circuit) "z"

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
