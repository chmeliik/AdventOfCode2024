module Main (main) where

import Control.Monad.State (State, execState, gets, modify)
import Data.List (find, foldl', intercalate, sort)
import Data.Map qualified as M
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Set qualified as S

data Wire = NumberedWire Char Int | Wire String deriving (Eq, Ord)

instance Show Wire where
  show (NumberedWire c n) = c : show n
  show (Wire s) = s

data GateType = And | Or | Xor deriving (Eq, Ord, Show)

data Gate = Gate GateType Wire Wire
  deriving (Show)

instance Eq Gate where
  Gate g1 w1 w2 == Gate g2 w3 w4 =
    g1 == g2 && (w1 == w3 && w2 == w4 || w1 == w4 && w2 == w3)

instance Ord Gate where
  compare (Gate g1 w1 w2) (Gate g2 w3 w4) =
    compare
      (g1, min w1 w2, max w1 w2)
      (g2, min w3 w4, max w3 w4)

type Inputs = M.Map Wire Bool

type Circuit = M.Map Wire Gate

evalWire :: Circuit -> Wire -> State Inputs Bool
evalWire circuit wire = do
  maybeVal <- gets (M.lookup wire)
  case maybeVal of
    Just b -> pure b
    Nothing -> do
      b <- maybe (pure False) (evalGate circuit) $ M.lookup wire circuit
      modify (M.insert wire b)
      pure b

evalGate :: Circuit -> Gate -> State Inputs Bool
evalGate circuit (Gate g w1 w2) = case g of
  And -> (&&) <$> ev w1 <*> ev w2
  Or -> (||) <$> ev w1 <*> ev w2
  Xor -> (/=) <$> ev w1 <*> ev w2
  where
    ev = evalWire circuit

evalFirstNBits :: Int -> Inputs -> Circuit -> Inputs
evalFirstNBits n inputs circuit =
  execState (mapM (evalWire circuit) relevantOutputs) relevantInputs
  where
    relevantInputs = M.filterWithKey (\w _ -> isRelevantInput w) inputs
    relevantOutputs = filter isRelevantOutput $ M.keys circuit

    isRelevantInput (NumberedWire c n') | c `elem` "xy" = n' < n
    isRelevantInput _ = False

    -- the output has +1 bits compared to the inputs
    isRelevantOutput (NumberedWire 'z' n') = n' <= n
    isRelevantOutput _ = False

evalAll :: Inputs -> Circuit -> Inputs
evalAll = evalFirstNBits 45 -- inputs are 45-bit

toNumber :: Inputs -> Char -> Int
toNumber inputs wiretype = sum $ map (eval wiretype) $ M.toList inputs
  where
    eval c (NumberedWire c' n, b) | c == c' = if b then 2 ^ n else 0
    eval _ _ = 0

addsNBitsCorrectly :: Int -> Inputs -> Circuit -> Bool
addsNBitsCorrectly n inputs circuit = x + y == z
  where
    inputs' = evalFirstNBits n inputs circuit
    x = toNumber inputs' 'x'
    y = toNumber inputs' 'y'
    z = toNumber inputs' 'z'

fixNBitAdder :: Int -> Inputs -> Circuit -> Circuit
fixNBitAdder n inputs circuit = circuit'''
  where
    prevX = NumberedWire 'x' (n - 1)
    prevY = NumberedWire 'y' (n - 1)
    prevZ = NumberedWire 'z' (n - 1)
    currX = NumberedWire 'x' n
    currY = NumberedWire 'y' n
    currZ = NumberedWire 'z' n

    Just (Gate _ w1 w2) = M.lookup prevZ circuit
    carry1 = Gate And w1 w2
    carry2 = Gate And prevX prevY
    (carry, circuit') = findOrSwap circuit Or carry1 carry2
    (result, circuit'') = findOrSwap circuit' Xor carry (Gate Xor currX currY)
    circuit''' = pointWireTo circuit'' currZ result

    findOrSwap :: Circuit -> GateType -> Gate -> Gate -> (Gate, Circuit)
    findOrSwap circuit gt g1 g2 =
      if isJust correctGate
        then (toGate $ fromJust correctGate, circuit)
        else (Gate gt w1' w2', pointWireTo circuit wrongWire missingGate)
      where
        needGates = S.fromList [g1, g2]

        interestingGates :: [(Wire, Wire, Maybe Gate, Maybe Gate)]
        interestingGates =
          [ (w1', w2', g1', g2')
            | (Gate gt' w1' w2') <- M.elems circuit,
              gt' == gt,
              let [g1', g2'] = map (`M.lookup` circuit) [w1', w2'],
              let connectedGates = S.fromList $ catMaybes [g1', g2'],
              not $ S.null (S.intersection needGates connectedGates)
          ]

        toGate :: (Wire, Wire, Maybe Gate, Maybe Gate) -> Gate
        toGate (w1', w2', _, _) = Gate gt w1' w2'

        correctGate =
          find
            (\(_, _, g1', g2') -> S.fromList (catMaybes [g1', g2']) == needGates)
            interestingGates

        (w1', w2', g1', g2') = head interestingGates
        missingGate = if g1 `elem` catMaybes [g1', g2'] then g2 else g1
        wrongWire = if g1' `elem` map Just [g1, g2] then w2' else w1'

    pointWireTo :: Circuit -> Wire -> Gate -> Circuit
    pointWireTo circuit w g = M.insert w g $ M.update (const g') w' circuit
      where
        w' = fst $ fromJust $ find (\(k, v) -> v == g) (M.toList circuit)
        g' = M.lookup w circuit

fixWholeAdder :: Inputs -> Circuit -> Circuit
fixWholeAdder inputs circuit = foldl' fixIfNeeded circuit [1 .. 45]
  where
    fixIfNeeded :: Circuit -> Int -> Circuit
    fixIfNeeded c n =
      if addsNBitsCorrectly n inputs c
        then c
        else fixNBitAdder n inputs c

parseInput :: String -> (Inputs, Circuit)
parseInput input = (foldl' addInput M.empty inputs, foldl' addGate M.empty gates)
  where
    (inputs, rest) = break null (lines input)
    gates = tail rest

    parseWire :: String -> Wire
    parseWire (c : cs)
      | c `elem` "xyz" = NumberedWire c (read cs)
      | otherwise = Wire (c : cs)

    addInput :: Inputs -> String -> Inputs
    addInput inputs line = M.insert wire value inputs
      where
        (wire', value') = break (== ':') line
        wire = parseWire wire'
        value = read (tail value') == 1

    addGate :: Circuit -> String -> Circuit
    addGate circuit line = M.insert w3 (Gate g w1 w2) circuit
      where
        [w1', g', w2', "->", w3'] = words line
        [w1, w2, w3] = map parseWire [w1', w2', w3']
        g = case g' of
          "AND" -> And
          "OR" -> Or
          "XOR" -> Xor

part1 :: (Inputs, Circuit) -> Int
part1 (inputs, circuit) = toNumber (evalAll inputs circuit) 'z'

part2 :: (Inputs, Circuit) -> String
part2 (inputs, circuit) = intercalate "," $ sort $ map show changedWires
  where
    circuit' = fixWholeAdder inputs circuit
    changedWires =
      [w | ((w, g1), (_, g2)) <- zip (M.toList circuit) (M.toList circuit'), g1 /= g2]

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  print $ part1 input
  putStrLn $ part2 input
