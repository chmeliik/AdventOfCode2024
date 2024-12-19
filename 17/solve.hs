module Main (main) where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Array.IArray (Array, bounds, elems, listArray, (!))
import Data.Bits (shiftL, shiftR, xor)
import Data.List (find, foldl', intercalate)
import Data.Maybe (catMaybes, fromJust)

data ProgramState = ProgramState
  { pInstructionPointer :: Int,
    pRegisterA :: Int,
    pRegisterB :: Int,
    pRegisterC :: Int
  }
  deriving (Show)

newState :: ProgramState
newState = ProgramState 0 0 0 0

movePointer :: (Int -> Int) -> ProgramState -> ProgramState
movePointer f p = p {pInstructionPointer = f (pInstructionPointer p)}

storeInA :: Int -> ProgramState -> ProgramState
storeInA n p = p {pRegisterA = n}

storeInB :: Int -> ProgramState -> ProgramState
storeInB n p = p {pRegisterB = n}

storeInC :: Int -> ProgramState -> ProgramState
storeInC n p = p {pRegisterC = n}

type Operand = Int

combo :: Operand -> ProgramState -> Int
combo 4 = pRegisterA
combo 5 = pRegisterB
combo 6 = pRegisterC
combo n = const n

literal :: Operand -> ProgramState -> Int
literal = const

type RunProg a = State ProgramState a

binOp ::
  (Int -> Int -> a) ->
  (ProgramState -> Int) ->
  (ProgramState -> Int) ->
  RunProg a
binOp f getA getB = do
  a <- gets getA
  b <- gets getB
  pure (f a b)

unaryOp :: (Int -> a) -> (ProgramState -> Int) -> RunProg a
unaryOp f getA = do
  a <- gets getA
  pure (f a)

divideBy :: Int -> RunProg Int
divideBy n = binOp shiftR pRegisterA (combo n)

update :: (Int -> ProgramState -> ProgramState) -> Int -> RunProg ()
update how n = modify (how n)

data Instruction
  = ADV
  | BXL
  | BST
  | JNZ
  | BXC
  | OUT
  | BDV
  | CDV
  deriving (Enum, Show)

evalInstr :: Instruction -> Operand -> RunProg (Maybe Int)
evalInstr instr n = case instr of
  ADV -> divideBy n >>= update storeInA >> nextInstr
  BDV -> divideBy n >>= update storeInB >> nextInstr
  CDV -> divideBy n >>= update storeInC >> nextInstr
  BXL -> binOp xor pRegisterB (literal n) >>= update storeInB >> nextInstr
  BXC -> binOp xor pRegisterB pRegisterC >>= update storeInB >> nextInstr
  BST -> unaryOp (`mod` 8) (combo n) >>= update storeInB >> nextInstr
  OUT -> unaryOp (`mod` 8) (combo n) >>= step (+ 2) . Just
  JNZ ->
    unaryOp (/= 0) pRegisterA >>= \jump ->
      step (if jump then const n else (+ 2)) Nothing
  where
    step :: (Int -> Int) -> Maybe Int -> RunProg (Maybe Int)
    step moveP out = modify (movePointer moveP) >> pure out

    nextInstr = step (+ 2) Nothing

type Program = Array Int Int

runProg :: Program -> RunProg [Maybe Int]
runProg program = do
  pointer <- gets pInstructionPointer
  let (_, end) = bounds program
  if pointer >= end
    then
      pure []
    else do
      let instr = toEnum (program ! pointer)
      let operand = program ! (pointer + 1)
      v <- evalInstr instr operand
      vs <- runProg program
      pure $ v : vs

evalProgram :: ProgramState -> Program -> [Maybe Int]
evalProgram p instrs = evalState (runProg instrs) p

-- My program is:
--
-- BST 4
-- BXL 2
-- CDV 5
-- BXC 5
-- ADV 3
-- BXL 7
-- OUT 5
-- JNZ 0
--
-- A couple interesting properties of this program:
--
-- * it's a simple do ... while loop, the jump is at the end and jumps to the beginning
-- * the value of register A rightshifts by 3 in every iteration
--   (=> the program definitely terminates)
-- * there is one OUT instruction => each iteration outputs one element
-- * the output of each iteration depends on *and only on* the value of A
--   * BST 4 => B = A % 8
--   * BXL 2 => B = B ^ 2
--   * CDV 5 => C = A >> B
--   * at this point, B and C have both been initialized based on nothing but A
--
-- To find the initial A such that the program returns the desired output:
--
-- * Search the output reversed, we know A is 0 at the end
-- * For each number in the reversed output:
--   * leftshift the current A by 3 to invert the rightshift
--   * the rightshift also dropped the last three bits, so you actually need to try
--     all 8 different possible values that would rightshift down to the current A
--   * find the first of the 8 possible previous As that returns the desired number
--   * make that the current A
-- * When you reach the end of the reversed output, the current A is the initial A

findInitialA :: Program -> [Int] -> Int
findInitialA prog forOutput = foldl' previousA 0 (reverse forOutput)
  where
    previousA :: Int -> Int -> Int
    previousA currentA out = fromJust $ find returnsOut possiblePrevAs
      where
        leastPrevA = currentA `shiftL` 3
        possiblePrevAs = [leastPrevA .. leastPrevA + 7]

        returnsOut :: Int -> Bool
        returnsOut a =
          (== out) $ head $ catMaybes $ evalProgram (newState {pRegisterA = a}) prog

parseInput :: String -> (ProgramState, Program)
parseInput input = (programState, listArray (0, length program - 1) program)
  where
    (stateLines, _ : programLine : _) = break null (lines input)
    ("Program:" : program' : _) = words programLine

    programState = foldl' updateState newState stateLines
    program = map read $ split ',' program'

    updateState :: ProgramState -> String -> ProgramState
    updateState p line = case words line of
      ["Register", "A:", x] -> storeInA (read x) p
      ["Register", "B:", x] -> storeInB (read x) p
      ["Register", "C:", x] -> storeInC (read x) p
      _ -> p

    split :: (Eq a) => a -> [a] -> [[a]]
    split _ [] = []
    split x xs = let (as, bs) = break (== x) xs in as : split x (drop 1 bs)

part1 :: (ProgramState, Program) -> String
part1 = intercalate "," . map show . catMaybes . uncurry evalProgram

part2 :: (ProgramState, Program) -> Int
part2 (_, prog) = findInitialA prog (elems prog)

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ part1 input
  print $ part2 input
