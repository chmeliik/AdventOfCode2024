module Main (main) where

import Control.Monad.State (State, evalState, gets, modify)
import Data.Array.IArray (Array, bounds, listArray, (!))
import Data.Bits (shiftR, xor)
import Data.List (foldl', intercalate)
import Data.Maybe (catMaybes)

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

evalAll :: Program -> RunProg [Maybe Int]
evalAll program = do
  pointer <- gets pInstructionPointer
  let (_, end) = bounds program
  if pointer >= end
    then
      pure []
    else do
      let instr = toEnum (program ! pointer)
      let operand = program ! (pointer + 1)
      v <- evalInstr instr operand
      vs <- evalAll program
      pure $ v : vs

evalProgram :: ProgramState -> Program -> [Maybe Int]
evalProgram p instrs = evalState (evalAll instrs) p

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

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ part1 input
