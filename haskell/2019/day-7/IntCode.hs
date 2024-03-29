module IntCode where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO

runFile :: FilePath -> Inputs -> IO Outputs
runFile file inputs = do
  c <- readFile "input"
  pure (runProgram c inputs)

runProgram :: String -> Inputs -> Outputs
runProgram program inputs =
  let numbers = map read (splitOn "," program)
      initialMemory = M.fromList (zip [0..] numbers)
  in execute initialMemory inputs

type Memory = M.Map Integer Integer
type Outputs = [Integer]
type Inputs = [Integer]

execute :: Memory -> Inputs -> Outputs
execute initialMemory inputs =
    evalState loop initialState
  where
    initialState = (0, initialMemory, inputs)
    loop = do
      (done, stepOutputs) <- step
      followingOutputs <- if done then pure [] else loop
      pure (stepOutputs ++ followingOutputs)

type S = (Integer, Memory, Inputs)

-- Returns true when finished.
step :: State S (Bool, Outputs)
step = do
  (ip, m, inputs) <- get
  let instruction = m M.! ip
      (opcode, parameterModes) = parseInstruction instruction
      imm1 = m M.! (ip+1)
      imm2 = m M.! (ip+2)
      imm3 = m M.! (ip+3)
      ind1 = m M.! imm1
      ind2 = m M.! imm2
      arg1 = if parameterModes !! 0 == 0 then ind1 else imm1
      arg2 = if parameterModes !! 1 == 0 then ind2 else imm2
  case opcode of
    1 -> store imm3 (arg1 + arg2) >> advance 4 >> pure (False, [])
    2 -> store imm3 (arg1 * arg2) >> advance 4 >> pure (False, [])
    3 -> readInput >>= store imm1 >> advance 2 >> pure (False, [])
    4 -> advance 2 >> pure (False, [arg1])
    5 -> (if arg1 /= 0 then jump arg2 else advance 3) >> pure (False, [])
    6 -> (if arg1 == 0 then jump arg2 else advance 3) >> pure (False, [])
    7 -> store imm3 (if arg1 < arg2 then 1 else 0) >> advance 4 >> pure (False, [])
    8 -> store imm3 (if arg1 == arg2 then 1 else 0) >> advance 4 >> pure (False, [])
    99 -> pure (True, [])
    _ -> error ("unknown opcode: " ++ show opcode)

store :: Integer -> Integer -> State S ()
store address value = modify $ \(ip, m, inputs) -> (ip, M.insert address value m, inputs)
advance :: Integer -> State S ()
advance n = modify $ \(ip, m, inputs) -> (ip+n, m, inputs)
jump :: Integer -> State S ()
jump x = modify $ \(_ip, m, inputs) -> (x, m, inputs)
readInput :: State S Integer
readInput = do
  (ip, m, inputs) <- get
  case inputs of
    [] -> error "empty input"
    (input:inputs) -> do
      put (ip, m, inputs)
      pure input
-- writeOutput :: Integer -> State S ()
-- writeOutput x = modify $ \(ip, m, inputs, outputs) -> (ip, m, inputs, x:outputs)
  

parseInstruction :: Integer -> (Integer, [Integer])
parseInstruction instruction =
    (opcode, modes)
  where
    opcode = instruction `mod` 100
    modes = (map read . map (:[]) . reverse . show) (instruction `div` 100) ++ repeat 0
