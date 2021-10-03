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
  c <- readFile file
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
    initialState = (0, initialMemory, inputs, 0)
    loop = do
      (done, stepOutputs) <- step
      followingOutputs <- if done then pure [] else loop
      pure (stepOutputs ++ followingOutputs)

type S = (Integer, Memory, Inputs, Integer)

(!!!) :: (Ord k, Num v) => M.Map k v -> k -> v
(!!!) m k = M.findWithDefault 0 k m

-- Returns true when finished.
step :: State S (Bool, Outputs)
step = do
  (ip, m, inputs, rel) <- get
  let instruction = m !!! ip
      (opcode, parameterModes) = parseInstruction instruction
      operand1 = m !!! (ip+1)
      operand2 = m !!! (ip+2)
      operand3 = m !!! (ip+3)
      address1 = operand1 + if parameterModes !! 0 == 0 then 0 else rel
      address2 = operand2 + if parameterModes !! 1 == 0 then 0 else rel
      address3 = operand3 + if parameterModes !! 2 == 0 then 0 else rel
      arg1 = if parameterModes !! 0 == 1 then operand1 else m !!! address1
      arg2 = if parameterModes !! 1 == 1 then operand2 else m !!! address2
  case opcode of
    1 -> store address3 (arg1 + arg2) >> advance 4 >> pure (False, [])
    2 -> store address3 (arg1 * arg2) >> advance 4 >> pure (False, [])
    3 -> readInput >>= store address1 >> advance 2 >> pure (False, [])
    4 -> advance 2 >> pure (False, [arg1])
    5 -> (if arg1 /= 0 then jump arg2 else advance 3) >> pure (False, [])
    6 -> (if arg1 == 0 then jump arg2 else advance 3) >> pure (False, [])
    7 -> store address3 (if arg1  < arg2 then 1 else 0) >> advance 4 >> pure (False, [])
    8 -> store address3 (if arg1 == arg2 then 1 else 0) >> advance 4 >> pure (False, [])
    9 -> adjustRelative arg1 >> advance 2 >> pure (False, [])
    99 -> pure (True, [])
    _ -> error ("unknown opcode: " ++ show opcode)

store :: Integer -> Integer -> State S ()
store address value = modify $ \(ip, m, inputs, rel) -> (ip, M.insert address value m, inputs, rel)
advance :: Integer -> State S ()
advance n = modify $ \(ip, m, inputs, rel) -> (ip+n, m, inputs, rel)
jump :: Integer -> State S ()
jump x = modify $ \(_ip, m, inputs, rel) -> (x, m, inputs, rel)
readInput :: State S Integer
readInput = do
  (ip, m, inputs, rel) <- get
  case inputs of
    [] -> error "empty input"
    (input:inputs) -> do
      put (ip, m, inputs, rel)
      pure input
-- writeOutput :: Integer -> State S ()
-- writeOutput x = modify $ \(ip, m, inputs, outputs) -> (ip, m, inputs, x:outputs)
adjustRelative :: Integer -> State S ()
adjustRelative offset = modify $ \(ip, m, inputs, rel) -> (ip, m, inputs, rel + offset)
  

parseInstruction :: Integer -> (Integer, [Integer])
parseInstruction instruction =
    (opcode, modes)
  where
    opcode = instruction `mod` 100
    modes = (map read . map (:[]) . reverse . show) (instruction `div` 100) ++ repeat 0
