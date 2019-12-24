module IntCode ( runFile
               , runProgram
               , ExecutionStatus(..)
               )
where

import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO

runFile :: FilePath -> Inputs -> IO (ExecutionStatus, Outputs)
runFile file inputs = do
  c <- readFile file
  pure (runProgram c inputs)

runProgram :: String -> Inputs -> (ExecutionStatus, Outputs)
runProgram program inputs =
  let numbers = map read (splitOn "," program)
      initialMemory = M.fromList (zip [0..] numbers)
  in execute initialMemory inputs

type Memory = M.Map Integer Integer
type Outputs = [Integer]
type Inputs = [Integer]

data ExecutionStatus = ExecutionFinished | ExecutionWaiting (Inputs -> (ExecutionStatus, Outputs))

instance Show ExecutionStatus where
    show ExecutionFinished = "ExecutionFinished"
    show (ExecutionWaiting _) = "ExecutionWaiting <f>"

execute :: Memory -> Inputs -> (ExecutionStatus, Outputs)
execute initialMemory inputs =
    evalState loop initialState
  where
    initialState = (0, initialMemory, inputs, 0)
    loop = do
      (status, stepOutputs) <- step
      case status of
        Finished -> pure (ExecutionFinished, stepOutputs)
        Running -> do (s,o) <- loop
                      pure (s, stepOutputs ++ o)
        Waiting -> do (ip,mem,_,rel) <- get
                      pure (ExecutionWaiting (\inputs -> evalState loop (ip,mem,inputs,rel)), stepOutputs)

type S = (Integer, Memory, Inputs, Integer)

(!!!) :: (Ord k, Num v) => M.Map k v -> k -> v
(!!!) m k = M.findWithDefault 0 k m

data Status = Finished | Running | Waiting

step :: State S (Status, Outputs)
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
    1 -> store address3 (arg1 + arg2) >> advance 4 >> pure (Running, [])
    2 -> store address3 (arg1 * arg2) >> advance 4 >> pure (Running, [])
    3 -> do mi <- readInput
            case mi of
              Nothing -> pure (Waiting, [])
              Just i -> store address1 i >> advance 2 >> pure (Running, [])
    4 -> advance 2 >> pure (Running, [arg1])
    5 -> (if arg1 /= 0 then jump arg2 else advance 3) >> pure (Running, [])
    6 -> (if arg1 == 0 then jump arg2 else advance 3) >> pure (Running, [])
    7 -> store address3 (if arg1  < arg2 then 1 else 0) >> advance 4 >> pure (Running, [])
    8 -> store address3 (if arg1 == arg2 then 1 else 0) >> advance 4 >> pure (Running, [])
    9 -> adjustRelative arg1 >> advance 2 >> pure (Running, [])
    99 -> pure (Finished, [])
    _ -> error ("unknown opcode: " ++ show opcode)

store :: Integer -> Integer -> State S ()
store address value = modify $ \(ip, m, inputs, rel) -> (ip, M.insert address value m, inputs, rel)
advance :: Integer -> State S ()
advance n = modify $ \(ip, m, inputs, rel) -> (ip+n, m, inputs, rel)
jump :: Integer -> State S ()
jump x = modify $ \(_ip, m, inputs, rel) -> (x, m, inputs, rel)
readInput :: State S (Maybe Integer)
readInput = do
  (ip, m, inputs, rel) <- get
  case inputs of
    [] -> pure Nothing
    (input:inputs) -> do
      put (ip, m, inputs, rel)
      pure (Just input)
adjustRelative :: Integer -> State S ()
adjustRelative offset = modify $ \(ip, m, inputs, rel) -> (ip, m, inputs, rel + offset)
  

parseInstruction :: Integer -> (Integer, [Integer])
parseInstruction instruction =
    (opcode, modes)
  where
    opcode = instruction `mod` 100
    modes = (map read . map (:[]) . reverse . show) (instruction `div` 100) ++ repeat 0
