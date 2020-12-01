import Control.Monad
import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import System.Environment
import System.IO
import Text.Printf

main = do
  c <- readFile "input"
  let numbers = map read (splitOn "," c)

  let initialMemory = M.fromList (zip [0..] numbers)

  let outputs = execute initialMemory [1]
  print outputs
  pure ()

type Memory = M.Map Integer Integer
type Outputs = [Integer]
type Inputs = [Integer]

execute :: Memory -> Inputs -> Outputs
execute initialMemory inputs =
    let (_,_,_,os) = execState loop initialState
    in os
  where
    initialState = (0, initialMemory, inputs, [])
    loop = do
      done <- step
      when (not done) loop

type S = (Integer, Memory, Inputs, Outputs)

-- Returns true when finished.
step :: State S Bool
step = do
  (ip, m, inputs, outputs) <- get
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
    1 -> store imm3 (arg1 + arg2) >> advance 4 >> pure False
    2 -> store imm3 (arg1 * arg2) >> advance 4 >> pure False
    3 -> readInput >>= store imm1 >> advance 2 >> pure False
    4 -> writeOutput arg1 >> advance 2 >> pure False
    99 -> pure True

store :: Integer -> Integer -> State S ()
store address value = modify $ \(ip, m, inputs, outputs) -> (ip, M.insert address value m, inputs, outputs)
advance :: Integer -> State S ()
advance n = modify $ \(ip, m, inputs, outputs) -> (ip+n, m, inputs, outputs)
readInput :: State S Integer
readInput = do
  (ip, m, inputs, outputs) <- get
  case inputs of
    [] -> error "empty input"
    (input:inputs) -> do
      put (ip, m, inputs, outputs)
      pure input
writeOutput :: Integer -> State S ()
writeOutput x = modify $ \(ip, m, inputs, outputs) -> (ip, m, inputs, x:outputs)
  

parseInstruction :: Integer -> (Integer, [Integer])
parseInstruction instruction =
    (opcode, modes)
  where
    opcode = instruction `mod` 100
    modes = (map read . map (:[]) . reverse . show) (instruction `div` 100) ++ repeat 0
