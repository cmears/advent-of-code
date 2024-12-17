import Control.Monad
import Data.Maybe
import Data.List
import Data.Bits

type Registers = (Integer,Integer,Integer)
type InstructionPointer = Int
type ExecutionState = (InstructionPointer, Registers)
type Program = [Int]

parseInput :: String -> (Registers, Program)
parseInput s = 
  let [a0,b0,c0,_,p] = lines s
      [a,b,c] = map (read . last . words) [a0,b0,c0]
      program = read $ "[" ++ last (words p) ++ "]"
  in ((a,b,c),program)

main = do
  s <- readFile "17.txt"
  let (registers, program) = parseInput s
      output = execute program (0, registers)
  putStrLn $ intercalate "," (map show output)
  let results = build2 program (reverse program) []
  let values = map (foldl (\acc x -> 8*acc+x) 0) results
  print $ minimum values

build2 :: Program -> [Int] -> [Integer] -> [[Integer]]
build2 program target partialA =
    let n = length partialA
        a = foldl (\acc x -> 8*acc+x) 0 partialA
    in if length partialA == length target
       then pure partialA
       else do x <- [0..7]
               guard (execute program (0,(a*8+x,0,0)) == reverse (take (n+1) target))
               build2 program target (partialA ++ [x])

execute :: Program -> ExecutionState -> [Int]
execute program executionState =
    case step program executionState of
      Nothing -> []
      Just (maybeOutput, exSt) -> maybeToList maybeOutput ++ execute program exSt

step :: Program -> ExecutionState -> Maybe (Maybe Int, ExecutionState)
step program (ip, (a,b,c)) | ip >= length program = Nothing
                           | otherwise =
    let (instruction:operand:_) = drop ip program
        combo :: Integer
        combo = fromIntegral $ case operand of
                  0 -> 0
                  1 -> 1
                  2 -> 2
                  3 -> 3
                  4 -> a
                  5 -> b
                  6 -> c
                  7 -> error "invalid"
        literal = operand
    in Just $ case instruction of
                0 -> (Nothing, (ip+2,(a `div` (2^combo),b,c)))
                1 -> (Nothing, (ip+2,(a,b `xor` (fromIntegral literal),c)))
                2 -> (Nothing, (ip+2,(a,combo `mod` 8,c)))
                3 -> (Nothing, if a == 0 then (ip+2, (a,b,c)) else (literal, (a,b,c)))
                4 -> (Nothing, (ip+2,(a,b `xor` c,c)))
                5 -> (Just (fromIntegral (combo `mod` 8)), (ip+2,(a,b,c)))
                6 -> (Nothing, (ip+2,(a,a `div` (2^combo),c)))
                7 -> (Nothing, (ip+2,(a,b,a `div` (2^combo))))
