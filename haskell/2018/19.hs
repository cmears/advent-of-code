import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as S
import Data.Set (Set)

import Debug.Trace

type Instruction = (Operation, (Int, Int, Int))

data Operation = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
  deriving (Eq, Ord, Enum, Show)

main = do
  -- Part 1 & 2 by inspection of program
  let part1 = 926
      part2 = 10551326
  print $ sum $ divisors $ part1
  print $ sum $ divisors $ part2

  -- Part 1 by running the program
  (ip, instructions) <- parseInput <$> readFile "19.txt"
  print $ (`Seq.index` 0) $ execState (evaluateProgram ip (Seq.fromList instructions)) (Seq.fromList [0,0,0,0,0,0])

divisors :: Int -> [Int]
divisors x =
  let ns = filter (\n -> x `mod` n == 0) [1..(floor (sqrt (fromIntegral x) :: Double))]
  in nub $ ns ++ ((x `div`) <$> ns)

parseInput :: String -> (Int, [Instruction])
parseInput s =
    let (directive:rest) = lines s
        instructions = map parseInstruction rest
        ["#ip", n] = words directive
    in (read n, instructions)

parseInstruction :: String -> Instruction
parseInstruction s =
  let [o,a,b,c] = words s
  in (parseOp o, (read a, read b, read c))

parseOp s = fromJust (find (\o -> map toLower (show o) == s) [Addr .. Eqrr])

type Registers = Seq Int

evaluate :: Operation -> (Int,Int,Int) -> State Registers ()
evaluate Addr (a,b,c) = liftM2 (+) (reg a) (reg b) >>= set c
evaluate Addi (a,b,c) = liftM2 (+) (reg a) (pure b) >>= set c
evaluate Mulr (a,b,c) = liftM2 (*) (reg a) (reg b) >>= set c
evaluate Muli (a,b,c) = liftM2 (*) (reg a) (pure b) >>= set c
evaluate Banr (a,b,c) = liftM2 (.&.) (reg a) (reg b) >>= set c
evaluate Bani (a,b,c) = liftM2 (.&.) (reg a) (pure b) >>= set c
evaluate Borr (a,b,c) = liftM2 (.|.) (reg a) (reg b) >>= set c
evaluate Bori (a,b,c) = liftM2 (.|.) (reg a) (pure b) >>= set c
evaluate Setr (a,_,c) = reg a >>= set c
evaluate Seti (a,_,c) = set c a
evaluate Gtir (a,b,c) = liftM2 (>) (pure a) (reg b) >>= set c . bool2int
evaluate Gtri (a,b,c) = liftM2 (>) (reg a) (pure b) >>= set c . bool2int
evaluate Gtrr (a,b,c) = liftM2 (>) (reg a) (reg b) >>= set c . bool2int
evaluate Eqir (a,b,c) = liftM2 (==) (pure a) (reg b) >>= set c . bool2int
evaluate Eqri (a,b,c) = liftM2 (==) (reg a) (pure b) >>= set c . bool2int
evaluate Eqrr (a,b,c) = liftM2 (==) (reg a) (reg b) >>= set c . bool2int

evaluateWithIp :: Int -> Instruction -> State Registers ()
evaluateWithIp ip instruction = do
  uncurry evaluate instruction
  modify (Seq.adjust succ ip)

evaluateProgram :: Int -> Seq Instruction -> State Registers ()
evaluateProgram ip instructions = do
  idx <- reg ip
  if idx < 0 || idx >= Seq.length instructions
    then pure ()
    else do let instruction = instructions `Seq.index` idx
            s <- get
            evaluateWithIp ip instruction
            evaluateProgram ip instructions

bool2int = fromEnum

reg :: Int -> State Registers Int
reg r = (`Seq.index` r) <$> get
set :: Int -> Int -> State Registers ()
set r x = modify (Seq.update r x)


