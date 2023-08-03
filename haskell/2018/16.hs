import Control.Monad.State
import Data.Bits
import Data.Foldable (toList)
import Data.List
import Data.List.Split
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as S
import Data.Set (Set)

type TestCase = (Registers, MysteryInstruction, Registers)

type MysteryInstruction = (Int, (Int, Int, Int))

type Instruction = (Operation, (Int, Int, Int))

data Operation = Addr | Addi | Mulr | Muli | Banr | Bani | Borr | Bori | Setr | Seti | Gtir | Gtri | Gtrr | Eqir | Eqri | Eqrr
  deriving (Eq, Ord, Enum, Show)

main = do
  (testCases, program) <- parseInput <$> readFile "16.txt"
  let threeOrMoreCases = filter (\tc -> numConsistentInterpretations tc >= 3) testCases
  print $ length threeOrMoreCases
  let candidates = deduceCandidates testCases
  let deductions = deduceInstructions (zip [0..] (toList candidates))
  let realProgram = map (demystifyInstruction deductions) program
  print $ execState (evaluateProgram realProgram) (Seq.replicate 4 0)

parseInput :: String -> ([TestCase], [MysteryInstruction])
parseInput s =
    let [part1, part2] = splitOn "\n\n\n\n" s
        testCases = map parseTestCase (splitOn "\n\n" part1)
        program = map parseMysteryInstruction (lines part2)
    in (testCases, program)

parseMysteryInstruction :: String -> MysteryInstruction
parseMysteryInstruction s = let [o,a,b,c] = (map read . words) s in (o,(a,b,c))

parseTestCase :: String -> TestCase
parseTestCase s =
    let [s1,s2,s3] = lines s
        before = Seq.fromList $ read (drop 8 s1)
        mystery = parseMysteryInstruction s2
        after = Seq.fromList $ read (drop 8 s3)
    in (before, mystery, after)

interpretations :: MysteryInstruction -> [Instruction]
interpretations (_,operands) = [ (op,operands) | op <- [Addr .. Eqrr] ]

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

evaluateProgram :: [Instruction] -> State Registers ()
evaluateProgram = mapM_ (uncurry evaluate)

bool2int = fromEnum

reg :: Int -> State Registers Int
reg r = (`Seq.index` r) <$> get
set :: Int -> Int -> State Registers ()
set r x = modify (Seq.update r x)

consistentOperations :: TestCase -> Set Operation
consistentOperations (before, mystery, after) =
    let check (op,operands) = execState (evaluate op operands) before == after
        consistentInstructions = filter check (interpretations mystery)
    in S.fromList (map fst consistentInstructions)

numConsistentInterpretations :: TestCase -> Int
numConsistentInterpretations = S.size . consistentOperations

deduceCandidates :: [TestCase] -> Seq (Set Operation)
deduceCandidates testCases = foldl f (Seq.replicate 16 (S.fromList [Addr .. Eqrr])) testCases
  where
    f :: Seq (Set Operation) -> TestCase -> Seq (Set Operation)
    f candidates testCase =
        let (_, (opcode, _), _) = testCase
            reduced = S.intersection (candidates `Seq.index` opcode) (consistentOperations testCase)
        in Seq.update opcode reduced candidates

deduceInstructions :: [(Int, Set Operation)] -> Seq Operation
deduceInstructions candidates =
    let deductions = loop [(op,S.toList ops) | (op,ops) <- candidates] S.empty []
    in Seq.fromList (map snd (sort deductions))
  where
    loop [] _ acc = acc
    loop cs used acc = do
      let ((opcode,operations):cs') = sortOn (length . snd) cs
      op <- operations
      guard (not (S.member op used))
      loop cs' (S.insert op used) ((opcode,op):acc)
    
demystifyInstruction :: Seq Operation -> MysteryInstruction -> Instruction
demystifyInstruction deductions (opcode,operands) =
    (deductions `Seq.index` opcode, operands)

