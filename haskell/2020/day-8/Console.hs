import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

-- Data representation
----------------------
data Instruction = Nop Int | Jmp Int | Acc Int
  deriving (Show)

type Program = IM.IntMap Instruction

-- Parsing
----------

type Parser = Parsec Void String

space :: Parser ()
space = L.space space1 empty empty
symbol = L.symbol space
lexeme = L.lexeme space
number = L.signed space (lexeme L.decimal)

instruction :: Parser Instruction
instruction = nop <|> acc <|> jmp
nop = symbol "nop" *> (Nop <$> number)
acc = symbol "acc" *> (Acc <$> number)
jmp = symbol "jmp" *> (Jmp <$> number)

program :: Parser Program
program = IM.fromAscList . zip [0..] <$> many instruction

-- Interpreting
---------------

-- Program counter (address of instruction to be executed) and accumulator.
type ExecutionState = (Int, Int)

-- Start at instruction zero, with accumulator value zero.
initialExecutionState = (0,0)

-- Execute one instruction.
-- The PC must be valid.
data StepResult = NewState ExecutionState
                | SafeTermination ExecutionState
                | Crash ExecutionState
step :: Program -> ExecutionState -> StepResult
step p (pc,acc) =
  if pc == fst (IM.findMax p) + 1
  then SafeTermination (pc,acc)
  else case IM.lookup pc p of
         Just i -> NewState (stepi i (pc,acc))
         Nothing -> Crash (pc,acc)


stepi :: Instruction -> ExecutionState -> ExecutionState
stepi i (pc,acc) =
  case i of
    Nop _ -> (pc+1,acc)
    Acc x -> (pc+1,acc+x)
    Jmp o -> (pc+o,acc)


-- Execute a program to termination, returning a trace of the execution.
-- Note that it returns an infinite list in the event of an infinite loop.
trace :: Program -> ExecutionState -> [ExecutionState]
trace p = go
  where go es = let result = case step p es of
                               SafeTermination _ -> []
                               NewState es' -> go es'
                               Crash _ -> error "oops"
                in es : result

-- What happens when you execute this program?
--   Does it terminate or loop?
data Result = InfiniteLoop ExecutionState
            | Termination ExecutionState
  deriving (Show)
simulate :: Program -> ExecutionState -> Result
simulate p es =
    -- Invariant: s is the set of instruction addresses we've already executed.
    -- Returns the state at the point before we'd execute an instruction a second time.
    let loop s [(pc,acc)] = Termination (pc,acc)
        loop s ((pc,acc):rest) | S.member pc s = InfiniteLoop (pc,acc)
                               | otherwise = loop (S.insert pc s) rest
    in loop S.empty (trace p es)



-- What are the possible corruptions of the program?
corruptions :: Program -> [Program]
corruptions p = do
  -- Choose an address to corrupt.
  a <- [0..fst (IM.findMax p)]
  -- Compute the corrupted instruction.
  i' <- case p IM.! a of
          Nop x -> pure (Jmp x)
          Jmp x -> pure (Nop x)
          Acc x -> mempty
  -- Overwrite the instruction.
  pure (IM.insert a i' p)


main = do
  c <- readFile "input.txt"
  case parse program "" c of
    Left e -> putStrLn (errorBundlePretty e)
    Right p -> do
      -- What happens when we execute the program?
      print (simulate p initialExecutionState)
      -- Compute the result of every corruption that terminates.
      let terminationStates =
              map (\cp -> case simulate cp initialExecutionState of
                     Termination es -> Just es
                     InfiniteLoop es -> Nothing) (corruptions p)
      -- Print them out (there should be only one).
      print $ catMaybes terminationStates
