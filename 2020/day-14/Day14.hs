import Data.Char
import Data.List.Split
import qualified Data.Map as M

main = do
  program <- map parseLine . lines <$> readFile "input.txt"
--  mapM_ print program
  let (mem, _) = runProgram program (M.empty, replicate 36 '0')
--  print mem
  print . sum . M.elems $ mem

  -- flip mapM_ program $ \i -> 
  --   case i of
  --     Mask m -> print $ length (filter (=='X') m)
  --     _ -> pure ()

data Instruction = Mask String
                 | Write Int Int
  deriving (Show)

parseLine :: String -> Instruction
parseLine line =
  if take 4 line == "mask"
  then Mask (drop 7 line)
  else let a = read (takeWhile isDigit (drop 4 line))
           [_,r] = splitOn "=" line
           v = read (tail r)
       in Write a v

runProgram :: [Instruction] -> E -> E
runProgram instructions e = foldl (flip execute) e instructions
  
type E = (M.Map Int Int, String)

execute :: Instruction -> E -> E
execute (Mask s) (mem,_) = (mem,s)
execute (Write a v) (mem,mask) =
    let addresses = compute a mask
        mem' = foldr (\a' -> M.insert a' v) mem addresses
    in (mem', mask)

compute :: Int -> String -> [Int]
compute n mask =
    let s = int2String n
        s2 = zipWith f mask s
    in string2IntFloat s2
  where
    f 'X' _ = 'X'
    f '1' _ = '1'
    f '0' d = d

int2String :: Int -> String
int2String n =
  let r = int2String' $ n
  in replicate (36 - length r) '0' ++ reverse r
  where
    int2String' 0 = ""
    int2String' n = (if even n then '0' else '1'):(int2String' (n `div` 2))

string2Int :: String -> Int
string2Int = foldl (\acc d -> acc*2 + digitToInt d) 0

string2IntFloat :: String -> [Int]
string2IntFloat = string2IntFloat' . reverse
  where
    string2IntFloat' [] = pure 0
    string2IntFloat' (d:ds) = do
      rest <- string2IntFloat' ds
      case d of
        '1' -> [2*rest + 1]
        '0' -> [2*rest + 0]
        'X' -> [2*rest + 0, 2*rest + 1]
