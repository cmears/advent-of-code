{-# LANGUAGE ViewPatterns #-}
-- See the history of this file to find the version in the Youtube video.

import Data.Char
import Data.List.Split
import qualified Data.Map as M
import Text.Regex.TDFA

submatches :: String -> String -> Maybe [String]
submatches regex s =
  case getAllTextSubmatches (s =~ regex) of
    [] -> Nothing
    matches -> Just (tail matches)

main = do
  program <- map parseLine . lines <$> readFile "input.txt"
  let go w = print . sum . M.elems . fst $ runProgram w program (M.empty, replicate 36 '0')
  go part1Writer
  go part2Writer

data Instruction = Mask String | Write Int Int
  deriving (Show)

parseLine :: String -> Instruction
parseLine (submatches "mask = (.*)" -> Just [m]) = Mask m
parseLine (submatches "mem\\[(.*)\\] = (.*)" -> Just [a,v]) = Write (read a) (read v)

runProgram :: W -> [Instruction] -> E -> E
runProgram writer instructions e = foldl (flip (execute writer)) e instructions
  
type E = (M.Map Int Int, String)
type W = Int -> Int -> String -> [(Int, Int)]

execute :: W -> Instruction -> E -> E
execute _ (Mask s) (mem,_) = (mem,s)
execute writer (Write a v) (mem,mask) = 
    (M.union (M.fromList (writer a v mask)) mem, mask)

part1Writer a v m = [(a, compute1 v m)]

part2Writer a v m = [(a', v) | a' <- compute2 a m]

compute1 :: Int -> String -> Int
compute1 n mask = string2Int . zipWith f mask . int2String $ n
  where
    f 'X' d = d
    f '1' _ = '1'
    f '0' _ = '0'

compute2 :: Int -> String -> [Int]
compute2 n mask = string2IntFloat . zipWith f mask . int2String $ n
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
