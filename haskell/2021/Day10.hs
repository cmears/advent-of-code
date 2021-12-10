import Data.Either
import Data.List
import Data.Maybe
main = do
  (c,i) <- partitionEithers . map (loop []) . lines <$> readFile "input10"
  print . sum . map score1 $ c
  print . median . map score2 $ i

loop ss     []                    = Right ss
loop ss     (x:xs) | open x       = loop (x:ss) xs
loop (s:ss) (x:xs) | x `closes` s = loop ss xs
loop _      (x:_)                 = Left x

median xs = sort xs !! (length xs `div` 2)
score1 c = fromJust (lookup c scores)
scores = [(')',3),(']',57),('}',1197),('>',25137),('(',1),('[',2),('{',3),('<',4)]
open = (`elem` "({[<")
a `closes` b = (b,a) `elem` pairs
pairs = [('(',')'),('[',']'),('{','}'),('<','>')]
score2 = foldl (\acc c -> acc * 5 + score1 c) 0
