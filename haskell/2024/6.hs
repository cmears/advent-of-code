import qualified Data.Map as M
import Data.List
import Data.Array.Unboxed

main = do
  s <- readFile "6.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]
      start = fst $ M.findMin $ M.filter (=='^') m
      a = listArray ((0,0),fst (M.findMax m)) (M.elems m)
      m2a :: (Int,Int) -> UArray (Int,Int) Char
      m2a c = a // [(c,'#')]
  let cs = nub $ part1 a (start,(-1,0))
  print $ length cs
  print $ length $ filter (\c -> part2 (m2a c) (start,(-1,0)) (M.size m * 4)) (cs \\ [start])

part1 a ((r,c),(dr,dc)) = (r,c) :
  case a !? (r+dr,c+dc) of
    Just '#' -> part1 a ((r,c),rotate (dr,dc))
    Just  _  -> part1 a ((r+dr,c+dc), (dr,dc))
    Nothing  -> []

part2 a ((r,c),(dr,dc)) 0 = True
part2 a ((r,c),(dr,dc)) n =
  case a !? (r+dr,c+dc) of
    Just '#' -> part2 a ((r,c),rotate (dr,dc)) (n-1)
    Just  _  -> part2 a ((r+dr,c+dc), (dr,dc)) (n-1)
    Nothing  -> False

rotate (1,0) = (0,-1); rotate (0,-1) = (-1,0); rotate (-1,0) = (0,1); rotate (0,1) = (1,0)
