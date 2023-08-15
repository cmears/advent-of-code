{-# LANGUAGE TupleSections #-}
import Control.Monad.State
import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

type Coord = (Int,Int)
type Edge = (Coord,Coord)

type S = State (Set Edge)

explore :: [Coord] -> String -> S ()
explore (c:cs) ('W':xs) = move c (-1,0) >>= \c' -> explore (c':cs) xs
explore (c:cs) ('E':xs) = move c (1,0) >>= \c' -> explore (c':cs) xs
explore (c:cs) ('S':xs) = move c (0,-1) >>= \c' -> explore (c':cs) xs
explore (c:cs) ('N':xs) = move c (0,1) >>= \c' -> explore (c':cs) xs
explore (c:cs) ('(':xs) = explore (c:c:cs) xs
explore (c:d:cs) ('|':xs) = explore (d:d:cs) xs
explore (c:cs) (')':xs) = explore cs xs
explore (c:cs) ('^':xs) = explore (c:cs) xs
explore (c:cs) ('$':xs) = pure ()

move :: Coord -> (Int,Int) -> S Coord
move (x,y) (a,b) = do
  let c' = (x+a,y+b)
      e = ((x,y),c')
  modify (S.insert e)
  pure c'

main = do
  s <- readFile "20.txt"
  let edges = execState (explore [(0,0)] s) S.empty
  let distances = map fst $ bfs (0,0) edges
  print $ last distances
  print $ length $ filter (>=1000) distances

bfs :: Coord -> Set Edge -> [(Int, Coord)]
bfs coord edges =
    let biedges = S.fromList $ concat [ [(a,b),(b,a)] | (a,b) <- S.toList edges ]
        m = M.fromListWith (++) [ (a,[b]) | (a,b) <- S.toList biedges ]
    in bfs' coord m

bfs' :: Coord -> Map Coord [Coord] -> [(Int, Coord)]
bfs' coord m = loop 0 [coord] S.empty
  where
    loop _ [] _ = []
    loop i frontier seen =
      let seen' = seen `S.union` S.fromList frontier
          frontier' = filter (not . (`S.member` seen')) [ b | a <- frontier, b <- fromMaybe (error ("?" ++ show a)) (M.lookup a m) ]
      in map (i,) frontier ++ loop (i+1) frontier' seen'
      
