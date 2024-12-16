import Control.Monad
import Data.List
import Data.Array.Unboxed
import qualified Data.Map as M

import qualified Data.PQueue.Min as PQ

type Coord = (Int,Int)
type A = UArray Coord Char

data Dir = East | North | West | South
  deriving (Show, Eq, Ord)
dirs = [North,South,East,West]

type Node = (Coord, Dir)

type Cost = Int

type Visited = M.Map Node Cost

neighbours :: A -> Node -> [(Node, Cost)]
neighbours a (coord, dir) =
    let coord2 = move dir coord
    in (case a ! coord2 of
          '#' -> []
          _ -> [ ((coord2, dir), 1) ]) ++ 
                  [ ((coord, left dir), 1000)
                  , ((coord, right dir), 1000) ]

move North (r,c) = (r-1,c)
move South (r,c) = (r+1,c)
move East (r,c) = (r,c+1)
move West (r,c) = (r,c-1)

left North = West
left West = South
left South = East
left East = North
right = left . left . left

dijkstra :: [Node] -> A -> Visited
dijkstra nodes a = loop M.empty (PQ.fromList [(0,node)|node<-nodes])
  where
    loop visited pq =
        case PQ.minView pq of
          Nothing -> visited
          Just ((cost,node), queue) ->
            case M.lookup node visited of
              Just c | cost >= c -> loop visited queue
              _ -> let ns = [(cost+c,n) | (n,c) <- neighbours a node]
                   in loop (M.insert node cost visited) (foldl (\q (c',n') -> PQ.insert (c',n') q) queue ns)

main = do
  s <- readFile "16.txt"
  let ls = lines s
  let pairs = [ ((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ]
  let a :: A
      a = array ((0,0),(length ls-1, length (head ls) - 1)) pairs
      start = head [ (r,c) | ((r,c),x) <- pairs, x == 'S' ]
      end = head [ (r,c) | ((r,c),x) <- pairs, x == 'E' ]
  let v = dijkstra [(start, East)] a
  let best = minimum $ [ v M.! (end,dir) | dir <- dirs ]
  print best

  let v2 = dijkstra [(end,dir)|dir <- dirs] a
  let goodCoords = do
        (coord,x) <- assocs a
        guard (x /= '#')
        let costs = do dir <- dirs
                       pure ((v M.! (coord,dir)) + (v2 M.! (coord,left (left dir))))
        guard (minimum costs == best)
  print $ length goodCoords
