import Control.Monad
import Data.List
import Data.Array.Unboxed
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

type Coord = (Int,Int)
type Visited = M.Map Node Cost
type Cost = Int
type Node = Coord
type A = UArray Coord Char

main = do
  s <- readFile "20.txt"
  let ls = lines s
  let pairs = [ ((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ]
  let a :: A
      a = array ((0,0),(length ls-1, length (head ls) - 1)) pairs
      start = head [ (r,c) | ((r,c),x) <- pairs, x == 'S' ]
      end   = head [ (r,c) | ((r,c),x) <- pairs, x == 'E' ]
  let va = array (bounds a) (M.toList (bfs [start] a))
  mapM_ (print . length . filter (>= 100) . cheats a va) [2,20]

cheats :: A -> Array Coord Cost -> Int -> [Int]
cheats a va limit = do
  let ((0,0),(mx,my)) = bounds a
  (x1,y1) <- indices a
  guard (a ! (x1,y1) /= '#')
  x2 <- [max 1 (x1-limit) .. min (mx-1) (x1+limit)]
  let dx = abs(x1-x2)
  y2 <- [max 1 (y1-(limit-dx)) .. min (my-1) (y1+(limit-dx))]
  guard (a ! (x2,y2) /= '#')
  let manhattan = dx + abs(y2-y1)
  let diff = (va ! (x2,y2)) - (va ! (x1,y1))
  pure (diff - manhattan)

neighbours a (x,y) = [ (c,1) | c <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)], a ! c /= '#' ]

bfs :: [Node] -> A -> Visited
bfs nodes a = loop M.empty (Seq.fromList [(0,node)|node<-nodes])
  where
    loop :: Visited -> Seq.Seq (Cost,Node) -> Visited
    loop visited q =
        case Seq.viewl q of
          Seq.EmptyL -> visited
          (cost,node) Seq.:< queue ->
            case M.lookup node visited of
              Just c -> loop visited queue
              _ -> let ns = [(cost+c,n) | (n,c) <- neighbours a node]
                   in loop (M.insert node cost visited) (foldl (Seq.|>) queue ns)
