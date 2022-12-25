import Control.Monad
import Data.List
import qualified Data.IntMap as I
import qualified Data.PQueue.Prio.Min as Q
import qualified Data.Set as S

type Coord = (Int,Int)

blizzardMove :: (Int,Int) -> Char -> Coord -> Coord
blizzardMove (h,w) '<' (r,c) | c == 1 = (r,w-2)
                             | otherwise = (r,c-1)
blizzardMove (h,w) '>' (r,c) | c == w-2 = (r,1)
                             | otherwise = (r,c+1)
blizzardMove (h,w) '^' (r,c) | r == 1 = (h-2,c)
                             | otherwise = (r-1,c)
blizzardMove (h,w) 'v' (r,c) | r == h-2 = (1,c)
                             | otherwise = (r+1,c)

main = do
  ls <- lines <$> readFile "24.txt"
  let h = length ls
      w = length (head ls)
  let input = [ ((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l, (x /= '.'), x /= '#' ]

  let start = (0,1)
      end = (h-1,w-2)

  let period = lcm (h-2) (w-2)

  let blizzards = [ take period (iterate (blizzardMove (h,w) x) (r,c)) | ((r,c),x) <- input ]
  let occupied = I.fromList . zip [0..] . map S.fromList $ transpose blizzards

  -- It's time t and we are plotting our next move.
  let moves (r,c) t = do
         let occ = occupied I.! ((t+1) `mod` period)
         coord <- [(r+1,c),(r-1,c),(r,c+1),(r,c-1),(r,c)]
         guard (not (coord `S.member` occ))
         guard (fst coord > 0 || coord == start)
         guard (fst coord < h-1 || coord == end)
         guard (snd coord > 0)
         guard (snd coord < w-1)
         pure coord

  let dist (r,c) (y,x) = abs (y-r) + abs (x-c)

  let mintrip = dist start end

  let astar :: Q.MinPQueue Int (Coord, Int, Int) -> Int -> S.Set (Coord, Int, Int) -> Int
      astar q incumbent seen =
          case Q.minViewWithKey q of
            Nothing -> incumbent
            Just ((k,(coord,t,phase)),q') ->
                let coords = if (phase == 3 && coord == end) || k >= incumbent || S.member (coord,t,phase) seen then [] else moves coord t
                    phase' = if phase == 1 && coord == end then 2
                             else if phase == 2 && coord == start then 3
                                  else phase
                    q'' = foldl (\qq cc -> Q.insert (t+1+dist cc (if odd phase' then end else start) + (if phase' < 2 then mintrip else 0) + (if phase' < 3 then mintrip else 0)) (cc,t+1,phase') qq) q' coords
                    incumbent' = if (phase == 3 && coord == end) then min t incumbent else incumbent
                    seen' = S.insert (coord,t,phase) seen
                in astar q'' incumbent' seen'

  print $ astar (Q.singleton mintrip (start,0,1)) (2^50) S.empty



