import Control.Monad
import Data.List
import qualified Data.Map as M
import Data.Ord

power serial x y =
    let rack = x + 10
    in hundreds ((rack * y + serial) * rack) - 5

hundreds x = x `mod` 1000 `div` 100

serial = 7857

grid :: M.Map (Int,Int) Int
grid = M.fromList $ do
         x <- [1..300]
         y <- [1..300]
         pure ((x,y), power serial x y)

scoregrid :: M.Map (Int, Int, Int) Int
scoregrid = M.fromList $ do
              n <- [1..300]
              x <- [1..301-n]
              y <- [1..301-n]
              
              let t = if n == 1 then grid M.! (x,y)
                      else scoregrid M.! (n-1, x+1, y+1) + edge
                           where edge = sum (map (grid M.!) edgecoords)
                                 edgecoords = (x,y):[(x+i,y)|i<-[1..n-1]]++[(x,y+i)|i<-[1..n-1]]
              pure ((n,x,y),t)

score (x, y) = sum $ map (grid M.!) coords
  where
    coords = [ (x,y), (x+1,y), (x+2,y)
             , (x,y+1), (x+1,y+1), (x+2,y+1)
             , (x,y+2), (x+1,y+2), (x+2,y+2) ]

scoreN (n, x, y) = sum $ map (grid M.!) coords
  where
    coords = do
      dx <- [0..n-1]
      dy <- [0..n-1]
      pure (x+dx, y+dy)

part1 = do
  let candidates = (,) <$> [1..298] <*> [1..298]
  let best = maximumBy (comparing score) candidates
  print best

part2 = do
  let candidates = do
          n <- [1..300]
          x <- [1..301-n]
          y <- [1..301-n]
          pure (n, x, y)
      best = maximumBy (comparing (scoregrid M.!)) candidates
  print best

main = part2
