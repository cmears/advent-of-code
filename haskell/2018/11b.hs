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

summedarea :: M.Map (Int,Int) Int
summedarea = M.fromList $ do
               x <- [1..300]
               y <- [1..300]
               let t = if (x,y) == (1,1) then grid M.! (x,y)
                       else if x == 1 then summedarea M.! (x,y-1) + grid M.! (x,y)
                            else if y == 1 then summedarea M.! (x-1,y) + grid M.! (x,y)
                                 else summedarea M.! (x-1,y) + summedarea M.! (x,y-1) + grid M.! (x,y) - summedarea M.! (x-1,y-1)
               pure ((x,y), t)

score :: (Int,Int,Int) -> Int
score (n,x,y) =
    f (x+n-1,y+n-1) - f (x-1,y+n-1) - f (x+n-1,y-1) + f (x-1,y-1)
  where
    f (a,b) = M.findWithDefault 0 (a,b) summedarea

part1 = do
  let candidates = do
          let n = 3
          x <- [1..301-n]
          y <- [1..301-n]
          pure (n, x, y)
      best = maximumBy (comparing score) candidates
  print best

part2 = do
  let candidates = do
          n <- [1..300]
          x <- [1..301-n]
          y <- [1..301-n]
          pure (n, x, y)
      best = maximumBy (comparing score) candidates
  print best

main = part2
