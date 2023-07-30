import Control.Monad
import Data.List
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Data.Ord

power serial x y =
    let rack = x + 10
    in hundreds ((rack * y + serial) * rack) - 5

hundreds x = x `mod` 1000 `div` 100

serial = 7857

type A = UArray (Int,Int) Int

grid :: A
grid = array ((1,1),(300,300)) $ do
         x <- [1..300]
         y <- [1..300]
         pure ((x,y), power serial x y)

summedarea :: A
summedarea = runSTUArray $ do
               a <- newArray ((1,1),(300,300)) 0
               let f (x,0) = pure 0
                   f (0,y) = pure 0
                   f (x,y) = readArray a (x,y)
               forM_ [1..300] $ \x -> do
                 forM_ [1..300] $ \y -> do
                   rest <- sum <$> sequenceA [f (x-1,y), f (x,y-1), negate <$> f (x-1,y-1)]
                   let t = grid ! (x,y) + rest
                   writeArray a (x,y) t
               pure a

score :: (Int,Int,Int) -> Int
score (n,x,y) =
    f (x+n-1,y+n-1) - f (x-1,y+n-1) - f (x+n-1,y-1) + f (x-1,y-1)
  where
    f (0,b) = 0
    f (a,0) = 0
    f (a,b) = summedarea ! (a,b)

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

main = part1 >> part2
