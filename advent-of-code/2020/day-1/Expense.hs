{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

import Data.List
import Data.Maybe
import Data.Time
import Text.Printf

main = do
  xs <- (map read . lines <$> readFile "input.txt") :: IO [Integer]
  measure "input" $ print (sum xs)

  -- Find 2 that sum to 2020, slow way.
  measure "2 slow" $ print [ (x,y, x*y) | x <- xs, y <- xs, x+y == 2020 ]

  -- Find 3 that sum to 2020, slow way.
  measure "3 slow" $ print [ (x,y,z, x*y*z) | x <- xs, y <- xs, z <- xs, x+y+z == 2020 ]

  -- Find 2 that sum to 2020, fast way.
  measure "2 fast" $ case fast2 2020 xs of
                       Nothing -> putStrLn "no solution"
                       Just (x,y) -> print (x,y,x*y)

  -- Find 3 that sum to 2020, fast way.
  measure "3 fast" $ case fast3 2020 xs of
                       Nothing -> putStrLn "no solution"
                       Just (x,y,z) -> print (x,y,z,x*y*z)
         

-- Assumes there's no duplicate numbers (e.g. 1010 and 1010).
fast2 :: Integer -> [Integer] -> Maybe (Integer, Integer)
fast2 t xs0 =
    let xs1 = sort xs0
        ys1 = reverse xs1
    in loop t xs1 ys1

-- Assumes no duplicates.
fast3 :: Integer -> [Integer] -> Maybe (Integer, Integer, Integer)
fast3 t xs0 = 
    let xs1 = sort xs0
    in listToMaybe $ do
         (x:xs2) <- tails xs1
         let ys2 = reverse xs2
         let t' = t - x
         (\(b,c) -> (x,b,c)) <$> maybeToList (loop t' xs2 ys2)

-- xs must be sorted ascending; ys must be sorted descending.
loop :: Integer -> [Integer] -> [Integer] -> Maybe (Integer, Integer)
loop t (x:xs) (y:ys) =
    let s = x+y
    in if | s == t && x /= y -> Just (x,y)
          | s == t -> loop t xs (y:ys)
          | s <  t -> loop t xs (y:ys)
          | s >  t -> loop t (x:xs) ys
loop _ _ _ = Nothing

measure s a = do
  t1 <- getCurrentTime
  r <- a
  t2 <- getCurrentTime
  printf "%s: %s\n" s (show (diffUTCTime t2 t1))
  pure r
