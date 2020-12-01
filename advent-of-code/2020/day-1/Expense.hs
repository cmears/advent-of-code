{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

import Data.Array.Unboxed
import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Time
import Text.Printf

main = do
  xs <- (map read . lines <$> readFile "input.txt") :: IO [Integer]
  measure "input" $ print (sum xs)

  let target :: Integral a => a
      target = 2020

  -- Find 2 that sum to target, slow way.
  measure "2 slow" $ print $ listToMaybe [ (x,y, x*y) | x <- xs, y <- xs, x+y == target ]

  -- Find 3 that sum to target, slow way.
  measure "3 slow" $ print $ listToMaybe [ (x,y,z, x*y*z) | x <- xs, y <- xs, z <- xs, x+y+z == target ]

  -- Find 2 that sum to target, fast way.
  measure "2 fast" $ case fast2 target xs of
                       Nothing -> putStrLn "no solution"
                       Just (x,y) -> print (x,y,x*y)

  -- Find 3 that sum to target, fast way.
  measure "3 fast" $ case fast3 target xs of
                       Nothing -> putStrLn "no solution"
                       Just (x,y,z) -> print (x,y,z,x*y*z)

  -- Find 3 that sum to target, faster (?) way.
  measure "3 faster" $
          let a = listArray (1, length xs) (map fromInteger (sort xs))
          in case faster3 target a of
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

faster3 :: Int -> UArray Int Int -> Maybe (Int, Int, Int)
faster3 t a = loop m
  where
    (m,n) = bounds a
    loop i | i > n = Nothing
    loop i =
      let x1 = a!i
          t' = t - x1
          loop2 j k | j >= k = Nothing
                    | otherwise =
            let s = a!j + a!k
            in if | s == t' -> Just (a!i,a!j,a!k)
                  | s < t' -> loop2 (j+1) k
                  | otherwise -> loop2 j (k-1)
      in case loop2 (i+1) n of
           Just x -> Just x
           Nothing -> loop (i+1)

measure s a = do
  t1 <- getCurrentTime
  r <- a
  t2 <- getCurrentTime
  printf "%s: %s\n" s (show (diffUTCTime t2 t1))
  pure r
