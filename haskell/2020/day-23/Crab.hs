{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
import Control.Monad
import Data.Array.IArray
import Data.Array.ST
import Control.Monad.ST

input :: [Int]
input = [1,5,7,6,2,3,9,8,4]

ex :: [Int]
ex = [3,8,9,1,2,5,4,6,7]

input2 = input ++ [10..10^6]
ex2 = ex ++ [10..10^6]

-- Given a linked list and the current cup, update the linked list and
-- return the new current cup.
move :: forall s. STUArray s Int Int -> Int -> ST s Int
move next current = do
    -- The block [x,y,z] that will move.
    x <- readArray next current
    y <- readArray next x
    z <- readArray next y

    n <- snd <$> getBounds next

    -- Figure out the destination cup.
    let findDest c | c == 0 = findDest n
                   | c `elem` [x,y,z] = findDest (c-1)
                   | otherwise = c
        dest = findDest (current-1)
    -- Transform:
    --     ... current, x,y,z, α, ... dest, β ...
    -- into:
    --     ... current, α, ... dest, x,y,z, β ...
    α <- readArray next z
    β <- readArray next dest
    writeArray next current α
    writeArray next dest x
    writeArray next z β

    -- Move the current cup along the circle.
    readArray next current

-- Make a linked list from the input list.
makeA :: (MArray a Int m) => [Int] -> m (a Int Int)
makeA xs = do
  a <- newArray_ (1,length xs)
  let pairs = zip xs (tail xs)
  forM_ pairs $ \(x,y) -> writeArray a x y
  writeArray a (last xs) (head xs)
  pure a

-- Iterate a monadic function n times.
iterateN :: Monad m => Int -> (a -> m a) -> a -> m a
iterateN 0 _ a = pure a
iterateN n f a = do
  a' <- f a
  iterateN (n-1) f a'

-- Compute the resulting linked list from an input list.
computeA xs n = runSTUArray $ do
                  a <- makeA xs
                  c <- iterateN n (move a) (head xs)
                  pure a

main = do
  let a1 = computeA input 100
  putStrLn . concatMap show . tail . take 9 . iterate (a1!) $ 1
  let a2 = computeA input2 (10^7)
  print $ (a2!1) * (a2!(a2!1))
