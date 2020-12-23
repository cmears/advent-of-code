{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
import qualified Data.IntMap.Strict as M
import Control.Monad
import Debug.Trace
import qualified Data.Array.IArray as I
import Data.Array.MArray
import Data.Array.ST
import Control.Monad.ST
import Data.Array.Unsafe
import Data.Array.Unboxed (UArray)

input :: [Int]
input = [1,5,7,6,2,3,9,8,4]

ex :: [Int]
ex = [3,8,9,1,2,5,4,6,7]

move :: [Int] -> [Int]
move (c:cs) =
    let (chunk, rest) = splitAt 3 cs
        n = length (c:cs)
        dest = head $ dropWhile (`elem` chunk) (map ((+1) . (`mod` n) . (subtract 1)) [c-1,c-2..])
        ((_:front),(_:back)) = break (==dest) (c:rest)
    in front ++ [dest] ++ chunk ++ back ++ [c]

moves :: [Int] -> [[Int]]
moves = iterate move

result :: [Int] -> String
result cs =
    let (front,(_:back)) = break (==1) cs
    in concatMap show $ back ++ front

input2 = input ++ [10..10^6]
ex2 = ex ++ [10..10^6]

-- A cup circle is represented by a doubly linked list.  Each cup
-- knows who comes before it, and who comes after it.
type S = (Int, M.IntMap Int, M.IntMap Int)

move2 :: S -> S
move2 (current, prev, next) =
    let [_,x,y,z] = take 4 circle
        findDest c | c == 0 = findDest (M.size next)
                   | c `elem` [x,y,z] = findDest (c-1)
                   | otherwise = c
        dest = findDest (current-1)
        -- Transform:
        --     ... current, x,y,z, α, ... dest, β ...
        -- into:
        --     ... current, α, ... dest, x,y,z, β ...
        updates = [ (current, after z)
                  , (dest, x)
                  , (z, after dest) ]
        next' = M.union (M.fromList updates) next
        prev' = M.union (M.fromList (map swap updates)) prev
    in -- trace (show (current, [x,y,z], dest, updates, showS (current,prev,next))) $
       (next' M.! current, prev', next')
  where
    circle = loop current
    loop c = c : loop (next M.! c)
    after x = next M.! x
    before x = prev M.! x
    swap (a,b) = (b,a)

circle (current, prev, next) =
  loop current
  where
    loop c = c : loop (next M.! c)

makeS xs =
    let next = M.fromList $ zip xs (tail xs ++ [head xs])
        prev = M.fromList $ zip (tail xs ++ [head xs]) xs
    in (head xs, prev, next)

showS :: S -> String
showS (current, prev, next) =
    let n = M.size next
        big = n > 25
        cs = take (min n 20) (circle (current, prev, next))
        cs' = take (min n 20) (circle (current, next, prev))
        ok = head cs == head cs' && tail cs == reverse (tail cs')
    in show (ok, cs, cs')

moves2 = iterate move2

result2 s =
    let (_:α:β:_) = circle s
    in α * β

a ! i = readArray a i

move3 :: forall s. STUArray s Int Int -> Int -> ST s Int
move3 next current = do
    x <- next ! current
    y <- next ! x
    z <- next ! y

--    a <- freeze next

    n <- snd <$> getBounds next

    let findDest c | c == 0 = findDest n
                   | c `elem` [x,y,z] = findDest (c-1)
                   | otherwise = c
        dest = findDest (current-1)
    -- Transform:
    --     ... current, x,y,z, α, ... dest, β ...
    -- into:
    --     ... current, α, ... dest, x,y,z, β ...
    α <- next!z
    β <- next!dest
    writeArray next current α
    writeArray next dest x
    writeArray next z β

    current' <- next ! current

--    a' <- freeze next

     -- trace (show (current, [x,y,z], dest, updates, showS (current,prev,next))) $
    pure -- $ trace (show (a :: UArray Int Int))
         -- $ trace (show (a' :: UArray Int Int))
         -- $ trace (show (current, current'))
         $ current'

makeA :: (MArray a Int m) => [Int] -> m (a Int Int)
makeA xs = do
  a <- newArray_ (1,length xs)
  let pairs = zip xs (tail xs)
  forM_ pairs $ \(x,y) -> writeArray a x y
  writeArray a (last xs) (head xs)
  pure a

test = do
  let (c,a) = runST $ do
        a <- makeA ex2
        c <- iterateN (10^7) (move3 a) (head ex2)
        a' <- unsafeFreeze a
        pure (c,a' :: UArray Int Int)
--  print (c,a)
--  print $ circleA a c
  print c

circleA :: UArray Int Int -> Int -> [Int]
circleA a c = c : loop (a I.! c)
  where loop c' | c' == c = []
                | otherwise = c' : loop (a I.! c')

iterateN :: Monad m => Int -> (a -> m a) -> a -> m a
iterateN 0 _ a = pure a
iterateN n _ _ | n `mod` 10000 == 0 && trace (show n) False = undefined
iterateN n f a = do
  a' <- f a
  iterateN (n-1) f a'

main = do
  putStrLn . result $ moves input !! 100
--  print . result2 $ moves2 (makeS ex2) !! (10^7)
  let (α,β) = runST $ do
        a <- makeA input2
        c <- iterateN (10^7) (move3 a) (head input2)
        α <- readArray a 1
        β <- readArray a α
        pure (α,β)
  print (α,β,α*β)
