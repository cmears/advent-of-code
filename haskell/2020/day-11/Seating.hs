import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import Debug.Trace
import Prelude hiding (floor)

occupied = '#'
empty = 'L'
floor = '.'

main = do
  ls <- lines <$> readFile "input.txt"
  let r = length ls
      c = length (head ls)
      a = listArray ((1,1),(r,c)) (concat ls)

  let visionMap1 :: Array (Int,Int) [C]
      visionMap1 = array (bounds a) [ (ij, vision1 a ij) | ij <- indices a ]
      visionMap2 :: Array (Int,Int) [C]
      visionMap2 = array (bounds a) [ (ij, vision2 a ij) | ij <- indices a ]

  let nocc = length . filter (==occupied) . elems
  print (nocc (fixpoint (step visionMap1 4) a))
  print (nocc (fixpoint (step visionMap2 5) a))

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

type C = (Int,Int)
type A = UArray C Char

inBounds a (i,j) = 1 <= i && i <= r && 1 <= j && j <= c
  where ((1,1),(r,c)) = bounds a

vision1 :: A -> C -> [C]
vision1 a (i,j) = do
  (i',j') <- (,) <$> [-1,0,1] <*> [-1,0,1]
  guard (i' /= 0 || j' /= 0)
  let c = (i+i',j+j')
  guard (inBounds a c)
  pure c

vision2 :: A -> C -> [C]
vision2 a (i,j) = do
  (i',j') <- (,) <$> [-1,0,1] <*> [-1,0,1]
  guard (i' /= 0 || j' /= 0)
  let cs = tail (iterate (\(ii,jj) -> (ii+i',jj+j')) (i,j))
      cs' = dropWhile ((==floor) . (a!)) . takeWhile (inBounds a) $ cs
  take 1 cs'

step :: Array C [C] -> Int -> A -> A
step visionMap crowded a = array b [ (ix, f ix) | ix <- indices a ]
  where b@((1,1),(r,c)) = bounds a
        g (i,j) | 1 <= i && i <= r && 1 <= j && j <= c = a!(i,j)
                | otherwise = floor
        f (i,j) = let x = count a visionMap (i,j)
                      y = a!(i,j)
                  in if y == empty && x == 0 then occupied
                     else if y == occupied && x >= crowded then empty
                     else y

count :: A -> Array C [C] -> C -> Int
count a visionMap (i,j) =
    let cs = visionMap!(i,j)
    in loop cs 0
  where
    loop [] acc = acc
    loop (c:cs) acc = loop cs $ if a!c == occupied then acc+1 else acc
