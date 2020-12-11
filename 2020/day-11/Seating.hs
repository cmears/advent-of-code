import Control.Monad
import Data.Array.IArray
import Data.Array.Unboxed
import Debug.Trace

main = do
  ls <- lines <$> readFile "input.txt"
  let r = length ls
      c = length (head ls)
      a = listArray ((1,1),(r,c)) (concat ls)

  let nocc = length . filter (=='#') . elems
  print (nocc (fixpoint (step vision1 4) a))
  print (nocc (fixpoint (step vision2 5) a))

fixpoint :: (Eq a) => (a -> a) -> a -> a
fixpoint f a | a == a' = a
             | otherwise = fixpoint f a'
  where a' = f a

type A = UArray (Int,Int) Char

p :: A -> String
p a = unlines [ [ a!(i,j) | j <- [1..c] ] | i <- [1..r] ]
  where ((1,1),(r,c)) = bounds a

get :: A -> (Int,Int) -> Char
get a coord | inBounds a coord = a!coord
            | otherwise = '.'

inBounds a (i,j) = 1 <= i && i <= r && 1 <= j && j <= c
  where ((1,1),(r,c)) = bounds a

vision1 :: A -> (Int,Int) -> [Char]
vision1 a (i,j) = do
  (i',j') <- (,) <$> [-1,0,1] <*> [-1,0,1]
  guard (i' /= 0 || j' /= 0)
  pure (get a (i+i',j+j'))

vision2 :: A -> (Int,Int) -> [Char]
vision2 a (i,j) = do
  (i',j') <- (,) <$> [-1,0,1] <*> [-1,0,1]
  guard (i' /= 0 || j' /= 0)
  let cs = tail (iterate (\(ii,jj) -> (ii+i',jj+j')) (i,j))
      cs' = dropWhile ((=='.') . (a!)) . takeWhile (inBounds a) $ cs
      cs'' = map (a!) cs'
  take 1 cs''

step :: (A -> (Int,Int) -> [Char]) -> Int -> A -> A
step vision crowded a = array b [ (ix, f ix) | ix <- indices a ]
  where b@((1,1),(r,c)) = bounds a
        g (i,j) | 1 <= i && i <= r && 1 <= j && j <= c = a!(i,j)
                | otherwise = '.'
        f (i,j) = let x = length (filter (=='#') (vision a (i,j)))
                      y = a!(i,j)
                  in if y == 'L' && x == 0 then '#'
                     else if y == '#' && x >= crowded then 'L'
                     else y
        n (i,j) = map g [(i+i',j+j') | i' <- [-1,0,1], j' <- [-1,0,1], i' /= 0 || j' /= 0]
