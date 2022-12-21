import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Data.List

type Address = Int
type A s = STUArray s Int
data Memory s = Memory { left :: A s Address, value :: A s Int, right :: A s Address }

initialiseMemory :: [Int] -> ST s (Memory s)
initialiseMemory xs = do
  let n = length xs
      l = [ (i-1) `mod` n | i <- [0..n-1] ]
      r = [ (i+1) `mod` n | i <- [0..n-1] ]
  lm <- newListArray (0, n-1) l
  vm <- newListArray (0, n-1) xs
  rm <- newListArray (0, n-1) r
  pure (Memory lm vm rm)

-- h i j k
moveRight :: Memory s -> Address -> ST s ()
moveRight mem i = do
  h <- readArray (left mem) i
  j <- readArray (right mem) i
  k <- readArray (right mem) j

  -- h j i k
  writeArray (right mem) h j
  writeArray (left mem) j h
  writeArray (right mem) j i
  writeArray (left mem) i j
  writeArray (right mem) i k
  writeArray (left mem) k i

-- g h i j
moveLeft :: Memory s -> Address -> ST s ()
moveLeft mem i = do
  h <- readArray (left mem) i
  g <- readArray (left mem) h
  j <- readArray (right mem) i

  -- g i h j
  writeArray (right mem) g i
  writeArray (left mem) i g
  writeArray (right mem) i h
  writeArray (left mem) h i
  writeArray (right mem) h j
  writeArray (left mem) j h

main :: IO ()
main = do
  xs0 <- map (read :: String -> Int) . lines <$> readFile "20.txt"

  let go reps key = 
        let xs = map (*key) xs0
            n = length xs
            v :: Array Int Int
            r :: Array Int Int
            (v,r) = runST $ do
               mem <- initialiseMemory xs
               replicateM_ reps $ do
                 forM_ [0..n-1] $ \i -> do
                   x <- readArray (value mem) i
                   if x > 0
                      then replicateM_ (x `mod` (n-1)) (moveRight mem i)
                      else replicateM_ ((-x) `mod` (n-1)) (moveLeft mem i)
               (,) <$> freeze (value mem) <*> freeze (right mem)

            Just zi = 0 `elemIndex` xs
            is = iterate (r!) zi
        in sum (map ((v!) . (is!!)) [1000,2000,3000])

  print $ go 1 1
  print $ go 10 811589153
