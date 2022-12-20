import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.IArray
import Data.List


type Address = Int
data Cell = Cell { left :: Address, value :: Int, right :: Address }
  deriving Show

type Memory s = STArray s Int Cell


initialiseMemory :: [Int] -> ST s (Memory s)
initialiseMemory xs = do
  let n = length xs
      cells = [ Cell { left = (i-1) `mod` n, value = x, right = (i+1) `mod` n } | (i,x) <- zip [0..] xs ]
  newListArray (0, n-1) cells


-- h i j k
moveRight :: Memory s -> Address -> ST s ()
moveRight mem i = do
  ci <- readArray mem i
  let h = left ci
  ch <- readArray mem h
  let j = right ci
  cj <- readArray mem j
  let k = right cj
  ck <- readArray mem k

  -- h j i k
  writeArray mem h (ch { right = j })
  writeArray mem j (cj { left = h, right = i })
  writeArray mem i (ci { left = j, right = k })
  writeArray mem k (ck { left = i })


-- g h i j
moveLeft :: Memory s -> Address -> ST s ()
moveLeft mem i = do
  ci <- readArray mem i
  let h = left ci
  ch <- readArray mem h
  let g = left ch
  cg <- readArray mem g
  let j = right ci
  cj <- readArray mem j

  -- g i h j
  writeArray mem g (cg { right = i })
  writeArray mem i (ci { left = g, right = h })
  writeArray mem h (ch { left = i, right = j })
  writeArray mem j (cj { left = h })

ex = [1,2,-3,3,-2,0,4]

main :: IO ()
main = do
--  xs <- pure ex
  xs <- map (read :: String -> Int) . lines <$> readFile "20.txt"
  
  let n = length xs
  let a = runSTArray $ do
         mem <- initialiseMemory xs
         forM_ [0..n-1] $ \i -> do
           cell <- readArray mem i
           if value cell > 0
              then replicateM_ (value cell) (moveRight mem i)
              else replicateM_ (- (value cell)) (moveLeft mem i)
         pure mem
  
  let Just zi = 0 `elemIndex` xs
  let goRight i = right (a ! i)
      is = iterate goRight zi
  let i1000 = is !! 1000
  let i2000 = is !! 2000
  let i3000 = is !! 3000
  let x1 = value (a ! i1000)
  let x2 = value (a ! i2000)
  let x3 = value (a ! i3000)
  print (x1,x2,x3)
  print (x1+x2+x3)
