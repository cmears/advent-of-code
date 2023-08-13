import Control.Monad
import Data.Array.IArray
import Data.Ix
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

data Obj = Lumberjack | Trees | Open
  deriving (Eq, Ord)

-- (row,col), topleft = (0,0)
type Coord = (Int,Int)

type G = Array Coord Obj

main = do
  g <- readGrid "18.txt"
  let gs = iterate step g
  print $ score (gs !! 10)

  let (i,j) = findRepeat gs
  -- The same scenario appears at i and at j.
  let period = j-i
  let target = 1000000000
  let adjustedTarget = i + ((target - i) `mod` period)
  print $ score (gs !! adjustedTarget)
  pure ()

findRepeat :: (Eq a, Ord a) => [a] -> (Int,Int)
findRepeat xs = loop M.empty (zip [0..] xs)
  where loop m ((j,x):jxs) =
            case M.lookup x m of
              Nothing -> loop (M.insert x j m) jxs
              Just i -> (i,j)

score :: G -> Int
score g = 
  let objs = elems g
      nlumber = length (filter (==Lumberjack) objs)
      ntrees = length (filter (==Trees) objs)
  in nlumber * ntrees

readGrid :: String -> IO G
readGrid path = do
    ls <- lines <$> readFile path
    let rows = length ls
        cols = length (head ls)
        objs = map readObj (concat ls)
    pure $ listArray ((0,0),(rows-1,cols-1)) objs

readObj '|' = Trees
readObj '#' = Lumberjack
readObj '.' = Open

showObj Trees = '|'
showObj Lumberjack = '#'
showObj Open = '.'

printGrid :: G -> IO ()
printGrid g = do
  let ((0,0),(maxr,maxc)) = bounds g
  forM_ [0..maxr] $ \r -> do
    forM_ [0..maxc] $ \c -> do
      putChar (showObj (g ! (r,c)))
    putStrLn ""

step :: G -> G
step g = genArray (bounds g) f
  where f (r,c) = calculate (g ! (r,c)) (neighbours g (r,c))
genArray (l,u) f = listArray (l,u) $ map f $ range (l,u)

neighbours :: G -> Coord -> Counts
neighbours g (r,c) = count (catMaybes (map (g !?) (adjacent (r,c))))

adjacent (r,c) = [ (r+i,c+j) | i <- [-1,0,1], j <- [-1,0,1], i /= 0 || j /= 0 ]

g !? (r,c) =
  if inRange (bounds g) (r,c)
  then Just (g ! (r,c))
  else Nothing

data Counts = Counts { lumberjack :: Int, trees :: Int, open :: Int }

count :: [Obj] -> Counts
count = foldl' f (Counts 0 0 0)
  where f (Counts l t o) Lumberjack = Counts (l+1) t o
        f (Counts l t o) Trees = Counts l (t+1) o
        f (Counts l t o) Open = Counts l t (o+1)

calculate :: Obj -> Counts -> Obj
calculate obj (Counts l t o) =
    case obj of
      Open | t >= 3 -> Trees
           | otherwise -> Open
      Trees | l >= 3 -> Lumberjack
            | otherwise -> Trees
      Lumberjack | l >= 1 && t >= 1 -> Lumberjack
                 | otherwise -> Open
