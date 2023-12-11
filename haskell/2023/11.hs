import Control.Monad
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "11.txt"
--  let ls2 = transpose . expand . transpose . expand $ ls
  let ls2 = ls
--  mapM_ putStrLn ls2
  let m = M.fromList [((r,c),x) | (r,l) <- zip [0..] ls2, (c,x) <- zip [0..] l]
  let galaxies = M.keys $ M.filter (=='#') m
--  print galaxies

  let maxKey = maximum (M.keys m)
      maxR = fst maxKey
      maxC = snd maxKey
  let emptyRows = S.fromList $ do
                    r <- [0..maxR]
                    guard (all (\c -> m M.! (r,c) == '.') [0..maxC])
                    pure r
  let emptyCols = S.fromList $ do
                    c <- [0..maxC]
                    guard (all (\r -> m M.! (r,c) == '.') [0..maxR])
                    pure c
  let rowSize r = if S.member r emptyRows then 1000000 else 1
      colSize c = if S.member c emptyCols then 1000000 else 1

  print emptyRows
  print emptyCols

  let dist2 (x,y) (a,b) = sum (map rowSize (between x a)) + sum (map colSize (between y b))

  let pairs = [(x,y) | (x:xs) <- tails galaxies, y <- xs]
--  print pairs
  print $ sum $ map (uncurry dist2) pairs
  

-- dist :: (Int,Int) -> (Int,Int) -> Int
-- dist (x,y) (a,b) = abs (x-a) + abs (y-b)


between x y | x > y = between y x
            | otherwise = [x+1 .. y]

-- expand = concatMap f
--   where f xs | all (=='.') xs = [xs,xs]
--              | otherwise = [xs]
