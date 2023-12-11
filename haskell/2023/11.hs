import Data.List
import qualified Data.Map as M
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "11.txt"
  let m = M.fromList [((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l]
  let galaxies = M.keys $ M.filter (=='#') m
  let (maxR, maxC) = maximum (M.keys m)
  let emptyRows = S.fromList [ r | r <- [0..maxR], all (\c -> m M.! (r,c) == '.') [0..maxC] ]
  let emptyCols = S.fromList [ c | c <- [0..maxC], all (\r -> m M.! (r,c) == '.') [0..maxR] ]
  let rowSize n r = if S.member r emptyRows then n else 1
      colSize n c = if S.member c emptyCols then n else 1
  let dist2 n (x,y) (a,b) = sum (map (rowSize n) (between x a)) + sum (map (colSize n) (between y b))
  mapM_ (\n -> print $ sum $ map (uncurry (dist2 n)) [(x,y) | (x:xs) <- tails galaxies, y <- xs]) [2,10^6]
  
between x y | x > y = between y x
            | otherwise = [x+1 .. y]
