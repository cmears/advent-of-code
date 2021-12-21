import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M

main = do
  let input = (4,3)
  let turns = scanl (\(p1,p1s,p2,p2s) rs ->
                   let p1' = (p1 + sum rs - 1) `mod` 10 + 1
                   in (p2,p2s,p1',p1s + p1')) (fst input,0,snd input,0) (chunksOf 3 (cycle [1..100]))

  let Just (i,(_,s1,_,s2)) = find (\(_,(_,p1s,_,p2s)) -> p2s >= 1000) (zip [0..] turns)
  print $ i*3 * s1
  let (x,y) = g (fst input, snd input, 0, 0, 1)
  print $ maximum [x,y]


f (p1,p2,p1s,p2s,t) =
  foldl' (\(a,b) (c,d) -> (a+c,b+d)) (0,0) . map g $ do
        dice <- replicateM 3 [1..3]
        pure $ if t == 1
               then let p1' = (p1 + sum dice - 1) `mod` 10 + 1
                    in (p1',p2,p1s + p1',p2s,3-t)
               else let p2' = (p2 + sum dice - 1) `mod` 10 + 1
                    in (p1,p2',p1s,p2s + p2',3-t)

g (p1,p2,p1s,p2s,t) | p1s >= 21 = (1,0)
                    | p2s >= 21 = (0,1)
                    | otherwise = m M.! (p1,p2,p1s,p2s,t)

m = M.fromList [ ((p1,p2,p1s,p2s,t),f (p1,p2,p1s,p2s,t)) | p1 <- [1..10], p2 <- [1..10], p1s <- [0..20], p2s <- [0..20], t <- [1..2] ]
