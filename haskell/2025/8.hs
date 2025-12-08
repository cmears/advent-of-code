import Data.List
import qualified Data.Map as M
import Data.Map (Map)
import Data.Ord

main = do
  poslist <- zip [0..] . map (\l -> read ("(" ++ l ++ ")")) . lines <$> readFile "8.txt"
  let distSq (a,b,c) (x,y,z) = (abs (a-x))^2 + (abs (b-y))^2 + (abs (c-z))^2
      (edges1,edges2) = splitAt 1000 . map fst . sortBy (comparing snd) $ [ ((i,j),distSq c1 c2) | ((i,c1):r) <- tails poslist, (j,c2) <- r ]
      m = M.fromList [ (i,i) | i <- map fst poslist ]
      m2 = foldl (\mm (i,j) -> M.map (\x -> if x == mm M.! i then mm M.! j else x) mm) m edges1
  print . product . take 3 . reverse . sort . map length . group . sort . M.elems $ m2
  let loop mm ((i,j):es) =
          let mm' = M.map (\x -> if x == mm M.! i then mm M.! j else x) mm
          in if (\(x:xs) -> all (==x) xs) (M.elems mm') then (i,j) else loop mm' es
      (i,j) = loop m2 edges2
      ((x1,_,_),(x2,_,_)) = (snd (poslist !! i), snd (poslist !! j))
  print (x1*x2)
