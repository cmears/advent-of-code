import Data.Graph.Inductive
import Data.List
import Data.Maybe
import Data.Ord

main = do
  poslist <- zip [0..] . map (\l -> read ("(" ++ l ++ ")")) . lines <$> readFile "8.txt"
  let distSq (a,b,c) (x,y,z) = (abs (a-x))^2 + (abs (b-y))^2 + (abs (c-z))^2
      edges = map fst . sortBy (comparing snd) $ [ ((i,j),distSq c1 c2) | ((i,c1):r) <- tails poslist, (j,c2) <- r ]
      pairs = scanl (\(g,_) (i,j) -> (insEdge (i,j,()) g, (i,j))) (mkUGraph (map fst poslist) [] :: Gr () (), (0,0)) edges
  print . product . take 3 . reverse . sort .  map length . components $ fst $ pairs !! 1000
  let x i = let (a,_,_) = snd (poslist!!i) in a
  print . (\(i,j) -> x i * x j) . snd . fromJust $ find (isConnected . fst) pairs
