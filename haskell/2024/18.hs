import Control.Monad
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Array.MArray
import Data.Array.IO

main = do
  s <- readFile "18.txt"
  let coords = map (\l -> read ("(" ++ l ++ ")")) (lines s)
  a <- newArray ((0,0),(70,70)) '.' :: IO (IOUArray (Int,Int) Char)
  mapM_ (\c -> writeArray a c '#') (take 1024 coords)
  v <- bfs' [(0,0)] a
  print $ fst $ v M.! (70,70)
  breakingPoint <- findBreak a (findPath v (70,70)) (drop 1024 coords)
  print breakingPoint

findBreak a0 pathSet coords = loop a0 pathSet coords
  where
    loop a pathSet (coord:ps) = do
      writeArray a coord '#'
      if S.member coord pathSet
        then do v <- bfs' [(0,0)] a
                if M.member (70,70) v
                  then loop a (findPath v (70,70)) ps
                  else pure coord
        else loop a pathSet ps

findPath v = S.fromList . loop
  where loop n = case v M.! n of
                   (_,p) | p == n -> [n]
                         | otherwise -> n : loop p

bfs' nodes a = loop M.empty (Seq.fromList [(0,node,node)|node<-nodes])
  where
    loop visited q =
        case Seq.viewl q of
          Seq.EmptyL -> pure visited
          (cost,node,predecessor) Seq.:< queue ->
            case M.lookup node visited of
              Just c -> loop visited queue
              _ -> do neighs <- neighbours' a node
                      let ns = [(cost+c,n,node) | (n,c) <- neighs]
                      loop (M.insert node (cost,predecessor) visited) (foldl (Seq.|>) queue ns)

neighbours' a (x,y) = do
  bounds <- getBounds a
  let coords = filter (inRange bounds) [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
  map (\c -> (c,1)) <$> filterM (\c -> readArray a c >>= (\x -> pure (x == '.'))) coords


