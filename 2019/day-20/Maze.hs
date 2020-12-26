{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

-- The solution for part 2 imposes an arbitrary limit on the recursion
-- depth, which is incorrect in general (but gives the right answer
-- for my input).  A better may might be to compute the distances
-- between portals on the same layer, and then search over that graph.
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), viewl)
import Debug.Trace

toMap :: String -> M.Map (Int,Int) Char
toMap s = M.fromList $ do
  (r,l) <- zip [1..] (lines s)
  (c,x) <- zip [1..] l
  pure ((r,c),x)

main = do
  maze <- toMap <$> readFile "input.txt"
  let f (r,c) = M.findWithDefault ' ' (r,c) maze
      portals = do
         ((r,c),x) <- M.toList maze
         let rules = [ ((isUpper x && isUpper (f (r-1,c)) && f (r+1,c) == '.'), ((r+1,c), [f (r-1,c), x]))
                     , ((isUpper x && isUpper (f (r+1,c)) && f (r-1,c) == '.'), ((r-1,c), [x, f (r+1,c)]))
                     , ((isUpper x && isUpper (f (r,c-1)) && f (r,c+1) == '.'), ((r,c+1), [f (r,c-1), x]))
                     , ((isUpper x && isUpper (f (r,c+1)) && f (r,c-1) == '.'), ((r,c-1), [x, f (r,c+1)]))
                     ]
         case find fst rules of
           Just (_rule,result) -> pure result
           Nothing -> []
      coordToPortal = M.fromList portals
      portalToCoords = M.fromListWith (++) [ (p,[c]) | (c,p) <- portals ]
      dotCoords = [ (r,c) | ((r,c),x) <- M.toList maze, x == '.' ]
      (minR, maxR) = (minimum (map fst dotCoords), maximum (map fst dotCoords))
      (minC, maxC) = (minimum (map snd dotCoords), maximum (map snd dotCoords))
      startCoord = (\(r,c) -> (r,c,0)) $ head (portalToCoords M.! "AA")
      endCoord = (\(r,c) -> (r,c,0)) $ head (portalToCoords M.! "ZZ")
      neighbourFunc part (r,c,l) =
          let adjacents = do
                coord <- [(r+1,c,l),(r-1,c,l),(r,c+1,l),(r,c-1,l)]
                guard (f (r,c) == '.')
                pure coord
              viaPortal = do
                Just p <- pure (M.lookup (r,c) coordToPortal)
                (pr,pc) <- portalToCoords M.! p
                guard ((pr,pc) /= (r,c))
                let pl = if part == 1
                         then l
                         else if r == minR || c == minC || r == maxR || c == maxC
                              then l-1
                              else l+1
                guard (pl >= 0)
                guard (pl <= 40)
                pure (pr,pc,pl)
          in viaPortal ++ adjacents
--  mapM_ print (M.toList neighbourMap)
  let depthMap = M.fromList $ bfs (neighbourFunc 1) startCoord
  print $ snd $ depthMap M.! endCoord

  let depthMap2 = M.fromList $ bfs (neighbourFunc 2) startCoord
  print $ snd $ depthMap2 M.! endCoord
--  print endCoord

  -- let path c | c == startCoord = [c]
  --            | otherwise = c : path (fst (depthMap2 M.! c))
  -- let p = path endCoord
  -- mapM_ print p
  -- printMaze maze p


printMaze maze path = do
  let coords = M.keys maze
      (rs,cs) = unzip coords
      (minR, maxR) = (minimum rs, maximum rs)
      (minC, maxC) = (minimum cs, maximum cs)
  print $ (minR, maxR, minC, maxC)

  forM_ [minR..maxR] $ \r -> do
    forM_ [minC..maxC] $ \c -> do
      putChar (if S.member (r,c) path then '*' else pretty (maze M.! (r,c)))
    putStrLn ""

pretty '#' = ' '
pretty c = c
                       
--  print portals

bfs :: (Eq a, Ord a) => (a -> [a]) -> a -> [(a,(a,Int))]
bfs neighbourFunc start = loop (Seq.singleton (start,(start,0))) S.empty
  where
    loop (viewl -> EmptyL) visited = []
    loop (viewl -> (coord,(prev,depth)) :< q) visited =
        if S.member coord visited
        then loop q visited
        else (coord,(prev,depth)) : loop (q <> Seq.fromList (map (,(coord,depth+1)) (neighbourFunc coord))) (S.insert coord visited)
