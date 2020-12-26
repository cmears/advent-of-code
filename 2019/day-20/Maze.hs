{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..), viewl)

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
      startCoord = head (portalToCoords M.! "AA")
      endCoord = head (portalToCoords M.! "ZZ")
      neighbourMap = M.fromList $ do
                       ((r,c),x) <- M.toList maze
                       guard (x == '.')
                       let adjacents = do
                                    coord <- [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]
                                    guard (f coord == '.')
                                    pure coord
                       let viaPortal = do
                                    Just p <- pure (M.lookup (r,c) coordToPortal)
                                    coord <- portalToCoords M.! p
                                    guard (coord /= (r,c))
                                    pure coord
                       pure ((r,c), viaPortal ++ adjacents)
--  mapM_ print (M.toList neighbourMap)
  let depthMap = M.fromList $ bfs neighbourMap startCoord
  print $ depthMap M.! endCoord
  print endCoord

  let path c | c == startCoord = [c]
             | otherwise = c : path (fst (depthMap M.! c))
  let p = S.fromList (path endCoord)
  printMaze maze p


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

bfs :: (Eq a, Ord a) => M.Map a [a] -> a -> [(a,(a,Int))]
bfs neighbourMap start = loop (Seq.singleton (start,(start,0))) S.empty
  where
    loop (viewl -> EmptyL) visited = []
    loop (viewl -> (coord,(prev,depth)) :< q) visited =
        if S.member coord visited
        then loop q visited
        else (coord,(prev,depth)) : loop (q <> Seq.fromList (map (,(coord,depth+1)) (neighbourMap M.! coord))) (S.insert coord visited)
        
