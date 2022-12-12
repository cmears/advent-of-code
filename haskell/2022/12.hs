import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe

main = do
  ls <- lines <$> readFile "12.txt"
  let m0 = M.fromList $ do
            (r,l) <- zip [0..] ls
            (c,h) <- zip [0..] l
            pure ((r,c),h)
--  print m
  let startCoord = fst $ fromJust $ find (\(coord, h) -> h == 'E') $ M.toList m0
  print startCoord
  let endCoord = fst $ fromJust $ find (\(coord, h) -> h == 'S') $ M.toList m0
  print endCoord

  let m1 :: M.Map (Int,Int) Char
      m1 = M.insert startCoord 'z' . M.insert endCoord 'a' $ m0

  let m :: M.Map (Int,Int) Int
      m = M.map (\c -> fromJust (c `elemIndex` ['a'..'z'])) m1

  let b = execState (search [(0,startCoord)] endCoord m) (M.singleton startCoord 0)
  print $ b M.! endCoord

  let lowlands = filter (\coord -> (m M.! coord) == 0) (M.keys m)
  print lowlands
  let scores = (\c -> M.lookup c b) <$> lowlands
  print $ sort scores

search :: [(Int,(Int,Int))] -> (Int,Int) -> M.Map (Int,Int) Int -> State (M.Map (Int,Int) Int) ()
search [] _ _ = pure ()
search ((_,coord):coords) endCoord m = do
  cache <- get
  let d = cache M.! coord
  let ns = neighbours coord m
  mapM_ (\n -> update n (d+1)) ns
  cache' <- get
  let changed = filter (\coord -> M.lookup coord cache /= M.lookup coord cache') (M.keys cache')
  search (sort (coords ++ (map (\n -> (d+1,n)) changed))) endCoord m

update :: (Int,Int) -> Int -> State (M.Map (Int,Int) Int) ()
update c v = do
        cache <- get
        case M.lookup c cache of
          Just x | x <= v -> pure ()
          _ -> put (M.insert c v cache)

neighbours :: (Int,Int) -> M.Map (Int,Int) Int -> [(Int,Int)]
neighbours (r,c) m = do
  let h = m M.! (r,c)
  coord <- [(r+1,c),(r-1,c),(r,c-1),(r,c+1)]
  Just h' <- pure (M.lookup coord m)
  guard (h' >= h-1)
  pure coord
