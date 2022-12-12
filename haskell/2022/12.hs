import Data.List
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "12.txt"
  let m0 = M.fromList [ ((r,c),h) | (r,l) <- zip [0..] ls, (c,h) <- zip [0..] l ]
  let [startCoord, endCoord] = (\c -> fst . fromJust . find ((==c) . snd) $ M.toList m0) <$> "ES"
  let m = M.insert startCoord 'z' . M.insert endCoord 'a' $ m0
  let xs = [ (d,c) | (d,s) <- zip [0..] $ bfs (S.singleton startCoord) (neighbours m) S.empty, c <- S.toList s ]
  mapM_ (\f -> print . fst . fromJust . find (f . snd) $ xs) [(== endCoord), (=='a') . (m M.!)]

bfs coords _ _ | S.null coords = []
bfs coords f seen = coords : bfs (S.unions (S.map (S.fromList . f) coords) S.\\ seen) f (S.union coords seen)

neighbours m (r,c) = [ rc | rc <- [(r+1,c),(r-1,c),(r,c-1),(r,c+1)], maybe False (>= pred (m M.! (r,c))) (M.lookup rc m) ]
