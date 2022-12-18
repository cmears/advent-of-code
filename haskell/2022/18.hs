import Control.Monad.State
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

data T = Exterior | Interior | Cube deriving (Eq, Show, Ord)
type Knowledge = M.Map Coord T
type Coord = (Int,Int,Int)

neighbours :: Coord -> [Coord]
neighbours (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]

surfaces :: Coord -> State Knowledge [T]
surfaces = mapM classify . neighbours

classify :: Coord -> State Knowledge T
classify c = do
  k <- get
  case M.lookup c k of
    Just t -> pure t
    Nothing -> let (t, cs) = dfs k c
               in modify (M.union (M.fromList (zip (S.toList cs) (repeat t)))) >> pure t

oob :: Coord -> Bool
oob (x,y,z) = not (-3 <= x && x <= 23 && -3 <= y && y <= 23 && -3 <= z && z <= 23)

dfs :: Knowledge -> Coord -> (T, S.Set Coord)
dfs k origin = dfs' [origin] S.empty
  where dfs' :: [Coord] -> S.Set Coord -> (T, S.Set Coord)
        dfs' [] seen = (Interior, seen)
        dfs' (c:cs) seen | c `S.member` seen = dfs' cs seen
                         | oob c = (Exterior, seen)
                         | otherwise =
                             case M.lookup c k of
                               Just Interior -> (Interior, seen)
                               Just Exterior -> (Exterior, seen)
                               Just Cube -> dfs' cs seen
                               Nothing -> dfs' (neighbours c ++ cs) (S.insert c seen)

main = do
  coords <- map ((\[a,b,c] -> (a,b,c)) . map read . splitOn ",") . lines <$> readFile "18.txt"
  let ts = evalState (concat <$> mapM surfaces coords) (M.fromList [(c,Cube) | c <- coords])
  print . length . filter (/= Cube) $ ts
  print . length . filter (== Exterior) $ ts
