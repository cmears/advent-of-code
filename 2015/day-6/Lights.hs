import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int,Int)
type Rect = (Coord,Coord)

data Instruction = On Rect
                 | Off Rect
                 | Toggle Rect
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s =
    case words s of
      ["toggle", x, "through", y] -> Toggle (parseRect x y)
      ["turn", "on", x, "through", y] -> On (parseRect x y)
      ["turn", "off", x, "through", y] -> Off (parseRect x y)

parseRect x y = read $ "(("++x++"),("++y++"))"

type Grid = S.Set Coord

rectCoords :: Rect -> S.Set Coord
rectCoords = S.fromList . rectCoords2

execute :: Instruction -> Grid -> Grid
execute (On r) grid = rectCoords r `S.union` grid
execute (Off r) grid = grid `S.difference` rectCoords r
execute (Toggle r) grid =
    let re = rectCoords r
        i = grid `S.intersection` re
    in (grid `S.difference` i) `S.union` (re `S.difference` i)

type Grid2 = M.Map Coord Int

rectCoords2 :: Rect -> [Coord]
rectCoords2 ((a,b),(c,d)) = (,) <$> [a..c] <*> [b..d]

execute2 :: Instruction -> Grid2 -> Grid2
execute2 (On r) grid = M.unionWith (+) (M.fromList [(k,1) | k <- rectCoords2 r]) grid
execute2 (Off r) grid = M.filter (>0) $ M.unionWith (+) (M.fromList [(k,-1) | k <- rectCoords2 r]) grid
execute2 (Toggle r) grid = M.unionWith (+) (M.fromList [(k,2) | k <- rectCoords2 r]) grid

main = do
  instructions <- map parseInstruction . lines <$> readFile "input.txt"
  print $ S.size $ foldl (flip execute) S.empty instructions
  print $ sum $ M.elems $ foldl (flip execute2) M.empty instructions
