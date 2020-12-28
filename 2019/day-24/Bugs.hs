import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

-- Real input
input = "..###\n.####\n...#.\n.#..#\n#.###"
-- Example input
--input = "....#\n#..#.\n#..##\n..#..\n#...."

type Grid = M.Map (Int,Int) Bool

initial :: Grid
initial = M.fromList $ do
            (r,l) <- zip [1..] (lines input)
            (c,x) <- zip [1..] l
            pure ((r,c),x=='#')

step :: Grid -> Grid
step grid = M.mapWithKey f grid
  where
    f coord x =
        let n = length . filter id . map (\c -> M.findWithDefault False c grid) $ neighbours coord
        in case (x,n) of
             (True,1) -> True
             (False,1) -> True
             (False,2) -> True
             _ -> False

showGrid :: Grid -> String
showGrid = unlines . chunksOf 5 . map (\x -> if x then '#' else '.') . M.elems

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (r,c) = [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]

findRepeat :: Grid -> Grid
findRepeat = loop S.empty
  where
    loop seen grid | S.member grid seen = grid
                   | otherwise = loop (S.insert grid seen) (step grid)

biodiversity :: Grid -> Integer
biodiversity grid =
  sum . map snd . filter fst $ zip (M.elems grid) powers2

powers2 :: [Integer]
powers2 = map (2^) [0..]
