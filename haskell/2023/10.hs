import qualified Data.Map as M

main = do
  ls <- lines <$> readFile "10.txt"
  let grid = M.fromList $ do
               (r,l) <- zip [0..] ls
               (c,x) <- zip [0..] l
               pure ((r,c),x)
  let start = fst $ head $ M.toList $ M.filter (=='S') grid
  let distances = flood grid start
  print $ maximum $ M.elems distances
  print $ sum $ map (\r -> rowCount r grid distances) [0..length ls - 1]

data Inside = None | All | Upper | Lower deriving (Eq)

rowCount row grid distances =
    let m = M.filterWithKey (\(r,c) _ -> r == row) distances
        coords = M.keys m
    in if null coords then 0
       else let minc = snd (head coords)
                maxc = snd (last coords)
                loop c _ acc | c == maxc = acc
                loop c inside acc =
                    case M.member (row,c) m of
                      False -> loop (c+1) inside (if inside == All then acc+1 else acc)
                      True -> loop (c+1) (figure (grid M.! (row,c)) inside) acc
            in loop minc None 0

figure '|' None = All
figure '|' All = None
figure '-' i = i
figure 'S' i = i
figure 'L' None = Upper
figure 'L' All = Lower
figure 'F' None = Lower
figure 'F' All = Upper
figure 'J' Upper = None
figure 'J' Lower = All
figure '7' Upper = All
figure '7' Lower = None

flood :: M.Map (Int,Int) Char -> (Int,Int) -> M.Map (Int,Int) Int
flood grid coord = loop [(coord,0)] (M.empty)
  where
    loop [] m = m
    loop ((c,d):cs) m =
        case M.lookup c m of
          Just _ -> loop cs m
          Nothing ->
              let ns = map (\c' -> (c',d+1)) (neighbours c grid)
              in loop (cs++ns) (M.insert c d m)

neighbours (r,c) grid =
    case grid M.! (r,c) of
      '|' -> [(r+1,c), (r-1,c)]
      '-' -> [(r,c+1), (r,c-1)]
      'L' -> [(r-1,c), (r,c+1)]
      'J' -> [(r-1,c), (r,c-1)]
      '7' -> [(r+1,c), (r,c-1)]
      'F' -> [(r+1,c), (r,c+1)]
      'S' -> concat [
              if grid `f` (r-1,c) `elem` "|7F" then [(r-1,c)] else []
             ,if grid `f` (r+1,c) `elem` "|LJ" then [(r+1,c)] else []
             ,if grid `f` (r,c-1) `elem` "-LF" then [(r,c-1)] else []
             ,if grid `f` (r,c+1) `elem` "-7J" then [(r,c+1)] else []
             ]
  where
    g `f` coord = M.findWithDefault '*' coord g
