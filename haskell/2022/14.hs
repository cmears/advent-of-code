import qualified Data.Set as S

readPaths = map (\s -> read ("("++s++")")) . filter (/= "->") . words

expandPath p = zipWith expand p (tail p)
expand (x1,y1) (x2,y2) | x1 == x2 = [ (x1,y) | y <- [min y1 y2 .. max y1 y2] ]
                       | y1 == y2 = [ (x,y1) | x <- [min x1 x2 .. max x1 x2] ]

fallPath walls floor ((x,y):path) | c' == (x,y) = (x,y):path
                                  | otherwise = fallPath walls floor (c':(x,y):path)
  where (c':_) = filter (not . (\c -> (c `S.member` walls || snd c == floor))) [(x,y+1),(x-1,y+1),(x+1,y+1),(x,y)]

restPoints _ _ [] = []
restPoints walls floor prevPath = c : restPoints (S.insert c walls) floor p
  where (c:p) = fallPath walls floor prevPath

main = do
  walls <- S.fromList . concat . concatMap (expandPath . readPaths) . lines <$> readFile "14.txt"
  let floor = 2 + S.findMax (S.map snd walls)
  mapM_ (\f -> (print . length . f) (restPoints walls floor [(500,0)])) [takeWhile ((<floor-1) . snd), id]
