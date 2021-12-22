import Data.List.Split

type Cube = ((Integer,Integer),(Integer,Integer),(Integer,Integer))

parse :: String -> (Bool, Cube)
parse line =
    let [onoff, range] = words line
        [x,y,z] = map (\s -> let [a,b] = splitOn ".." (drop 2 s) in (read a,read b)) (splitOn "," range)
    in (onoff == "on", (x,y,z))

main = do
  cubes <- map parse . lines <$> readFile "input22"
  let p1 = filter (\(_, (x,y,z)) -> all (\(a,b) -> -50 <= a && b <= 50) [x,y,z])
  mapM_ (\f -> print . sum . map volume . foldl process [] $ f cubes) [p1, id]

volume (x,y,z) = f x * f y * f z where f (l,u) = u-l+1

process existing (b,new) = (if b then [new] else []) ++ concatMap f existing
  where f e = case intersection3 e new of
                Just i -> filter (\sc -> not (sc `within` new)) (cleave i e)
                _ -> [e]

a `within` b = intersection3 a b == Just a

intersection3 (x1,y1,z1) (x2,y2,z2) = (,,) <$> intersection (x1,x2) <*> intersection (y1,y2) <*> intersection (z1,z2)
intersection ((l1,u1),(l2,u2)) | u1 < l2 = Nothing
                               | u2 < l1 = Nothing
                               | otherwise = Just (max l1 l2, min u1 u2)

cleave (xi,yi,zi) (x,y,z) = do
  let f (il,iu) (l,u) = filter (uncurry (<=)) [(l,il-1), (il,iu), (iu+1,u)]
  (,,) <$> f xi x <*> f yi y <*> f zi z
