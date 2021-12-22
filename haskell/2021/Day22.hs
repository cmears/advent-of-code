import Data.List.Split

parse :: String -> (Bool, ((Integer,Integer),(Integer,Integer),(Integer,Integer)))
parse line =
    let [onoff, range] = words line
        [x,y,z] = map (\s -> let [a,b] = splitOn ".." (drop 2 s) in (read a,read b)) (splitOn "," range)
    in (onoff == "on", (x,y,z))

main = do
  cubes <- map parse . lines <$> readFile "input22"
  let p1 = filter (\(_, (x,y,z)) -> all (\(a,b) -> -50 <= a && b <= 50) [x,y,z])
  mapM_ (\f -> print . sum . map volume . processAll [] $ f cubes) [p1, id]

volume (x,y,z) = f x * f y * f z where f (l,u) = u-l+1

processAll cubes [] = cubes
processAll cubes (bcube:bcubes) =
    case process cubes bcube of
      Updated cubes' -> processAll cubes' bcubes
      Split cs bcs -> processAll cs (bcs ++ bcubes)

type Cube = ((Integer,Integer),(Integer,Integer),(Integer,Integer))
type BCube = (Bool, Cube)
data Result = Updated [Cube] | Split [Cube] [BCube]
  deriving (Show)

process :: [Cube] -> BCube -> Result
process existing (b,(x2,y2,z2)) = loop existing []
  where
    loop [] acc = if b then Updated ((x2,y2,z2):acc) else Updated acc
    loop ((x1,y1,z1):es) acc =
        case map intersection [(x1,x2),(y1,y2),(z1,z2)] of
          [Just xi, Just yi, Just zi] ->
              if b && (xi,yi,zi) == (x2,y2,z2) then Updated existing
              else if not b && (xi,yi,zi) == (x1,y1,z1) then loop es acc
                   else let ones = cleave (xi,yi,zi) (x1,y1,z1)
                            twos = cleave (xi,yi,zi) (x2,y2,z2)
                        in Split (ones++acc++es) (map (b,) twos)
          _ -> loop es ((x1,y1,z1):acc)
          
intersection ((l1,u1),(l2,u2)) | u1 < l2 = Nothing
                               | u2 < l1 = Nothing
                               | otherwise = Just (max l1 l2, min u1 u2)

cleave (xi,yi,zi) (x,y,z) = do
  let f (il,iu) (l,u) = filter (uncurry (<=)) [(l,il-1), (il,iu), (iu+1,u)]
  (,,) <$> f xi x <*> f yi y <*> f zi z
