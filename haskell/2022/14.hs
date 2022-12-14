import Data.List
import Data.List.Split
import qualified Data.Set as S
import Debug.Trace

type Coord = (Int,Int)

readPaths :: String -> [(Coord)]
readPaths s = map (\w -> (\[a,b] -> (a,b)) $ map (read :: String -> Int) (splitOn "," w)) $ filter (\w -> w /= "->") $ words s

expand (x1,y1) (x2,y2) | x1 == x2 = [(x1,y) | y <- [min y1 y2 .. max y1 y2] ]
expand (x1,y1) (x2,y2) | y1 == y2 = [(x,y1) | x <- [min x1 x2 .. max x1 x2] ]

expandPath p = zipWith expand p (tail p)

isWall walls floor coord | coord `S.member` walls = True
                         | snd coord == floor = True
                         | otherwise = False

--fallStep :: Coord -> S.Set Coord -> Coord
fallStep (x,y) walls floor =
    if not (isWall walls floor (x,y+1))
    then (x,y+1)
    else if not (isWall walls floor (x-1,y+1))
         then (x-1,y+1)
         else if not (isWall walls floor (x+1,y+1))
              then (x+1,y+1)
              else (x,y)

fall :: Coord -> S.Set Coord -> Int -> Maybe (S.Set Coord)
--fall coord walls floor | trace ("FALL  " ++ show (coord, walls, floor)) False = undefined
fall coord walls floor =
    let c' = fallStep coord walls floor
    in if c' == (500,0)
       then Nothing
       else if coord == c'
            then Just (S.insert coord walls)
            else fall c' walls floor

loop walls floor =
    case fall (500,0) walls floor of
      Just walls' -> loop walls' floor
      Nothing -> walls


main = do
  paths <- map readPaths . lines <$> readFile "14.txt"
  let points = S.fromList $ concat $ concatMap expandPath paths
--  print points
  let floor = maximum (map snd (S.toList points)) + 2
  let final = loop points floor
--  print final
  print $ S.size final - S.size points + 1

