import Data.List.Split
import qualified Data.Set as S

-- (north, east)
f '>' = (0,1)
f '<' = (0,-1)
f '^' = (1,0)
f 'v' = (-1,0)

(a,b) !+! (c,d) = (a+c,b+d)

part1 = do
  coords <- scanl (!+!) (0,0) . map f <$> readFile "input.txt"
  let houses = S.fromList coords
  print (S.size houses)

part2 = do
  (santa,robo) <- unzip . map (\[a,b] -> (a,b)) . chunksOf 2 . map f <$> readFile "input.txt"
  let santaCoords = scanl (!+!) (0,0) santa
      roboCoords = scanl (!+!) (0,0) robo
  print . S.size . S.fromList $ santaCoords ++ roboCoords

main = part1 >> part2
