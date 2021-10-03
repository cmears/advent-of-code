import Data.List
import qualified Data.Set as S

readLine :: String -> [String]
readLine = unfoldr f
  where
    f (c:cs) | c `elem` "ew" = Just ([c],cs)
             | otherwise = Just (splitAt 2 (c:cs))
    f [] = Nothing

readCoord :: String -> (Int,Int)
readCoord "e" = (1,0)
readCoord "w" = (-1,0)
readCoord "ne" = (0,1)
readCoord "sw" = (0,-1)
readCoord "nw" = (-1,1)
readCoord "se" = (1,-1)

(a,b) +!+ (c,d) = (a+c,b+d)

lineToCoord = foldl1 (+!+) . map readCoord . readLine

unpair (x:y:xs) | x == y = unpair xs
unpair (x:xs) = x : unpair xs
unpair [] = []

neighbours = [(0,1), (0,-1), (1,0), (-1,0), (-1,1), (1,-1)]
neighbours' c = S.fromList (map (+!+c) neighbours)

step :: S.Set (Int,Int) -> S.Set (Int,Int)
step black =
    let universe = S.unions (map neighbours' (S.toList black))
        white = universe S.\\ black
        blackF n = n == 1 || n == 2
        whiteF n = n == 2
        num c = S.size (S.intersection (neighbours' c) black)
    in S.union (S.filter (blackF . num) black)
               (S.filter (whiteF . num) white)

main = do
  tiles <- S.fromList . unpair . sort . map lineToCoord . lines <$> readFile "input.txt"
  print . S.size $ tiles
  print . S.size . (!!100) . iterate step $ tiles
