import Data.List.Split
import qualified Data.Map as M

ex = "Game 1: 2 green, 6 blue, 7 red; 12 green, 6 blue, 3 red; 5 red, 18 green, 4 blue"
ex2 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
ex3 = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

main = do
  games <- map parseLine . lines <$> readFile "2.txt"
  let legit = filter gameCompatible games
  print $ sum $ map fst legit
  let f (_,draws) =
         let m = minForDraws draws
             power = product (M.elems m)
         in power
  print $ sum $ map f games

type Draw = [(Int,String)]
parseLine :: String -> (Int, [Draw])
parseLine line =
    let [g,r] = splitOn ":" line
        draws = splitOn ";" r
    in (read (last (words g)), map parseDraw draws)

parseDraw :: String -> [(Int, String)]
parseDraw draw =
    let colours = splitOn "," draw
    in map (\c -> let [n,col] = words c in (read n, col)) colours

gameCompatible (_, d) = all drawCompatible d

drawCompatible :: Draw -> Bool
drawCompatible draw = all colourCompatible draw

colourCompatible (n, "red") = n <= 12
colourCompatible (n, "green") = n <= 13
colourCompatible (n, "blue") = n <= 14
colourCompatible _ = False


minForDraw :: Draw -> M.Map String Int
minForDraw = M.fromList . map (\(x,y) -> (y,x))

minForDraws :: [Draw] -> M.Map String Int
minForDraws = M.unionsWith max . map minForDraw
