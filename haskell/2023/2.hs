import Data.List.Split
import qualified Data.Map as M

main = do
  games <- map parseLine . lines <$> readFile "2.txt"
  print $ sum $ map fst $ filter ((`lesseq` limit) . minForDraws. snd) games
  print $ sum $ map (product . M.elems . minForDraws . snd) games

a `lesseq` b = all (\c -> a M.! c <= b M.! c) ["red", "green", "blue"]
limit = M.fromList [("red",12),("green",13),("blue",14)]
parseLine line =
    let { [g,r] = splitOn ":" line ; draws = splitOn ";" r }
    in (read (last (words g)), parseDraw <$> draws)
parseDraw = M.fromList . map (\c -> let [n,col] = words c in (col, read n)) . splitOn ","
minForDraws = M.unionsWith max
