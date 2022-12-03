import Data.List
import Data.Maybe
import Data.List.Split

main = do
  sacks <- lines <$> readFile "3.txt"
  let p cs = fromJust (head cs `elemIndex` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") + 1
  print . sum . map (p . uncurry intersect . (splitAt . (`div` 2) =<< length)) $ sacks
  print . sum . map (p . foldl1 intersect) . chunksOf 3 $ sacks

