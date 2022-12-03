import Data.List
import Data.Maybe
import Data.List.Split

main = do
  sacks <- lines <$> readFile "3.txt"
  let p cs = fromJust (head cs `elemIndex` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ") + 1
      f1 sack = p . uncurry intersect . splitAt (length sack `div` 2) $ sack
      f2 = p . foldl1 intersect
  print . sum . map f1 $ sacks
  print . sum . map f2 . chunksOf 3 $ sacks
