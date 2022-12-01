import Data.List
import Data.List.Split

main = do 
  elves <- map (sum . map read) . splitOn [""] . lines <$> readFile "1.txt"
  print . maximum $ elves
  print . sum . take 3 . sortOn negate $ elves
