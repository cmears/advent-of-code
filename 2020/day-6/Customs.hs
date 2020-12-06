import Data.List
import Data.List.Split

main = do
  gs <- splitOn [""] . lines <$> readFile "input.txt"
  -- Part 1
  print . sum . map (length . foldl1 union) $ gs
  -- Part 2
  print . sum . map (length . foldl1 intersect) $ gs
