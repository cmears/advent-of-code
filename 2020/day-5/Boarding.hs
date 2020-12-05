import Data.List
import Numeric

main = do
  let f 'F' = 0
      f 'L' = 0
      f 'B' = 1
      f 'R' = 1
  let g = fst . head . readInt 2 (const True) f
  xs <- map g . lines <$> readFile "input.txt"
  let (m,n) = (minimum xs,maximum xs)
  -- Part 1
  print n
  -- Part 2
  print $ [m..n] \\ xs
  -- Alternate part 2 (linear time).
  print $ (m+n)*(n-m+1)`div`2 - sum xs
