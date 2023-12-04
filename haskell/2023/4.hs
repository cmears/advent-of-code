import Data.List
import Data.List.Split

main = do
  xs <- map (\[a,b] -> length (intersect a (b :: [Int]))) . map (map (map read . words) . splitOn "|" . (!!1) . splitOn ":") . lines <$> readFile "4.txt"
  print . sum $ ((`div`2) . (2^)) <$> xs
  print . sum . map head . init $ scanl (\(m:mults) c -> zipWith (+) (replicate c m ++ repeat 0) mults) (repeat 1) xs
