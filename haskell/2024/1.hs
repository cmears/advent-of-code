import Control.Arrow
import Data.List
main = do
  (xs,ys) <- (sort *** sort) . unzip . map ((\[a,b] -> (a,b)) . map read . words) . lines <$> readFile "1.txt"
  print . sum . map abs $ zipWith (-) xs ys
  print . sum $ map (\n -> n * length (filter (==n) ys)) xs
