import Data.List
import Data.List.Split

main = do
  cards <- (((((read <$>) . words) <$>) . splitOn "|" . (!!1) . splitOn ":") <$>) . lines <$> readFile "4.txt" :: IO [[[Int]]]
  let f [a,b] = length (intersect a b)
  print . sum $ ((`div`2) . (2^) . f) <$> cards
  print . sum . map head . init $ scanl (\(m:mults) c -> zipWith (+) (replicate (f c) m ++ repeat 0) mults) (repeat 1) cards
