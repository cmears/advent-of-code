import Data.List
import Data.List.Split
import Data.Either

main = do
  [rules0, updates0] <- splitOn [""] . lines <$> readFile "5.txt"
  let rules = (\[a,b] -> (a,b)) . map read . splitOn "|" <$> rules0
      updates = map read . splitOn "," <$> updates0
      f x y = if (x,y) `elem` rules then LT else if (y,x) `elem` rules then GT else EQ
      results = (\xs -> let ys = sortBy f xs in (if xs==ys then Left else Right) (ys !! (length ys `div` 2))) <$> updates
  mapM_ (print . sum) . (\(a,b) -> [a,b]) $ partitionEithers results
