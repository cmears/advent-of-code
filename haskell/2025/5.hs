import Data.List
import Data.List.Split

main = do
  ls <- lines <$> readFile "5.txt"
  let [x,y] = splitOn [""] ls
      ranges = map (\s -> let [a,b] = splitOn "-" s in (read a, read b)) x
  print . length . filter (\i -> any (\(a,b) -> a <= i && i <= b) ranges) $ read <$> y
  print $ unionSize ranges

unionSize :: [(Integer, Integer)] -> Integer
unionSize ranges = loop 0 (sort ranges)
  where
    loop acc [] = acc
    loop acc ((a,b):(c,d):rs) | b >= c = loop acc ((a,max b d):rs)
    loop acc ((a,b):rs) = loop (acc+b-a+1) rs
