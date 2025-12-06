import Data.Char
import Data.List
import Data.List.Split

main = do
  xs <- splitWhen (all isSpace) . transpose . lines <$> readFile "6.txt"
  print . sum $ solve . transpose <$> xs
  print . sum $ solve2 <$> xs

solve xs = (if op == "*" then product else sum) (read <$> ys)
  where (op:ys) = reverse (trim <$> xs)

solve2 (y:ys) = (if last y == '*' then product else sum) $ (read . trim) <$> (init y:ys)

trim = let f = reverse . dropWhile isSpace in f . f
