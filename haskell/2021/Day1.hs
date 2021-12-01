import Data.List
main = do
  xs <- map read . lines <$> readFile "input1" :: IO [Int]
  print $ length . filter id . zipWith (>) (tail xs) $ xs
  let ys = map sum . takeWhile ((>=3).length) . map (take 3) . tails $ xs
  print $ length . filter id . zipWith (>) (tail ys) $ ys
