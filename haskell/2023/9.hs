extend xs | all (==0) xs = 0
extend xs = head xs + extend (zipWith (-) xs (tail xs))
main = do
  ls <- map (map read . words) . lines <$> readFile "9.txt"
  mapM_ (\f -> print . sum $ map (extend . f) ls) [reverse, id]
