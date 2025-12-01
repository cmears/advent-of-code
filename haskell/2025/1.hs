main = do
  xs <- map (\(x:n) -> (if (x == 'R') then id else negate) (read n)) . lines <$> readFile "1.txt"
  let xs2 = concatMap (replicate <$> abs <*> signum) xs
  mapM_ (print . length . filter (==0) . scanl (((`mod` 100) .) . (+)) 50) [xs, xs2]
