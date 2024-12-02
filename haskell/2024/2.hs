main = do
  reports <- map (map read . words) . lines <$> readFile "2.txt"
  print $ length $ filter safe reports
  print $ length $ filter safe2 reports

safe xs = (all (uncurry (<=)) pairs || all (uncurry (>=)) pairs) && all f pairs
  where pairs = zip xs (tail xs)
        f (x,y) = x /= y && abs (x-y) <= 3
sublists [] = []; sublists (x:xs) = xs : ((x:) <$> sublists xs)
safe2 xs = safe xs || any safe (sublists xs)
