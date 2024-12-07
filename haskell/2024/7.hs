main = do
  eqs <- map ((\(t:ws) -> (read (init t), read <$> ws)) . words) . lines <$> readFile "7.txt"
  let ops = [(+), (*), (\a b -> a * 10^(length (show b)) + b)]
  mapM_ (\f -> print . sum . map fst . filter (solvable (f ops)) $ eqs) [init, id]
solvable ops (target, (x:xs)) = loop x xs
  where loop acc [] = acc == target
        loop acc (y:ys) = or [loop (op acc y) ys | op <- ops]
