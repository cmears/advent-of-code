main = mapM_ (\n -> print . length . filter id . (\xs -> zipWith (>) (drop n xs :: [Int]) xs) . map read . lines =<< readFile "input1") [1,3]
