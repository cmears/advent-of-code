main = read . (\s -> "["++s++"]") <$> readFile "input7" >>= \xs -> mapM_ (\f -> print . minimum $ map (\n -> sum [f (abs (n-x)) | x <- xs]) [minimum xs .. maximum xs]) [id,\n->n*(n+1)`div`2]

