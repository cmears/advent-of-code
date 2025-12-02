import Data.List.Split

invalid p n = any (\(x:xs) -> all (==x) xs) [ chunksOf c s | c <- [1..(l`div`2)], l `mod` c == 0 && (p == 2 || c*2==l) ]
  where { s = show n ; l = length s }

main = do rs <- map (\s -> let [a,b] = splitOn "-" s in [read a..read b::Int]) . splitOn "," <$> readFile "2.txt"
          flip mapM_ [1,2] (\p -> (print . sum . concatMap (filter (invalid p))) rs)
