import Data.List
import Data.List.Split
import Data.Ord

main = do
  ([seedLine]:mapChunks) <- splitOn [""] . lines <$> readFile "5.txt"
  let seeds = map read (tail (words seedLine)) :: [Integer]
      mappings = map (normaliseMap . parseMap . tail) mapChunks
      ranges1 = map (\s -> (s,1)) seeds
      ranges2 = map (\[a,b] -> (a, b)) (chunksOf 2 seeds)
  mapM_ (\r -> print $ fst $ minimum $ applyMappings r mappings) [ranges1,ranges2]

applyMappings = foldl (\rs m -> concatMap (applyMapping m) rs)
applyMapping _ (_,0) = []
applyMapping sections (start,len) =
  let Just (mdest,msource,mlen) = find (\(d,s,l) -> s <= start && start < s+l) sections
      y = min (mlen - (start - msource)) len
  in (mdest + start - msource, y) : applyMapping sections (start+y,len-y)

parseMap ls = [(dest,source,len) | l <- ls, let [dest, source, len] = map read (words l)]

normaliseMap sections = loop 0 $ sortBy (comparing (\(dest,source,len) -> source)) sections
  where loop x [] = [(x, x, 10^20)]
        loop x ((d,s,len):sections) | x == s = (d,s,len) : loop (s+len) sections
                                    | x < s = (x,x,s-x) : loop s ((d,s,len):sections)
