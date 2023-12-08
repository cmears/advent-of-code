import qualified Data.Map as M

main = do
  (d:_:rest) <- lines <$> readFile "8.txt"
  let m = M.fromList [(a, (tail (init c), init d)) | r <- rest, let [a,_,c,d] = words r]
  let f node dir = (if dir == 'L' then fst else snd) (m M.! node)
  let walk s = zip (cycle [0..length d - 1]) $ scanl f s $ cycle d
  print $ length $ takeWhile ((/="ZZZ") . snd) (walk "AAA")
  let starts = filter ((=='A').(!!2)) (M.keys m)
  -- We assume that z nodes appear exactly at multiples of
  -- the period of the cycles.
  print $ foldl1 lcm (map (period . walk) starts)

period xs = loop M.empty (zip [0..] xs)
  where
    loop seen ((i,x):xs) =
        case M.lookup x seen of
          Nothing -> loop (M.insert x i seen) xs
          Just j -> j-i
