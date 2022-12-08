import qualified Data.Map as M

main = do
  xs <- lines <$> readFile "8.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [1..] xs, (c,x) <- zip [1..] l ]
      coords (dr,dc) = takeWhile (flip M.member m) . tail . iterate (\(x,y)->(x+dr,y+dc))
      go a l rc = foldl1 a [ l rc ((m M.!) <$> coords d rc) | d <- [(0,1),(0,-1),(1,0),(-1,0)] ]
  let { loop1 _ [] = True ; loop1 rc (x:xs) = x < m M.! rc && loop1 rc xs
      ; loop2 _ [] = 0 ; loop2 rc (x:xs) = 1 + if x < m M.! rc then loop2 rc xs else 0 }
  print . length . filter (go (||) loop1) $ M.keys m
  print . maximum . map (go (*) loop2) $ M.keys m

