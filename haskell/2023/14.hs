import qualified Data.Map as M

main = do
  ls <- lines <$> readFile "14.txt"
  let ms = iterate cycle' $ M.fromList [((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l]
  print $ load $ fixpoint (tilt (-1,0)) $ head ms
  let (i,j) = detectCycle ms
  print $ load $ ms !! (i + ((1000000000-i) `mod` (j-i)))

cycle' = fixpoint (tilt (0,1)) . fixpoint (tilt (1,0)) . fixpoint (tilt (0,-1)) . fixpoint (tilt (-1,0))

tilt (dr,dc) m = foldl (f (dr,dc)) m $ M.keys $ M.filter (=='O') m

f (dr,dc) m (r,c) = case M.lookup (r+dr,c+dc) m of
                      Just '.' -> M.insert (r+dr,c+dc) 'O' . M.insert (r,c) '.' $ m
                      _ -> m

fixpoint f x = let y = f x in if x == y then x else fixpoint f y

load m = let maxR = snd $ last $ M.keys m
         in sum $ map (\(r,c) -> maxR-r+1) $ M.keys $ M.filter (=='O') m

detectCycle xs = loop (zip [0..] xs) M.empty
  where loop ((i,x):xs) seen =
          case M.lookup x seen of
            Just j -> (j,i)
            Nothing -> loop xs (M.insert x i seen)
    
