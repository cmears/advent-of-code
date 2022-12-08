import qualified Data.Map as M
import Control.Arrow

main = do
  xs <- lines <$> readFile "8.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [1..] xs, (c,x) <- zip [1..] l ]
      coords (dr,dc) = takeWhile (flip M.member m) . tail . iterate (\(x,y)->(x+dr,y+dc))
      go a l rc = foldl1 a [ (l . span (<m M.! rc)) ((m M.!) <$> coords d rc) | d <- [(0,1),(0,-1),(1,0),(-1,0)] ]
  print . length . filter (go (||) (null . snd)) $ M.keys m
  print . maximum . map (go (*) (length . uncurry (++) . second (take 1))) $ M.keys m
