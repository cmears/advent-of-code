import Control.Monad
import Data.List
import qualified Data.Map as M

main = do
  s <- readFile "10.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]
  let score (r,c) = length . nub . filter ((=='9').snd) . concat $ paths m '0' (r,c)
  let score2 (r,c) = length $ paths m '0' (r,c)
  mapM_ (\f -> print . sum . map f $ M.keys m) [score, score2]

paths m x (r,c) = do
    let y = M.findWithDefault '.' (r,c) m
    guard (x == y)
    if x == '9' then pure [((r,c),y)]
    else do (dr,dc) <- [(-1,0),(1,0),(0,1),(0,-1)]
            (((r,c),y) :) <$> paths m (succ x) (r+dr,c+dc)
