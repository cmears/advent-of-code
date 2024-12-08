import qualified Data.Map as M
import Data.List

main = do
  s <- readFile "8.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]
      (maxR,maxC) = fst (M.findMax m)
      inBounds (r,c) = and [r >= 0, c >= 0, r <= maxR, c <= maxC]
      minus (a,b) (c,d) = (a-c,b-d); plus (a,b) (c,d) = (a+c,b+d)
  let freqs = nub $ filter (/='.') (M.elems m)
      f part = length $ nub $ do
                 f <- freqs
                 let antennae = [ k | (k,v) <- M.toList m, v==f ]
                 (x,y) <- [(a1,a2) | (a1:rest) <- tails antennae, a2 <- rest]
                 let diff = y `minus` x
                 if part == 1 then filter inBounds [y `plus` diff, x `minus` diff]
                 else takeWhile inBounds (iterate (plus diff) y) ++ takeWhile inBounds (iterate (`minus` diff) x)
  mapM_ (print . f) [1,2]

