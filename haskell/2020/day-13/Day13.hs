-- See the history of this file to find the version in the Youtube video.

import Control.Arrow
import Data.List
import Data.List.Split
import Data.Ord

main = do
  c <- readFile "input.txt"
  let [l1,l2] = lines c 
  let e = read l1
      bs = map read (filter (/="x") (splitOn "," l2))
  let bestID = minimumBy (comparing (f e)) bs
  let waiting = f e bestID - e
  print $ bestID * waiting

  let buses = map (second read) . filter ((/="x").snd) . zip [0..] . splitOn "," $ l2
  print $ fst $ foldl1 h buses

f e b = b * ((e+b-1)`div`b)

h :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
h (t,s) (o,b) =
  let loop t' | (t'+o) `mod` b == 0 = (t',b*s)
              | otherwise = loop (t' + s)
  in loop t
