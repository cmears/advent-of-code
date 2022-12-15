{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.List
import Util

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine (submatches "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" -> Just [a,b,c,d]) = ((read a,read b),(read c, read d))

manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

coveredRange (sx,sy) sensorRange targety =
  let offset = abs (targety - sy)
      radius = sensorRange - offset
  in if radius <= 0 then Nothing
     else Just (sx-radius, sx+radius)

unify (x,y) [] = [(x,y)]
unify (x,y) ((a,b):rest) | y < a-1 = (a,b) : unify (x,y) rest
                         | b < x-1 = (a,b) : unify (x,y) rest
                         | otherwise = unify (min x a, max y b) rest
    

main = do
  pairs <- map parseLine . lines <$> readFile "15.txt"
  let triples = [(s,b,d) | (s,b) <- pairs, let d = manhattan s b]
  mapM_ print triples

--  let row = 2000000
--  let row = 10

  let rangesInRow row = foldl (flip unify) [] $ catMaybes $ map (\(s,b,d) -> coveredRange s d row) triples
--  let ranges0 = 
--  mapM_ print ranges0

--  let ranges = foldl (flip unify) [] ranges0

  -- let beaconsInRow = nub $ filter (\(bx,by) -> by == row) $ map (\(s,b,d) -> b) triples

  -- let size = sum $ map (\(x,y) -> y-x+1) ranges

  -- print ranges
  -- print beaconsInRow

  -- print $ size - length beaconsInRow

  -- let ranges = map rangesInRow [1..20]
  -- mapM_ print ranges

  let Just r = find (\range -> length (rangesInRow range) /= 1) [1..4*10^6]
  print r
  print $ rangesInRow r

  
