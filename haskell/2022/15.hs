{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.List
import Util

parseLine :: String -> ((Int, Int), (Int, Int))
parseLine (submatches "Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)" -> Just [a,b,c,d]) = ((read a,read b),(read c, read d))

manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

coveredRange (sx,sy) sr targety | radius <= 0 = Nothing
                                | otherwise = Just (sx-radius,sx+radius)
  where radius = sr - abs (targety - sy)

ins [] (x,y) = [(x,y)]
ins ((a,b):rest) (x,y) | y < a-1 = (x,y) : (a,b) : rest
                       | b < x-1 = (a,b) : ins rest (x,y)
                       | otherwise = ins rest (min x a, max y b)

main = do
  pairs <- map parseLine . lines <$> readFile "15.txt"
  let triples = [(s,b,d) | (s,b) <- pairs, let d = manhattan s b]
  let ranges row = foldl ins [] $ mapMaybe (\(s,b,d) -> coveredRange s d row) triples
  let beacons = nub . filter ((==2*10^6).snd) $ (\(s,b,d) -> b) <$> triples
  print (sum (map (\(x,y) -> y-x+1) (ranges 2000000)) - length beacons)
  print . head $ mapMaybe (\r -> case ranges r of
                                   [_] -> Nothing
                                   [(_,a),(b,_)] -> Just (r+4000000*(a+1))) [1..4*10^6]
