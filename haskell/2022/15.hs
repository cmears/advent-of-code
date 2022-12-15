{-# LANGUAGE ViewPatterns #-}
import Data.Maybe
import Data.List
import Util
import qualified Data.Set as S

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

unify2 (x,y) [] = [(x,y)]
unify2 (x,y) ((a,b):rest) | y < a-1 = (x,y) : (a,b) : rest
                          | b < x-1 = (a,b) : unify2 (x,y) rest
                          | otherwise = unify2 (min x a, max y b) rest


unifyOne (a,b) (x,y) | b < x-1 = Nothing
                     | otherwise = Just (a,max b y)

loopLeft i l = case S.maxView l of
                 Nothing -> (i, l)
                 Just (a, l') -> case unifyOne a i of
                                   Nothing -> (i, l)
                                   Just a' -> loopLeft a' l'
loopRight i r = case S.minView r of
                  Nothing -> (i, r)
                  Just (b, r') -> case unifyOne i b of
                                    Nothing -> (i, r)
                                    Just b' -> loopRight b' r'

unifySet i s =
    let (l,r) = S.split i s
        (i2,l') = loopLeft i l
        (b,r') = loopRight i2 r
    in S.insert b (S.union l' r')

-- unifySet i s =
--     let (l,r) = S.split i s
--         (i2,l') = case S.maxView l of
--                     Nothing -> (i, l)
--                     Just (a, l') -> case unifyOne a i of
--                                       Nothing -> (i, l)
--                                       Just a' -> (a', l')
--         (b,r') = case S.minView r of
--                    Nothing -> (i2, r)
--                    Just (b, r') -> case unifyOne i2 b of
--                                      Nothing -> (i2, r)
--                                      Just b' -> (b', r')
--     in S.insert b (S.union l' r')
    
                 
--     case S.lookupGE (x,y) s of
--       Nothing -> case S.lookupLT (x,y) s of
--                    Nothing -> S.insert (x,y) s
--                    Just a -> S.delete a
    
unifyInOrder [x] = [x]
unifyInOrder ((a,b):(x,y):rest) | b < x-1 = (a,b) : unifyInOrder ((x,y):rest)
                                | otherwise = unifyInOrder ((a, max b y):rest)


main = do
  pairs <- map parseLine . lines <$> readFile "15.txt"
  let triples = [(s,b,d) | (s,b) <- pairs, let d = manhattan s b]
--  mapM_ print triples

--  let row = 2000000
--  let row = 10

  let rangesInRow1 row = foldl (flip unify) [] $ catMaybes $ map (\(s,b,d) -> coveredRange s d row) triples
  let rangesInRow2 row = foldl (flip unifySet) S.empty $ catMaybes $ map (\(s,b,d) -> coveredRange s d row) triples
  let rangesInRow3 row = unifyInOrder $ sort $ catMaybes $ map (\(s,b,d) -> coveredRange s d row) triples
  let rangesInRow4 row = foldl (flip unify2) [] $ catMaybes $ map (\(s,b,d) -> coveredRange s d row) triples

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

--  print $ take 5 $ filter (\r -> sort (rangesInRow1 r) /= rangesInRow4 r) [1..4*10^6]

  -- let r = 313968

--  mapM_ (\r -> print $ sort $ catMaybes $ map (\(s,b,d) -> coveredRange s d r) triples) (take 20 [1000000..])
  -- print $ rangesInRow1 r
  -- print $ rangesInRow2 r

  -- let Just r = find (\range -> length (rangesInRow1 range) /= 1) [1..4*10^6]
  -- print r
  -- print $ rangesInRow1 r

  -- let Just r = find (\range -> length (rangesInRow2 range) /= 1) [1..4*10^6]
  -- print r
  -- print $ rangesInRow2 r

  -- let Just r = find (\range -> length (rangesInRow3 range) /= 1) [1..4*10^6]
  -- print r
  -- print $ rangesInRow3 r

  let Just r = find (\range -> length (rangesInRow4 range) /= 1) [1..4*10^6]
  print r
  print $ rangesInRow4 r

  
