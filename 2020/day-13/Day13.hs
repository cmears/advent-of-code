import Data.List
import Data.List.Split
import Data.Ord

main = do
  c <- readFile "input.txt"
  --let c = "939\n7,13,x,x,59,x,31,19\n"
  let [l1,l2] = lines c 
  let e = read l1
      bs = map read (filter (/="x") (splitOn "," l2))
  print (e :: Int)
  print (bs :: [Int])
  let bestID = minimumBy (comparing (\b -> f b e)) bs
  print bestID
  let catchingTime = f bestID e
  let waiting = catchingTime - e
  print catchingTime
  print waiting
  print $ bestID * waiting


f b e =
  if e `mod` b == 0
  then e
  else b * (e `div` b) + b

candidates = [ t | k <- [0..], let t = 13*k, (t+7)`mod`37 == 0 ]

buses = filter ((>0).snd) $ zip [0..] [13,0,0,0,0,0,0,37,0,0,0,0,0,401,0,0,0,0,0,0,0,0,0,0,0,0,0,17,0,0,0,0,19,0,0,0,23,0,0,0,0,0,29,0,613,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,41]

buses2 = filter ((>0).snd) $ zip [0..] [7,13,0,0,59,0,31,19]

go :: [(Integer,Integer)] -> (Integer,Integer)
go = foldl1 h

h :: (Integer,Integer) -> (Integer,Integer) -> (Integer,Integer)
h (t,s) (o,b) =
  let loop t' | (t'+o) `mod` b == 0 = (t',b*s)
              | otherwise = loop (t' + s)
  in loop t

-- t such that
--   t = 13*k1
--   t+7 = 37*k2
--   t+13 = 401*k3

--   t = 13*k1
--   t = 37*k2-7
--   t = 37*k2+30
--   t = 401*k3-13
--   t = 401*k3+388

--   t = 13*k1
--   t = 37*k3-7

--   13*k = 37*j-7
  
--   t = 13*k
--   t+7 `mod` 37

  -- t = 13*a = 37*b-7 = 401*c-13

  -- 13a = 37b-7
  -- 13*37a = 37*37b-259
