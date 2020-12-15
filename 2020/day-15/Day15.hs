import Data.List
import qualified Data.IntMap as M
import Debug.Trace

main = do
  let xs = go input
  print $ xs !! (2020-1)
  mapM_ (\(i,x) -> if i `mod` 1000000 == 0 then print (i,x) else pure ())  $ zip [0..] (take (31*10^6) xs)
  print $ xs !! (30000000-1)

input :: [Int]
input = [1,0,18,10,19,6]
ex :: [Int]
ex = [0,3,6]

go input = input ++ loop (length input) (last input) (M.fromList (zip (init input) [1..]))

-- t is the turn at which x was spoken.
--loop t x xs | trace (show ("loop",t,x,xs)) False = undefined
loop t x xs =
    case M.lookup x xs of
      Nothing -> 0 : loop (t+1) 0 (M.insert x t xs)
      Just i -> (t-i) : loop (t+1) (t-i) (M.insert x t xs)

-- loop t x xs =
--     case x `elemIndex` xs of
--       Nothing -> 0 : loop (t+1) 0 (x:xs)
--       Just i -> (i+1) : loop (t+1) (i+1) (x:xs)

-- [0,3,6,0,3,3,1,0,4,0,2,0,2,2,1,8,0,5,0,2]
