import Control.Monad
import Data.List
import qualified Data.Map as M

data N = N (M.Map String Integer)

instance Num N where
--  fromInteger x = N (M.singleton x 1)
  (N m1) + (N m2) = N (M.unionWith (+) m1 m2)

instance Show N where
  show (N m1) = intercalate "+" [ if c == 1 then s else show c ++ s | (s,c) <- M.toList m1 ]

s2n :: String -> N
s2n s = N $ M.fromList [ ([k],1) | k <- s ]


main = do
  s <- head . lines <$> readFile "input2"
  let xs = s2s s
  let c = outputs xs !! 100
  print c
  putStrLn (take 8 (concatMap show c))

  -- Part 2
  let realInput = concat (replicate 10000 xs)
  let offset = read (take 7 s)
  let os = outputs realInput
  forM_ os $ \o -> print (take 10 o)
  let o = os !! 100
  let m = drop offset o
  print m
  putStrLn (take 8 (concatMap show m))

s2s :: String -> [Integer]
s2s = map (read . (:[]))

outputs = iterate fft

fft :: Num a => [a] -> [a]
fft xs = map f (take (length xs) patterns)
  where f p = lastDigit (sum (zipWith (*) xs (tail p)))
        lastDigit x = abs x `mod` 10

patterns :: Num a => [[a]]
patterns = map pattern [1..]

pattern i = cycle (concatMap (replicate i) [0, 1, 0, -1])
