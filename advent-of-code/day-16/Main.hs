import Control.Monad

main = do
  s <- head . lines <$> readFile "input"
  let xs = s2s s
  let c = outputs xs !! 100
  print c
  putStrLn (take 8 (concatMap show c))

  -- -- Part 2
  -- let realInput = concat (replicate 10000 xs)
  -- let offset = read (take 7 s)
  -- let os = outputs realInput
  -- forM_ os $ \o -> print (take 10 o)
  -- let o = os !! 100
  -- let m = drop offset o
  -- print m
  -- putStrLn (take 8 (concatMap show m))

s2s :: String -> [Integer]
s2s = map (read . (:[]))

outputs = iterate fft

fft :: [Integer] -> [Integer]
fft xs = map f (take (length xs) patterns)
  where f p = lastDigit (sum (zipWith (*) xs (tail p)))
        lastDigit x = abs x `mod` 10

patterns = map pattern [1..]

pattern i = cycle (concatMap (replicate i) [0, 1, 0, -1])
