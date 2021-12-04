import Data.List
c n = unfoldr (\xs -> if null xs then Nothing else Just (splitAt n xs))
main = do
  (x:xs) <- words <$> readFile "input4"
  mapM_ (\f -> print (f (loop (inits (read$"["++x++"]")) (c 25 (read<$>xs))))) [head,last]
loop (d:ds) bs =
  let (w,l) = partition winner (process d<$>bs)
  in [last d * sum (filter (>0) b)|b<-w]++loop ds l
loop _ _ = []
process draws = map (\x -> if x `elem` draws then -x else x)
winner board = any (all ((<0).(board!!))) is
is = let i = c 5 [0..24] in i++transpose i
