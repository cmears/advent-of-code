import Data.List
import Data.Ord

main = do
  pairs <- map (\l -> let [a,b] = words l in (map (head . (`elemIndices` "0123456789TJQKA")) a, read b)) . lines <$> readFile "7.txt"
  let pairs2 = map (\(h,b) -> (map (\c -> if c == 11 then 1 else c) h, b)) pairs
  mapM_ (print . sum . zipWith (*) [1..] . map snd . sortBy (comparing (judgeHand . fst) <> compare)) [pairs, pairs2]
judgeHand = f . reverse . sort . map snd . map (\g -> (head g, length g)) . group . sort . filter (/= 1)
  where { f [] = [5] ; f (c:cs) = (c+(5-sum (c:cs)):cs) }
