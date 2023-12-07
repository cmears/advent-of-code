import Data.List
import Data.Ord

main = do
  pairs <- map (\l -> let [a,b] = words l in (map charToCard a, read b)) . lines <$> readFile "7.txt"
  let pairs2 = map (\(h,b) -> (map (\c -> if c == 11 then 1 else c) h, b)) pairs
  mapM_ (print . sum . zipWith (*) [1..] . map snd . sortBy (comparing (judgeHand . fst) <> compare)) [pairs, pairs2]
charToCard = head . (`elemIndices` "0123456789TJQKA")
judgeHand = f . reverse . sort . map snd . count . filter (/= 1)
  where { f [] = [5] ; f (c:cs) = (c+(5-sum (c:cs)):cs) }
count = map (\g -> (head g, length g)) . group . sort
