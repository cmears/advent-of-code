import Data.Bifunctor
import Data.List
import Data.List.Split
import Data.Maybe

data P = L [P] | I Int deriving (Show, Eq)
instance Ord P where compare = compareP

parse s = let (p, "") = parseP s in p
parseP ('[':rest) = first L $ parseList rest
parseP xs = first I . head $ reads xs
parseList (',':xs) = parseList xs
parseList (']':rest) = ([], rest)
parseList xs = uncurry first . bimap (:) parseList $ parseP xs

compareP (I x) (I y) = compare x y
compareP (L xs) (L ys) = compare xs ys
compareP (I x) (L ys) = compare (L [I x]) (L ys)
compareP (L xs) (I y) = compare (L xs) (L [I y])

main = do
  ps <- map parse . filter (not . null) . lines <$> readFile "13.txt"
  print . sum . map fst . filter (\(_,[x,y]) -> x < y) . zip [1..] $ chunksOf 2 ps
  let dividers = parse <$> ["[[2]]","[[6]]"]
  print . product $ (\d -> succ . fromJust . findIndex (==d) $ sort (dividers++ps)) <$> dividers
