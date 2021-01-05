import Data.List

nice s = threeVowels s && double s && noForbidden s
threeVowels = (>=3) . length . filter (`elem` "aeiou")
double s = or $ zipWith (==) s (tail s)
noForbidden = all (not . (`elem` ["ab","cd","pq","xy"])) . map (take 2) . tails

nice2 s = pairTwice s && aba s
pairTwice (x:y:xs) = [x,y] `isInfixOf` xs || pairTwice (y:xs)
pairTwice _ = False
aba (a:b:c:xs) = a == c || aba (b:c:xs)
aba _ = False

main = do
  strings <- lines <$> readFile "input.txt"
  print . length . filter nice $ strings
  print . length . filter nice2 $ strings
