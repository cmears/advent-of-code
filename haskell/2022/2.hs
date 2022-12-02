import Data.List
import Data.Maybe

rules = [ (0,0,1), (0,1,2), (0,2,0), (1,0,0), (1,1,1), (1,2,2), (2,0,2), (2,1,0), (2,2,1) ]
f1 (x,y) (a,b,c) = a==x && b==y
f2 (x,y) (a,b,c) = a==x && c==y
go f = sum . map (\(x,y) -> let (a,b,c) = fromJust $ find (f (x,y)) rules in (b+1+c*3))
main = do
  let conv c = (fromJust (c `elemIndex` "ABCXYZ")) `mod` 3
  hints <- map ((\[[a],[b]] -> (a,b)) . map (map conv) . words) . lines <$> readFile "2.txt"
  print (go f1 hints, go f2 hints)
