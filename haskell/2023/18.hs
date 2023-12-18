import Data.Char
import Data.Array.IArray
import Numeric

main = do
  ls <- lines <$> readFile "18.txt"
  let answers = do
         p <- [1,2]
         let pairs = map (readPair p) ls
         let trench = dig pairs
         let n = length trench
         let a = (listArray (0,n+1) $ [last trench] ++ trench ++ [head trench]) :: Array Int (Integer,Integer)
         let { x i = snd (a ! i) ; y i = fst (a ! i) }
         let twiceArea = sum [ (y i + y (i+1)) * (x i - x (i+1)) | i <- [1..n] ]
         let boundary = sum (map snd pairs)
         pure $ (twiceArea + boundary)`div`2 + 1
  mapM_ print answers

dig = scanl (\coord (d,n) -> move d n coord) (0,0)
move "L" n (r,c) = (r,c-n)
move "R" n (r,c) = (r,c+n)
move "U" n (r,c) = (r-n,c)
move "D" n (r,c) = (r+n,c)

readPair :: Int -> String -> (String, Integer)
readPair p s =
    let [a,b,c] = words s
    in case p of
         1 -> (a, read b)
         2 -> let (x,y) = splitAt 5 (filter isHexDigit c)
              in (["RDLU" !! (read y)], fst (head (readHex x)))
