import Control.Monad
import Data.Char
import qualified Data.Set as S
import Data.Array.IArray
import Numeric

main = do
  ls <- lines <$> readFile "18.txt"
  let pairs = map readPair2 ls
  let trench = S.fromList $ dig pairs
--  print $ S.size trench
  let minR = minimum $ map fst $ S.toList trench
  let maxR = maximum $ map fst $ S.toList trench
  let minC = minimum $ map snd $ S.toList trench
  let maxC = maximum $ map snd $ S.toList trench
  -- let inner = sum $ do
  --               r <- [minR..maxR]
  --               let minC = minimum $ map snd $ S.toList $ S.filter ((==r).fst) trench
  --                   maxC = maximum $ map snd $ S.toList $ S.filter ((==r).fst) trench
  --                   loop c inside acc | c == maxC = acc
  --                                     | otherwise =
  --                                         if
                                   
  --                   dug = length [ c | c <- [minC..maxC], (r,c) `S.member` trench ]
  --               pure $ maxC - minC + 1 - dug
--  print $ S.size trench + inner
  -- forM_ [minR .. maxR] $ \r -> do
  --   forM_ [minC .. maxC] $ \c -> do
  --     putChar $ if S.member (r,c) trench then '#' else '.'
  --   putChar '\n'
  -- print (minR,maxR,minC,maxC)
  -- let flood acc [] = acc
  --     flood acc ((r,c):q) | (r,c) `S.member` acc = flood acc q
  --                         | (r,c) `S.member` trench = flood acc q
  --                         | r < minR-1 = flood acc q
  --                         | r > maxR+1 = flood acc q
  --                         | c < minC-1 = flood acc q
  --                         | c > maxC+1 = flood acc q
  --                         | otherwise = flood (S.insert (r,c) acc) (neighbours (r,c) ++ q)
  -- let outer = flood S.empty [(minR-1,minC-1)]
  -- print $ ((maxR+1)-(minR-1)+1) * ((maxC+1)-(minC-1)+1) - S.size outer
  let trench2 = dig2 pairs
  let n = length trench2 :: Int
  let a = (listArray (0,n+1) $ [last trench2] ++ trench2 ++ [head trench2]) :: Array Int Coord
  let x i = snd (a ! i)
      y i = fst (a ! i)
  let twiceArea = sum [ (y i + y (i+1)) * (x i - x (i+1)) | i <- [1..n] ]
  print twiceArea

  let boundary = sum (map snd pairs)
  let twiceInterior = twiceArea - boundary + 2
  print boundary
  print (even twiceInterior)
  print (twiceInterior `div` 2)
  print $ boundary + (twiceInterior `div` 2)
  print $ twiceArea `div` 2 + (boundary`div`2) + 1
  

neighbours (r,c) = [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]

type Coord = (Integer,Integer)

dig :: [(String, Integer)] -> [Coord]
dig = loop (0,0)
  where
    loop _ [] = []
    loop (r,c) ((d,0):cs) = loop (r,c) cs
    loop (r,c) ((d,n):cs) =
        let coord' = move d (r,c)
        in coord' : loop coord' ((d,n-1):cs)

dig2 :: [(String, Integer)] -> [Coord]
dig2 = loop (0,0)
  where
    loop _ [] = []
    loop (r,c) ((d,n):cs) =
        let coord' = move2 d n (r,c)
        in coord' : loop coord' cs

move "L" (r,c) = (r,c-1)
move "R" (r,c) = (r,c+1)
move "U" (r,c) = (r-1,c)
move "D" (r,c) = (r+1,c)

move2 "L" n (r,c) = (r,c-n)
move2 "R" n (r,c) = (r,c+n)
move2 "U" n (r,c) = (r-n,c)
move2 "D" n (r,c) = (r+n,c)

readPair :: String -> (String, Integer)
readPair s =
    let (a:b:_) = words s
    in (a, read b)

readPair2 :: String -> (String, Integer)
readPair2 s =
    let [_,_,c] = words s
        c' = filter isHexDigit c
        (x,y) = splitAt 5 c'
        d = (read y) :: Int
    in (["RDLU" !! d], fst (head (readHex x)))
