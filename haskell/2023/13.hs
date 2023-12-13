import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Map (Map)

main = do
  c <- readFile "13.txt"
  let patterns = splitOn "\n\n" c
  print $ sum $ map (findSymmetry . parsePattern) $ patterns
  print $ sum $ map (findAlmostSymmetry . parsePattern) $ patterns

findAlmostSymmetry :: M -> Int
findAlmostSymmetry m =
    case find ((==1) . snd) (judgeHorizontalSymmetry m) of
      Just (i,1) -> 100*i
      Just _ -> error "?"
      Nothing -> case find ((==1) . snd) (judgeVerticalSymmetry m) of
                   Just (i,1) -> i
                   _ -> error "!"

findSymmetry :: M -> Int
findSymmetry m =
    case findHorizontalSymmetry m of
      Just x -> 100*x
      Nothing -> case findVerticalSymmetry m of
                   Just x -> x
                   Nothing -> error "?"

type Coord = (Int,Int)
type M = Map Coord Char

parsePattern :: String -> M
parsePattern s = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]

findHorizontalSymmetry :: M -> Maybe Int
findHorizontalSymmetry m =
    let (maxR,_) = last (M.keys m)
    in find f [1..maxR]
  where
    f r = all (\(r2,c) -> (not (M.member (r+(r-r2)-1,c) m)) || (m M.! (r2,c) == m M.! (r+(r-r2)-1,c))) (filter (\(r2,c) -> r2 < r) (M.keys m))

findVerticalSymmetry :: M -> Maybe Int
findVerticalSymmetry m =
    let (_,maxC) = last (M.keys m)
    in find f [1..maxC]
  where
    f c = all (\(r,c2) -> (not (M.member (r,c+(c-c2)-1) m)) || (m M.! (r,c2) == m M.! (r,c+(c-c2)-1))) (filter (\(r,c2) -> c2 < c) (M.keys m))

judgeHorizontalSymmetry :: M -> [(Int,Int)]
judgeHorizontalSymmetry m =
    let (maxR,_) = last (M.keys m)
    in zip [1..maxR] $ map f [1..maxR]
  where
    f r = let ks = filter (\(r2,c) -> r2 < r) (M.keys m)
              n = length ks
              matches = filter (\(r2,c) -> (not (M.member (r+(r-r2)-1,c) m)) || (m M.! (r2,c) == m M.! (r+(r-r2)-1,c))) ks
              bad = n - length matches
          in bad

judgeVerticalSymmetry :: M -> [(Int,Int)]
judgeVerticalSymmetry m = judgeHorizontalSymmetry (transposeMap m)

transposeMap m = M.fromList [ ((c,r),x) | ((r,c),x) <- M.toList m ]
