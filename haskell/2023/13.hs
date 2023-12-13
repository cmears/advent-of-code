import Data.List
import Data.List.Split
import qualified Data.Map as M

main = do
  patterns <- splitOn "\n\n" <$> readFile "13.txt"
  mapM_ (\n -> print $ sum $ map (findAlmostSymmetry n . parsePattern) $ patterns) [0,1]

findAlmostSymmetry n m =
    case find ((==n) . snd) (judgeHorizontalSymmetry m) of
      Just (i,_) -> 100*i
      Nothing -> case find ((==n) . snd) (judgeVerticalSymmetry m) of
                   Just (i,_) -> i

parsePattern s = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]

judgeHorizontalSymmetry m =
    let (maxR,_) = last (M.keys m) in zip [1..maxR] $ map f [1..maxR]
  where
    f r = let ks = filter ((<r) . fst) (M.keys m)
              matches = filter (\(r2,c) -> (not (M.member (r+(r-r2)-1,c) m)) || (m M.! (r2,c) == m M.! (r+(r-r2)-1,c))) ks
          in length ks - length matches

judgeVerticalSymmetry = judgeHorizontalSymmetry . transposeMap

transposeMap m = M.fromList [ ((c,r),x) | ((r,c),x) <- M.toList m ]
