import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

main = do
  ls <- lines <$> readFile "3ex.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ]
  let (m1,m2) = flip execState (M.empty, M.empty) $ mapM_ updateDigit (M.toList (M.filter isDigit m))
  print $ sum $ map (m2 M.!) $ nub $ mapMaybe (m1 M.!?) $ concatMap neighbours $ M.keys (M.filter (\d -> d /= '.' && not (isDigit d)) m)
  let doStar g = case nub $ mapMaybe (m1 M.!?) (neighbours g) of { [a,b] -> Just (m2 M.! a * m2 M.! b) ; _ -> Nothing }
  print $ sum $ mapMaybe doStar (M.keys $ M.filter (=='*') m)
neighbours (r,c) = (,) <$> [r-1..r+1] <*> [c-1..c+1]
updateDigit ((r,c),d) =
  modify $ \(m1,m2) -> case M.lookup (r,c-1) m1 of
       Nothing -> (M.insert (r,c) (r,c) m1, M.insert (r,c) (digitToInt d) m2)
       Just co -> (M.insert (r,c) co m1, M.update (Just . (+digitToInt d) . (*10)) co m2)
                         
