import Control.Arrow
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import Data.List
import Data.Ord

main = do
  s <- readFile "12.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]
  let answer = flip evalState m $ regions m
  print $ (sum *** sum) $ unzip $ map (\(a,e) -> (a*length e,a*length (compress (sort e)))) answer

compress :: [Edge] -> [Edge]
compress (x:y:xs) =
    case (x,y) of
      (Vert d1 c1 r1, Vert d2 c2 r2) | d1 == d2 && c1 == c2 && r2 == r1+1 -> compress (y:xs)
      (Horiz d1 r1 c1, Horiz d2 r2 c2) | d1 == d2 && r1 == r2 && c2 == c1+1 -> compress (y:xs)
      _ -> x : compress (y:xs)
compress xs = xs

data Edge = Horiz Int Int Int | Vert Int Int Int deriving (Eq, Ord, Show)

processEdge ((r1,c1),(r2,c2)) =
    if r1 == r2 then Vert (c2-c1) c1 r1 else Horiz (r2-r1) r1 c1

regions origM = do
  m <- get
  case M.minViewWithKey m of
    Nothing -> pure []
    Just (((r,c),x), _) -> (:) <$> explore x (r,c) origM <*> regions origM

explore x (r,c) origM = do
  m <- get
  case M.lookup (r,c) m of
    Just y | x == y -> do
          let es = [ processEdge ((r,c),d) | d <- [(r+1,c),(r-1,c),(r,c+1),(r,c-1)], M.lookup d origM /= Just y ]
          modify (M.delete (r,c))
          (aa,ee) <- unzip <$> mapM (\d -> explore x d origM) [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]
          pure (1+sum aa, es++concat ee)
    _ -> pure (0,[])
            
  
