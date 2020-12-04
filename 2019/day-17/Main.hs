import Control.Monad
import Data.Char
import qualified Data.Map as M
import Text.Printf

import IntCode

main = do
  (ExecutionFinished, outputs) <- runFile "input" []
  let chars = map (chr . fromIntegral) outputs
  let ls = lines chars
      m = M.fromList $ do
            (r,l) <- zip [0..] ls
            (c,x) <- zip [0..] l
            pure ((r,c), x)
  let intersections = findIntersections m
      alignments = map (\(x,y) -> x*y) intersections
  draw m intersections
  print (sum alignments)
  -- print (determinePath m)

findIntersections m = M.keys (M.filterWithKey f m)
  where f (x,y) '#' = all (== (Just '#')) [ M.lookup k m | k <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)] ]
        f _ _ = False

determinePath m =
    let [((r,c),d)] = M.toList (M.filter (`elem` "^>v<") m)
    in consolidate $ determinePath' m (r,c) d

data Move = F Integer | L | R
  deriving Show

determinePath' m (r,c) d | canForward m (r,c) d = F 1 : determinePath' m (go (r,c) d) d
                         | canForward m (r,c) (turn L d) = L : determinePath' m (r,c) (turn L d)
                         | canForward m (r,c) (turn R d) = R : determinePath' m (r,c) (turn R d)
                         | otherwise = []

consolidate (F x : F y : rest) = consolidate (F (x+y) : rest)
consolidate (m:ms) = m : consolidate ms
consolidate [] = []

turn :: Move -> Char -> Char
turn L '^' = '<'
turn L '<' = 'v'
turn L 'v' = '>'
turn L '>' = '^'

turn R d = (turn L . turn L . turn L) d

go (r,c) '^' = (r-1,c)
go (r,c) 'v' = (r+1,c)
go (r,c) '>' = (r,c+1)
go (r,c) '<' = (r,c-1)

canForward m (r,c) d = M.lookup (go (r,c) d) m == Just '#'

draw m intersections = do
  let coords = M.keys m
      maxC = maximum (map snd coords)
      maxR = maximum (map fst coords)
  forM_ [0..maxR] $ \r -> do
    forM_ [0..maxC] $ \c -> do
      printf "%c" (if (r,c) `elem` intersections then 'I' else m M.! (r,c))
    putStrLn ""


-- 12345678901234567890

-- [
-- L,4,L,4,L,10,R,4,R,4,L,4,L,4,



-- R,8,R,10,

-- L,4,L,4,L,10,R,4,R,4,L,10,R,10,L,4,L,4,L,10,R,4,R,4,

--             L,10,R,10,R,4,L,4,L,4,R,8,R,10,R,4,L,10,R,10,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10]


-- L,4,R,8,R,10
-- L,4,L,4,L,10,R,4,R,4,L,4,L,4,R,6/8
