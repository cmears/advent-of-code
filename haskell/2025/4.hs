import Data.List
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "4.txt"
  let rolls = S.fromList [ (r,c) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l, x == '@' ]
      steps = iterate (\rs -> S.filter (not . accessibleRoll rs) rs) rolls
      Just (final:_) = find (\(x:y:_) -> S.size x == S.size y) (tails steps)
  print $ S.size rolls - S.size (steps !! 1)
  print $ S.size rolls - S.size final

neighbours (r,c) = [ (r+a,c+b) | a <- [-1,0,1], b <- [-1,0,1], a /= 0 || b /= 0 ]
accessibleRoll g = (<4) . length . filter (`S.member` g) . neighbours
