import Data.List (unfoldr)
import qualified Data.Map as M

parse :: [String] -> M.Map (Int,Int) Char
parse input = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] input, (c,x) <- zip [0..] l ]
main = do
  s <- parse . lines <$> readFile "input25"
  let steps = unfoldr (\s -> (\b -> (s,b)) <$> step s) s
  print $ length steps + 1

move s = M.mapWithKey f s
  where f (r,c) '.' | s M.! (left (r,c)) == '>' = '>'
        f (r,c) '>' | s M.! (right (r,c)) == '.' = '.'
        f (r,c) x = x
        left (r,0) = (r,mc)
        left (r,c) = (r,c-1)
        right (r,c) | c == mc = (r,0)
        right (r,c) = (r,c+1)
        mc = snd $ fst $ M.findMax s

transpose = M.fromList . map f . M.toList
  where f ((r,c),'>') = ((c,r),'v')
        f ((r,c),'v') = ((c,r),'>')
        f ((r,c),'.') = ((c,r),'.')

step s = let s' = (transpose . move . transpose . move) s in if s /= s' then Just s' else Nothing
