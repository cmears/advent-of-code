import Data.Array.Unboxed
import qualified Data.Map as M

main = do
  (algorithm:"":rest) <- lines <$> readFile "input20"
  let array = listArray (0, 511) (map (=='#') algorithm)
  let g = (M.fromList [ ((r,c),(x=='#')) | (r,l) <- zip [0..] rest, (c,x) <- zip [0..] l ], False)
  let xs = map (length . filter id . M.elems . fst) . iterate (step array) $ g
  print $ xs !! 2
  print $ xs !! 50

type G = (M.Map (Int,Int) Bool, Bool)
step :: UArray Int Bool -> G -> G
step array (m,d) =
  let [rmin,rmax,cmin,cmax] = ($ (M.keys m)) <$> ((\a b -> b . map a) <$> [fst,snd] <*> [minimum,maximum])
      f r c = ((r,c), (array!) . b2i . map (\x -> M.findWithDefault d x m) . vicinity $ (r,c))
      m' = M.fromList $ f <$> [rmin-2 .. rmax+2] <*> [cmin-2 .. cmax+2]
      d' = if (not d && array!0) || (d && not (array!511)) then not d else d
  in (M.filter (/=d') m',d')
  where
    b2i = foldl (\acc b -> 2*acc + (if b then 1 else 0)) 0
    vicinity (r,c) = [ (r+i,c+j) | i <- [-1,0,1], j <- [-1,0,1] ]
