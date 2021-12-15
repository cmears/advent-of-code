import qualified Data.Map as M
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "input15"
  let m1 = M.fromList [ ((r,c),read [v]) | (r,l) <- zip [0..] ls, (c,v) <- zip [0..] l ] :: M.Map (Int,Int) Int
  let n = length ls
  let m2 = M.fromList [ ((i*n+r,j*n+c),(v+i+j-1)`mod`9+1) | i <- [0..4], j <- [0..4], ((r,c),v) <- M.toList m1 ]
  let neighbours (r,c) = [ (r-1,c), (r+1,c), (r,c-1), (r,c+1) ]
  let loop m f d = case S.minView f of
                     Nothing -> d
                     Just ((_,(r,c)),f') | M.member (r,c) d -> loop m f' d
                     Just ((v,(r,c)),f') ->
                         let nf = [ (v + m M.! coord, coord) | coord <- neighbours (r,c), M.member coord m ]
                         in loop m (S.union (S.fromList nf) f') (M.insert (r,c) v d)
  mapM_ (\m -> print $ snd $ M.findMax $ loop m (S.singleton (0,(0,0))) M.empty) [m1,m2]
