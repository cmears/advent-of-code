import qualified Data.Map as M
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "16.txt"
  let g = M.fromList [((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l]
  print $ evaluate g ((0,0),(0,1))
  let (maxR,maxC) = last (M.keys g)
  let bs = concat [
            [((0,c),(1,0)) | c <- [0..maxC]]
           ,[((maxR,c),(-1,0)) | c <- [0..maxC]]
           ,[((r,0),(0,1)) | r <- [0..maxR]]
           ,[((r,maxC),(0,-1)) | r <- [0..maxR]]
           ]
  print $ maximum $ map (evaluate g) bs

evaluate g b = length $ S.toList $ S.map fst $ loop g [b] S.empty

loop g [] s = s
loop g (b:bs) s | b `S.member` s = loop g bs s
loop g (((r,c),(dr,dc)):bs) s =
    case M.lookup (r,c) g of
      Nothing -> loop g bs s
      Just x ->
        let deltas = case (x,(dr,dc)) of
                       ('|',(0,_)) -> [(1,0),(-1,0)]
                       ('-',(_,0)) -> [(0,1),(0,-1)]
                       ('/',(0,1)) -> [(-1,0)]
                       ('/',(0,-1)) -> [(1,0)]
                       ('/',(1,0)) -> [(0,-1)]
                       ('/',(-1,0)) -> [(0,1)]
                       ('\\',(0,1)) -> [(1,0)]
                       ('\\',(0,-1)) -> [(-1,0)]
                       ('\\',(1,0)) -> [(0,1)]
                       ('\\',(-1,0)) -> [(0,-1)]
                       (_,(dr,dc)) -> [(dr,dc)]
            bs' = map (\(dr,dc) -> ((r+dr,c+dc),(dr,dc))) deltas
        in loop g (bs'++bs) (S.insert ((r,c),(dr,dc)) s)
