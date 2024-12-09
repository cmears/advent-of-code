import Data.Ord
import Data.List
import Data.Char
import qualified Data.Map as M
import Data.Array

calcPos pos fileid [sz] = [(fileid, fileid, sz, pos, pos+sz)]
calcPos pos fileid (sz:f:xs) = (fileid, fileid, sz, pos, pos+sz) : calcPos (pos+sz+f) (succ fileid) xs

sz_ (_,_,sz,_,_) = sz

queueHeads :: Int -> Qs -> [(Int,F)]
queueHeads maxSize queues =
  let validPairs = takeWhile (\(i,e) -> i <= maxSize) (assocs queues)
      validQueues = filter (\(i,q) -> not (null q)) validPairs
  in [(i,head q) | (i,q) <- validQueues]

getFromQueue :: Int -> Qs -> Maybe (F, Qs)
getFromQueue maxSize queues =
    case queueHeads maxSize queues of
      [] -> Nothing
      fs -> let (i,f) = maximumBy (comparing (\(i,(fid,fv,sz,st,en)) -> fid)) fs
            in Just (f, queues // [(i, tail (queues ! i))])

type F = (Int,Int,Int,Int,Int)
type Qs = Array Int [F]

loop qs m [] = []
loop qs m ((pos@(fid,fv,sz,st,en)):positions) =
    if m M.! fid == pos
    then pos : free en qs m positions
    else free en qs m positions

free freePos qs m [] = []
free freePos qs m ((pos@(fid,fv,sz,st,en)):positions) =
    let space = st - freePos
    in case getFromQueue space qs of
        Just ((fid2,fv2,sz2,st2,en2),qs') | st2 > freePos ->
            let newf = (fid2,fv2,sz2,freePos,freePos+sz2)
            in newf : free (freePos+sz2) qs' (M.insert fid2 newf m) (pos:positions)
        _ -> loop qs m (pos:positions)

main = do
  s <- readFile "9.txt"
  let pos = calcPos 0 0 (map digitToInt (head (lines s)))
  let f positions =
          let groups = map (\g -> (sz_ (head g), g)) $ groupBy (\a b -> sz_ a == sz_ b) $ sortBy (comparing sz_) $ reverse positions
              queues = listArray (1,9) (replicate 9 []) // groups
          in loop queues (M.fromList [(fid, (fid,fv,sz,st,en)) | (fid,fv,sz,st,en) <- positions]) positions
  let score (_,fv,_,st,en) = if st < en then fv*st + score (undefined,fv,undefined,st+1,en) else 0
  let flatten positions =
          let bits = [ (fv,1,i,i+1) | (_,fv,_,st,en) <- positions, i <- [st..en-1] ]
          in zipWith (\fid (a,b,c,d) -> (fid,a,b,c,d)) [0..] bits
  print $ sum $ map score (f (flatten pos))
  print $ sum $ map score (f pos)
