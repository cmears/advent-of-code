import Control.Monad
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.STRef
import Data.Ord
import Data.List

data Block = File (Int,Int,Int) | Free (Int,Int) deriving (Show)

getPos (File (_,_,p)) = p
getPos (Free (_,p)) = p

parse s = loop 0 0 (map digitToInt (head (lines s)))
  where loop i pos (d:f:rest) = File (i,d,pos) : Free (f,pos+d) : loop (succ i) (pos+d+f) rest
        loop i pos [d] = [File (i,d,pos)]

main = do
  s <- readFile "9.txt"
  let vec = V.fromList (parse s)
  let (vec2, moved) = process vec
  let combined = sortBy (comparing getPos) (V.toList vec2 ++ moved)
  print $ sum $ map p2 combined
        where p2 (Free _) = 0
              p2 (File (fid,sz,pos)) = (fid * sz * (2*pos + sz - 1)) `div` 2

process :: V.Vector Block -> (V.Vector Block, [Block])
process vec = runST $ do
  mvec <- V.thaw vec
  moved <- newSTRef []
  cursors <- MV.replicate 10 0
  let n = V.length vec
  forM_ [n-1,n-2..0] $ \i -> do
    vi <- mvec `MV.read` i
    case vi of
      Free _ -> pure ()
      File (fid,sz,pos) -> do
                let loop j | j > i = pure ()
                           | j == i = MV.write cursors sz j
                    loop j = do
                        vj <- mvec `MV.read` j
                        case vj of
                          Free (f,fp) | sz <= f -> do
                                  let free' = Free (f-sz, fp+sz)
                                  MV.write mvec j free'
                                  MV.write mvec i (Free (sz,pos))
                                  let vi' = File (fid,sz,fp)
                                  modifySTRef moved (vi':)
                                  MV.write cursors sz j
                          _ -> loop (j+1)
                cursor <- cursors `MV.read` sz
                loop cursor
  (,) <$> V.freeze mvec <*> readSTRef moved
