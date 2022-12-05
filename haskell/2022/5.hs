import Control.Monad.State
import qualified Data.Map as M

initS = M.fromList [ (1, "JFCNDBW"), (2, "TSLQVZP"), (3, "TJGBZP"), (4, "CHBZJLTD"), (5, "SJBVG"), (6, "QSP"), (7, "NPMLFDVB"), (8, "RLDBFMSP"), (9, "RTDV") ]
parseLine l = let [_,n,_,f,_,t] = words l in (read n, read f, read t)

execute :: (Int,Int,Int) -> State (M.Map Int String) ()
execute (n,f,t) = modify (\m -> let (xs,ys) = splitAt n (m M.! f) in M.insert f ys . M.update (Just . (xs++)) t $ m)

main = do
  commands <- map parseLine . lines <$> readFile "5.txt"
  let go = print . map head . M.elems . flip execState initS . mapM execute
  go (concatMap (\(n,f,t) -> replicate n (1,f,t)) commands) >> go commands

