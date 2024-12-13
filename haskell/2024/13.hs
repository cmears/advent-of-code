import Data.Maybe
import Data.List.Split

main = do
  s <- readFile "13.txt"
  let machines = map parseMachine . splitOn [""] $ lines s
  print . sum . mapMaybe sol $ machines
  print . sum . mapMaybe sol $ map (\(ax,ay,bx,by,px,py) -> (ax,ay,bx,by,10000000000000+px,10000000000000+py)) machines

type Machine = (Integer,Integer,Integer,Integer,Integer,Integer)

parseMachine :: [String] -> Machine
parseMachine [a,b,p] =
    let [_,_,axs,ays] = words a
        ax = read (drop 2 (init axs))
        ay = read (drop 2 ays)
        [_,_,bxs,bys] = words b
        bx = read (drop 2 (init bxs))
        by = read (drop 2 bys)
        [_,pxs,pys] = words p
        px = read (drop 2 (init pxs))
        py = read (drop 2 pys)
    in (ax,ay,bx,by,px,py)

sol :: Machine -> Maybe Integer
sol (ax,ay,bx,by,px,py) = do
  (j,0) <- Just ((py*ax - px*ay) `quotRem` (by*ax - bx*ay))
  (i,0) <- Just ((px-j*bx) `quotRem` ax)
  pure (3*i+j)
