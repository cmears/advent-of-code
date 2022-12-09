import Data.List
import Data.Maybe

move (x,y) = ([(x,y+1),(x,y-1),(x-1,y),(x+1,y)] !!) . fromJust . (`elemIndex` "UDLR") 
follow (tx,ty) (hx,hy) = (tx+signum(hx-tx)*c, ty+signum(hy-ty)*c)
  where c = max (abs (tx-hx)) (abs (ty-hy)) - 1
main = do
  commands <- concatMap (\[[d],n] -> replicate (read n) d) . map words . lines <$> readFile "9.txt"
  let tailPositions = iterate (scanl follow (0,0)) (scanl move (0,0) commands)
  let f = print . length . nub . (tailPositions !!) in f 1 *> f 9
