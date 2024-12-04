import Control.Monad
import qualified Data.Map as M

ex = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX"

part1 m = do (r,c) <- M.keys m
             (dr,dc) <- [ (dr,dc) | dr <- [-1,0,1], dc <- [-1,0,1], (dr,dc) /= (0,0) ]
             guard ("XMAS" == map (\i -> M.findWithDefault '.' (r+dr*i,c+dc*i) m) [0..3])
part2 m = do (r,c) <- M.keys m
             let letters = map (\k -> M.findWithDefault '.' k m) [(r,c),(r+1,c+1),(r+2,c+2),(r,c+2),(r+2,c)]
             guard (letters `elem` ["MASMS", "MASSM", "SAMMS", "SAMSM"])

main = do
  s <- readFile "4.txt"
  let m = M.fromList [ ((r,c),x) | (r,l) <- zip [0..] (lines s), (c,x) <- zip [0..] l ]
  print $ length $ part1 m
  print $ length $ part2 m
