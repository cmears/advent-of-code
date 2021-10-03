import Control.Monad
import Data.List
import qualified Data.Map as M
import IntCode

maxC = 49

part1 = do
  program <- readFile "input.txt"
  let f (x,y) = head (snd (runProgram program [x,y]))
  let inputs = (,) <$> [0..maxC] <*> [0..maxC]
  let outputs = map f inputs
  print $ sum outputs
  -- let m = M.fromList $ zip inputs outputs
  -- forM_ [0..maxC] $ \y -> do
  --   forM_ [0..maxC] $ \x -> do
  --     putChar ((".#" `genericIndex` (m M.! (x,y))))
  --   putStrLn ""

part2 = do
  program <- readFile "input.txt"
  let f (x,y) = head (snd (runProgram program [x,y]))
  -- Compute the bounds of row y, given the bounds of the previous row.
      row y (pl,pr) =
          let Just l = find (\x -> f (x,y) == 1) [pl..]
              Just r = (subtract 1) <$> find (\x -> f (x,y) == 0) [max l pr..]
          in (l,r)
  let loop y prev =
          let b = row y prev
          in b : loop (y+1) b
  let bs = loop 0 (0,0)
  let blocks = zip [0..] (map (take 100) (tails bs))
  let validBlock (y,bs) =
          let (ls,rs) = unzip bs
          in minimum rs - maximum ls >= 99
  let Just (y,bs) = find validBlock blocks
  let topLeft = (maximum (map fst bs), y)
  print topLeft
  print $ fst topLeft * 10000 + snd topLeft

main = do
  part1
  part2
