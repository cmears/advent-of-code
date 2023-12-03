import Control.Monad
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

main = do
  ls <- lines <$> readFile "3.txt"
  let pairs = [ ((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ]
  let m = M.fromList pairs
--  print pairs
--  print (number (0,1) m)
  -- print $ startpoints m
  -- print $ neighbours (0,25) m

  let numberStarts = startpoints m
      partNumberStarts = filter (\coord -> any (\co -> hasSymbol co m) (neighbours coord m)) numberStarts
  -- print partNumberStarts
  let partNumbers = [ read x :: Int | coord <- partNumberStarts, let Just x = number coord m ]
  -- print $ zip partNumberStarts partNumbers
  -- print $ sum partNumbers
  let numberPairs = zip partNumberStarts partNumbers

  let stars = M.keys (M.filter (=='*') m)
      gears = do
        (r,c) <- stars
        let adjacentNumbers = do
                ((nr,nc),x) <- numberPairs
                let n = numlen x
                let adj = any (\neigh -> neigh == (r,c)) (neighbours (nr,nc) m)
                guard adj
                pure x
        case adjacentNumbers of
          [x,y] -> pure (x*y)
          _ -> []
                                          
  print stars
  print gears
  print $ sum gears

type Coord = (Int,Int)
type M = Map Coord Char
number :: Coord -> M -> Maybe String
number (r,c) m =
    case M.lookup (r,c) m of
      Just d | isDigit d ->
                 case number (r,c+1) m of
                   Nothing -> Just [d]
                   Just x -> Just (d:x)
      _ -> Nothing

numlen :: Int -> Int
numlen = length . show

startpoints :: M -> [Coord]
startpoints m =
    let coords = M.keys m
        coords' = filter (\(r,c) -> isJust (number (r,c) m) && isNothing (number (r,c-1) m)) coords
    in coords'

neighbours :: Coord -> M -> [Coord]
neighbours (r,c) m =
    case number (r,c) m of
      Nothing -> []
      Just x ->
          let n = length x
              cs = [c-1 .. c+n]
              rs = [r-1, r, r+1]
          in (,) <$> rs <*> cs

hasSymbol :: Coord -> M -> Bool
hasSymbol coord m =
    case M.lookup coord m of
      Nothing -> False
      Just '.' -> False
      Just d | isDigit d -> False
      _ -> True
