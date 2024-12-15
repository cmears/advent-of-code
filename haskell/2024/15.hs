{-# LANGUAGE LambdaCase #-}

import Data.List

import Control.Monad
import Control.Monad.ST
import qualified Data.Array.MArray as A
import Data.Array.ST
import Data.List.Split
import Data.Array.Unboxed

type Coord = (Int,Int)
type Move = Char
type Grid = UArray Coord Char
type MGrid s = STUArray s Coord Char

main = do
  s <- readFile "15.txt"
  let ls = lines s
      [grid,moves0] = splitOn [""] ls
  let width = length (head grid)
      height = length grid
      g :: Grid
      g = array ((0,0),(height-1,width-1)) [ ((r,c),x) | (r,l) <- zip [0..] grid, (c,x) <- zip [0..] l ]
  let robot = head [ coord | (coord, x) <- assocs g, x == '@' ]
      g2 = g // [(robot, '.')]
      moves = concat moves0
  let finalGrid :: Grid
      finalGrid = runST $ do
                    mg <- thaw g2
                    finalCoord <- foldM (\coord move -> step move (coord, mg)) robot moves
                    freeze mg
  let boxCoords = [ coord | (coord,obj) <- assocs finalGrid, obj == 'O' ]
      scores = [ (r*100+c) | (r,c) <- boxCoords ]
  print $ sum scores

  let grid2 = doubleGrid grid
  let width = length (head grid2)
      height = length grid2
  let g :: Grid
      g = array ((0,0),(height-1,width-1)) [ ((r,c),x) | (r,l) <- zip [0..] grid2, (c,x) <- zip [0..] l ]
  let robot = head [ coord | (coord, x) <- assocs g, x == '@' ]
      g2 = g // [(robot, '.')]
      moves = concat moves0
  let finalGrid :: Grid
      finalGrid = runST $ do
                    mg <- thaw g2
                    finalCoord <- foldM (\coord move -> step2 move (coord, mg)) robot moves
                    freeze mg
  let boxCoords = [ coord | (coord,obj) <- assocs finalGrid, obj == '[' ]
      scores = [ (r*100+c) | (r,c) <- boxCoords ]
  print $ sum scores
      
doubleGrid :: [String] -> [String]
doubleGrid = map (concatMap f)
  where f '#' = "##"
        f 'O' = "[]"
        f '.' = ".."
        f '@' = "@."

applyMove '^' (r,c) = (r-1,c)
applyMove 'v' (r,c) = (r+1,c)
applyMove '<' (r,c) = (r,c-1)
applyMove '>' (r,c) = (r,c+1)

step :: Move -> (Coord, MGrid s) -> ST s Coord
step move (coord, g) = do
  let coord2 = applyMove move coord
  readArray g coord2 >>= \case
    '.' -> pure coord2
    '#' -> pure coord
    'O' -> findSpace move coord2 g >>= \case
             Nothing -> pure coord
             Just coord3 -> do writeArray g coord2 '.'
                               writeArray g coord3 'O'
                               pure coord2

step2 :: Move -> (Coord, MGrid s) -> ST s Coord
step2 move (coord, g) = do
  let coord2 = applyMove move coord
  readArray g coord2 >>= \case
    '.' -> pure coord2
    '#' -> pure coord
    obj | obj == '[' || obj == ']' -> do
          shiftedBoxes move coord2 g >>= \case
                 Nothing -> pure coord
                 Just bs -> do forM_ (nub (reverse bs)) $ \b -> shift move b g
                               pure coord2
    e -> error (show e)

findSpace :: Char -> Coord -> MGrid s -> ST s (Maybe Coord)
findSpace move (r,c) g = do
  obj <- readArray g (r,c)
  case obj of
    '#' -> pure Nothing
    '.' -> pure (Just (r,c))
    'O' -> do
      let coord2 = case move of
                 '^' -> (r-1,c)
                 'v' -> (r+1,c)
                 '<' -> (r,c-1)
                 '>' -> (r,c+1)
      findSpace move coord2 g
    c -> error (show c)

shiftedBoxes :: Char -> Coord -> MGrid s -> ST s (Maybe [Coord])
shiftedBoxes move (r,c) g = do
  obj <- readArray g (r,c)
  case (obj,move) of
    ('#',_) -> pure Nothing
    ('.',_) -> pure (Just [])
    ('[','>') -> shiftedBoxes move (r,c+2) g >>= \case 
                    Nothing -> pure Nothing
                    Just boxes -> pure $ Just ((r,c) : boxes)
    (']','<') -> shiftedBoxes move (r,c-2) g >>= \case 
                    Nothing -> pure Nothing
                    Just boxes -> pure $ Just ((r,c-1) : boxes)
    ('[','^') -> do one <- shiftedBoxes move (r-1,c) g
                    two <- shiftedBoxes move (r-1,c+1) g
                    case (one,two) of
                      (Just b1, Just b2) -> pure $ Just $ (r,c) : (b1 ++ b2)
                      _ -> pure Nothing
    (']','^') -> do one <- shiftedBoxes move (r-1,c) g
                    two <- shiftedBoxes move (r-1,c-1) g
                    case (one,two) of
                      (Just b1, Just b2) -> pure $ Just $ (r,c-1) : (b1 ++ b2)
                      _ -> pure Nothing
    ('[','v') -> do one <- shiftedBoxes move (r+1,c) g
                    two <- shiftedBoxes move (r+1,c+1) g
                    case (one,two) of
                      (Just b1, Just b2) -> pure $ Just $ (r,c) : (b1 ++ b2)
                      _ -> pure Nothing
    (']','v') -> do one <- shiftedBoxes move (r+1,c) g
                    two <- shiftedBoxes move (r+1,c-1) g
                    case (one,two) of
                      (Just b1, Just b2) -> pure $ Just $ (r,c-1) : (b1 ++ b2)
                      _ -> pure Nothing
    e -> do
      error (show e)

shift :: Char -> Coord -> MGrid s -> ST s ()
shift move (r,c) g = do
  let (r2,c2) = applyMove move (r,c)
  readArray g (r,c) >>= \case
    'O' -> do writeArray g (r,c) '.'
              writeArray g (r2,c2) 'O'
    '[' -> do writeArray g (r,c) '.'
              writeArray g (r,c+1) '.'
              writeArray g (r2,c2) '['
              writeArray g (r2,c2+1) ']'
