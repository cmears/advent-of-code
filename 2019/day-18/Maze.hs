import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe

import Debug.Trace

type C = (Int,Int)

type Maze = M.Map C Char

-- Where am I?
-- What keys have I collected?
type S = (C, [Char])

-- From this state, what keys can I reach (and how far away are they)?
-- Won't return keys that are only reachable through other keys.
flood :: Maze -> S -> [(Char, C, Int)]
flood maze (startC, keys) = loop [(startC,0)] S.empty
  where
    loop [] visited = []
--    loop ((c,d):q) visited | trace (show c) False = undefined
    loop ((c,d):q) visited | c `S.member` visited = loop q visited
                           | otherwise =
      let visited' = S.insert c visited
      in case M.lookup c maze of
        Just a | isLower a && not (a `elem` keys) -> (a,c,d) : loop q visited'
        _ -> 
          let cs = availableNeighbours maze keys c
              q' = q ++ [ (c',d+1) | c' <- cs ]
          in loop q' visited'

-- What neighbours can I enter from here (given the keys I have)?
availableNeighbours :: Maze -> [Char] -> C -> [C]
availableNeighbours maze keys c = filter f (neighbours c)
  where f c' = let a = M.findWithDefault '#' c' maze
               in a `elem` ".@" || isLower a || toLower a `elem` keys

neighbours :: C -> [C]
neighbours (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

-- From here, find the length of the best completion.
path :: Maze -> S -> Int
path maze s = evalState (loop s) M.empty
  where
    nkeys = length $ filter isLower (M.elems maze)
    loop :: S -> State (M.Map S Int) Int
    loop (_,keys) | length keys == nkeys = pure 0
    loop s | trace (show (s, length (snd s), nkeys)) False = undefined
    loop s = do
      cache <- get
      case M.lookup (sortS s) cache of
        Just d -> --trace ("cached: " ++ show (d,nkeys)) $
                    pure d
        Nothing -> do
            let moves = flood maze s
                -- For each move, calculate the best completion.
            augmentedMoves <- mapM (\(k,c,d) -> do
                                     d' <- loop (c, k:snd s)
                                     pure (k,c,d,d')) moves
                -- Take the score of the best move + completion.
            let d = minimum [ d+d' | (k,c,d,d') <- augmentedMoves ]
--            trace ("RECORDING " ++ show (s,d)) $
            modify (M.insert (sortS s) d)
            pure d

sortS (c,keys) = (c,sort keys)
       

readInput :: String -> Maze
readInput s = M.fromList $ do
                (y,l) <- zip [1..] (lines s)
                (x,a) <- zip [1..] l
                pure ((x,y),a)

ex = unsafePerformIO (readFile "input.txt")
maze = readInput ex

go = do
  let m = readInput ex
      startC = head [ c | (c,a) <- M.toList m, a == '@' ]
      s = (startC, "")
  print $ flood m s
  print $ path m s

main = go
