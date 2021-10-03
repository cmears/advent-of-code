-- Inefficient: takes about 70 seconds to run when compiled.

import Control.Monad.State
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.IO.Unsafe

type C = (Int,Int)

type Maze = M.Map C Char

-- Where are we?
-- What keys have I collected?
type S = ([C], [Char])

updateI :: Int -> a -> [a] -> [a]
updateI i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- From this state, what keys can we reach (and how far away are they)?
-- Won't return keys that are only reachable through other keys.
flood :: Maze -> S -> [(Char, [C], Int)]
flood maze (startCs, keys) = do
    (i,startC) <- zip [0..] startCs
    (k,finalC,d) <- loop [(startC,0)] S.empty
    pure (k,updateI i finalC startCs,d)
  where
    loop [] visited = []
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
    loop s = do
      cache <- get
      case M.lookup (sortS s) cache of
        Just d -> pure d
        Nothing -> do
            let moves = flood maze s
                -- For each move, calculate the best completion.
            augmentedMoves <- mapM (\(k,cs,d) -> do
                                     d' <- loop (cs, k:snd s)
                                     pure (k,cs,d,d')) moves
                -- Take the score of the best move + completion.
            let d = minimum [ d+d' | (k,cs,d,d') <- augmentedMoves ]
            modify (M.insert (sortS s) d)
            pure d

sortS (c,keys) = (c,sort keys)
       

readInput :: String -> Maze
readInput s = M.fromList $ do
                (y,l) <- zip [1..] (lines s)
                (x,a) <- zip [1..] l
                pure ((x,y),a)

go input = do
  let m = readInput input
      startCs = [ c | (c,a) <- M.toList m, a == '@' ]
      s = (startCs, "")
  print $ path m s

main = do
  go =<< readFile "input.txt"
  go =<< readFile "input2.txt"
