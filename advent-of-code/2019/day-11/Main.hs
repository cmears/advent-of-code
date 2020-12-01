import Control.Monad
import Data.List
import qualified Data.Map as M

import IntCode

data Dir = U | D | L | R

rotate 0 U = L
rotate 0 L = D
rotate 0 D = R
rotate 0 R = U
rotate 1 L = U
rotate 1 D = L
rotate 1 R = D
rotate 1 U = R

move U (x,y) = (x,y+1)
move D (x,y) = (x,y-1)
move L (x,y) = (x-1,y)
move R (x,y) = (x+1,y)

main = do
  program <- readFile "input"

  let (initialStatus, initialOutputs) = runProgram program []

  let loop (dir, coord, path, outputs, status, panel) =
          case outputs of
            (color:turn:rest) ->
                let path' = (coord,color):path
                    dir' = rotate turn dir
                    coord' = move dir' coord
                in loop (dir', coord', path', rest, status, M.insert coord color panel)
            [] ->
                case status of
                  ExecutionFinished -> (reverse path, panel)
                  ExecutionWaiting f ->
                      let currentColor = M.findWithDefault 0 coord panel
                          (es,os) = f [currentColor]
                      in loop (dir, coord, path, os, es, panel)

  let (path, panel) = loop (U, (0,0), [], initialOutputs, initialStatus, M.empty)

  print (length (nub (map fst path)))

  
  let (_, panel2) = loop (U, (0,0), [], initialOutputs, initialStatus, M.singleton (0,0) 1)
  drawPanel panel2

drawPanel m = do
  let coords = M.keys m
  let xs = map fst coords
      ys = map snd coords
  print (minimum xs, maximum xs)
  print (minimum ys, maximum ys)
  forM_ (reverse [ minimum ys .. maximum ys ]) $ \y -> do
    forM_ ([ minimum xs .. maximum xs ]) $ \x -> do
      let c = M.findWithDefault 0 (x,y) m
      putStr ([".", "#"] `genericIndex` c)
    putStrLn ""
  
