import Control.Monad
import Data.List.Split
import qualified Data.Map as M
import IntCode
import UI.NCurses

main = do
  program <- readFile "input"
  let (ExecutionFinished, outputs) = runProgram program []
  let screen = foldl (\m [x,y,t] -> M.insert (x,y) t m) M.empty (chunksOf 3 outputs)
  print . length . filter (==2) . M.elems $ screen

  -- Play for free
  let program2 = '2' : tail program
  let (initialStatus, outputs) = runProgram program2 []
  let initialScreen = updateScreen outputs M.empty

  let loop screen status =
          case status of
            ExecutionFinished -> pure screen
            ExecutionWaiting continuation -> do
                        printScreen screen
                        render
                        i <- readKey screen
                        w <- defaultWindow
--                        updateWindow w $ drawString (show k)
--                        render
                        let (status', outputs') = continuation [i]
                        let screen' = updateScreen outputs' screen
                        loop screen' status'

  finalScreen <- runCurses $ loop initialScreen initialStatus
  print $ finalScreen M.! (-1,0)

updateScreen outputs screen = foldl (\m [x,y,t] -> M.insert (x,y) t m) screen (chunksOf 3 outputs)

readKey screen = do
  -- w <- defaultWindow
  -- me <- getEvent w Nothing
  -- case me of
  --   Nothing -> readKey
  --   Just (EventSpecialKey KeyLeftArrow) -> pure (-1)
  --   Just (EventSpecialKey KeyRightArrow) -> pure 1
  --   Just (EventSpecialKey KeyUpArrow) -> pure 0
  --   _ -> readKey
  let (bx,_) = head [ (x,y) | ((x,y),c) <- M.toList screen, c == 4 ]
  let (px,_) = head [ (x,y) | ((x,y),c) <- M.toList screen, c == 3 ]
  pure $ if px < bx then 1 else if px > bx then -1 else 0

printScreen screen = do
  let coords = M.keys screen
      maxX = maximum (map fst coords)
      maxY = maximum (map snd coords)
  w <- defaultWindow
  updateWindow w $ do
    moveCursor 1 1
    drawString $ "score: " ++ show (screen M.! (-1,0))
    forM_ [0..maxY] $ \y -> do
      forM_ [0..maxX] $ \x -> do
        let c = case screen M.! (x,y) of
                   0 -> " "
                   1 -> "#"
                   2 -> "█"
                   3 -> "-"
                   4 -> "o"
        moveCursor (y+2) (x+2)
        drawString c
    moveCursor (maxY+3) 1

