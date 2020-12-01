import Control.Monad.State

import IntCode

program = "3,99,4,99,1106,0,0"
main = do
  print (runProgram program [1,2,3,4,5])


infinite1 :: [Integer]
infinite1 = snd $ execState loop (0,[])
  where loop = do
          (x,xs) <- get
          put (x+1, x:xs)
          loop

-- infinite2 :: [Integer]
-- infinite2 = snd $ execState loop (0,[])
--   where
--     loop :: (Integer, [Integer]) -> ((Integer, [Integer]), ())
--     loop = get >>= \(x,xs) -> put (x+1, x:xs) >> loop

infinite3 :: [Integer]
infinite3 = evalState loop 0
  where loop = do
          x <- get
          put (x+1)
          xs <- loop
          pure (x:xs)
  
