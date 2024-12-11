{-# LANGUAGE LambdaCase #-}
import Control.Monad.State
import qualified Data.Map as M

evolve 0 = [1]
evolve n = if even sl then map read . (\(a,b) -> [a,b]) $ splitAt (sl `div` 2) s else [n*2024]
  where s = show n; sl = length s
score :: Int -> Int -> State (M.Map (Int,Int) Int) Int
score 0 _ = pure 1
score i n = M.lookup (i,n) <$> get >>= \case
    Just x -> pure x
    Nothing -> do scores <- mapM (score (i-1)) (evolve n)
                  let s = sum scores
                  modify (M.insert (i,n) s) >> pure s
main = do
  input <- map read . words <$> readFile "11.txt"
  mapM_ print . flip evalState M.empty $ mapM (\x -> sum <$> mapM (score x) input) [25,75]
