import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Map (Map)

main = do
  ls <- lines <$> readFile "11.txt"
  let f l = let (w:ws) = words l in [ (init w, x) | x <- ws ]
  let edges = M.fromListWith (++) [ (a,[b]) | (a,b) <- concatMap f ls ]
  let g x y = evalState (computePaths edges y x) M.empty
  print $ g "you" "out"
  print $ g "svr" "fft" * g "fft" "dac" * g "dac" "out"

computePaths :: Map String [String] -> String -> String -> State (Map String Integer) Integer
computePaths edges target source = do
  c <- get
  case M.lookup source c of
    Just x -> pure x
    Nothing ->
      if source == target then modify (M.insert source 1) >> pure 1
      else do x <- sum <$> mapM (computePaths edges target) (M.findWithDefault [] source edges)
              modify (M.insert source x) >> pure x
