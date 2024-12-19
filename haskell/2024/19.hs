import Data.List
import qualified Data.Map as M
import Control.Monad.State

main = do
  s <- readFile "19.txt"
  let (patterns0:_:designs) = lines s
      patterns = map (delete ',') (words patterns0)
  let vals = map (\t -> makeDesign patterns t) designs
  print . length . filter (>0) $ vals
  print . sum $ vals

makeDesign patterns target = flip evalState M.empty $ loop target
  where
    loop :: String -> State (M.Map String Integer) Integer
    loop [] = pure 1
    loop remTarget = do
      cache <- get
      case M.lookup remTarget cache of
        Just x -> pure x
        Nothing -> do let prefixes = filter (`isPrefixOf` remTarget) patterns
                      val <- sum <$> mapM loop [ drop (length p) remTarget | p <- prefixes ]
                      modify (M.insert remTarget val)
                      pure val
