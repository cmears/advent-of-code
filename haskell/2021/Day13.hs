import qualified Data.Set as S
import Control.Monad
import Data.List.Split

fold "x" n = S.map (\(x,y) -> (if x > n then n - (x-n) else x, y))
fold "y" n = S.map (\(x,y) -> (x, if y > n then n - (y-n) else y))

main = do
  [dotInput, foldInput] <- splitOn [""] . lines <$> readFile "input13"
  let dots = S.fromList $ map (\l -> let [x,y] = splitOn "," l in (read x, read y)) dotInput
      folds = map (\l -> let [d,n] = splitOn "=" (words l !! 2) in fold d (read n)) foldInput
  print . S.size . head folds $ dots
  let result = foldl (flip ($)) dots folds
  forM_ [0..S.findMax (S.map snd result)] $ \y -> do
    forM_ [0..S.findMax (S.map fst result)] $ \x -> do
      putStr (if S.member (x,y) result then "#" else " ")
    putStrLn ""
