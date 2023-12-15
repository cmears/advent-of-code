import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M

main = do
  commands <- splitOn "," . head . lines <$> readFile "15.txt"
  print $ sum $ map hash commands
  let boxes = M.toList $ foldl execute (M.fromList [(i,[]) | i <- [0..255]]) commands
      powers = map (\(bn,lenses) -> sum (map (\(i,(l,f)) -> (1+bn) * i * f) (zip [1..] lenses))) boxes
  print $ sum powers

hash s = foldl (\x c -> ((x + ord c) * 17) `mod` 256) 0 s

execute m command | '=' `elem` command =
    let [label, focal0] = splitOn "=" command
        focal = read focal0
        box = hash label
        contents = m M.! box
    in case find ((==label).fst.snd) (zip [0..] contents) of
              Nothing -> M.insert box (contents ++ [(label,focal)]) m
              Just (i,(l,f)) -> let contents' = take i contents ++ [(label,focal)] ++ drop (i+1) contents
                                in M.insert box contents' m
execute m command =
    let label = init command
        box = hash label
        contents = m M.! box
        contents' = filter (\(l,f) -> l /= label) contents
    in M.insert box contents' m
