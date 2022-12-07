{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as M
import Data.List

execute (cwd,fs) (words -> ["$", "cd", ".."]) = (tail cwd,fs)
execute (cwd,fs) (words -> ["$", "cd", "/"]) = ([],fs)
execute (cwd,fs) (words -> ["$", "cd", d]) = (d:cwd,fs)
execute (cwd,fs) (words -> ["$", "ls"]) = (cwd,fs)
execute (cwd,fs) (words -> ["dir", _]) = (cwd,fs)
execute (cwd,fs) (words -> [sz, f]) = (cwd,M.insert (f:cwd) (read sz) fs)

dirSize d = sum . map snd . filter ((d `isSuffixOf`) . fst) . M.toList
main = do
  fs <- snd . foldl execute ([],M.empty) . lines <$> readFile "7.txt"
  let sizes = map (flip dirSize fs) . nub . concatMap (tails . tail) $ M.keys fs
  print . sum . filter (<=10^5) $ sizes
  print . minimum . filter (>=(dirSize [] (fs) - 40*10^6)) $ sizes
