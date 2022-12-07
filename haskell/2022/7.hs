import qualified Data.Map as M
import Data.List

execute (cwd,fs) ["$", "cd", ".."] = (tail cwd,fs)
execute (cwd,fs) ["$", "cd", "/"] = ([],fs)
execute (cwd,fs) ["$", "cd", d] = (d:cwd,fs)
execute (cwd,fs) ["$", "ls"] = (cwd,fs)
execute (cwd,fs) ["dir", _] = (cwd,fs)
execute (cwd,fs) [sz, _] = (cwd,M.insertWith (+) cwd (read sz) fs)

main = do
  fs <- snd . foldl execute ([],M.empty) . map words . lines <$> readFile "7.txt"
  let dirSize d = sum . map snd . filter ((d `isSuffixOf`) . fst) $ M.toList fs
  let sizes = map dirSize . nub . concatMap tails $ M.keys fs
  print . sum . filter (<=10^5) $ sizes
  print . minimum . filter (>=(dirSize [] - 40*10^6)) $ sizes
