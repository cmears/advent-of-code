import Data.List
import qualified Data.Map as M

count :: (Eq a, Ord a) => [a] -> M.Map a Int
count = M.fromList . map (\xs -> (head xs, length xs)) . group . sort

part1 = do
  counts <- count <$> readFile "input.txt"
  print $ counts M.! '(' - counts M.! ')'

part2 = do
  xs <- map (\c -> if c == '(' then 1 else -1) <$> readFile "input.txt"
  print $ find ((<0) . snd) $ zip [0..] $ scanl (+) 0 xs

main = part1 >> part2
