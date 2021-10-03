import Control.Applicative
import Control.Arrow
import Data.List
import Data.Maybe
import qualified Data.Map as M

part1 = print . uncurry (*) . second (+1) . (fromJust . lookup 1 &&& fromJust . lookup 3) . map (head &&& length) . group . sort . (\xs -> zipWith (-) (tail xs) xs) . (0:) . sort . map read . lines =<< readFile "input.txt"

part2 = do
  xs <- (0:) . map read . lines <$> readFile "input.txt"
  let (!) = flip (M.findWithDefault 0)
      m = M.fromList ((maximum xs + 3, 1) : [ (x, m!(x+1) + m!(x+2) + m!(x+3)) | x <- xs ])
  print (m!0)

main = part1 >> part2
