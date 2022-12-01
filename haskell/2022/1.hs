import Control.Arrow
import Data.List
import Data.List.Split

main = print =<< (head &&& sum) . take 3 . sortOn negate . map (sum . map read) . splitOn [""] . lines <$> readFile "1.txt"
