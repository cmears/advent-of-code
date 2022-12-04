import Control.Monad
import Data.Bifunctor
import Data.List
import Data.List.Split

main = print =<< (\ps -> join bimap (length . flip filter ps . (\p [[a,b],[c,d]] -> a == c || (p == 1 && d <= b) || (p == 2 && c <= b))) (1,2)) . map (sort . map (map (read :: String -> Int) . splitOn "-") . splitOn ",") . lines <$> readFile "4.txt"
