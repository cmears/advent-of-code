import Data.List
import Data.Maybe

fromSnafu = sum . zipWith (*) (iterate (*5) 1) . map (subtract 2 . fromJust . (`elemIndex` "=-012")) . reverse
toSnafu = reverse . unfoldr (fmap (\n -> ("012=-" !! (n `mod` 5), (n-((n+2)`mod`5-2))`div`5)) . find (>0) . Just)
main = putStrLn . toSnafu . sum . map fromSnafu . lines =<< readFile "25.txt"
