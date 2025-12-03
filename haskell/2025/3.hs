import Data.List
import Data.Maybe

main = lines <$> readFile "3.txt" >>= \ls -> mapM_ (\n -> ((print . sum . map (read . f n)) ls)) [2,12]
f 0 xs = []
f n xs = let d = maximum (take (length xs - n+1) xs) in d : f (n-1) (drop (1 + fromJust (elemIndex d xs)) xs)
