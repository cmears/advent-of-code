import Control.Arrow
import Data.Char
import Data.List
import Data.Maybe

main = do
  xs <- lines <$> readFile "1.txt"
  let g = uncurry (+) . first (*10) . (head &&& last) . mapMaybe numeric . tails
  mapM_ (print . sum . map g) [filter isDigit <$> xs, xs]
numeric (c:_) | isDigit c = Just (digitToInt c)
numeric s = find (\n -> (numbers !! n) `isPrefixOf` s) [0..9]
numbers = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
