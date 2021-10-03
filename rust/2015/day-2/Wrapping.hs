{-# LANGUAGE ViewPatterns #-}
import Data.List
import Text.Regex.TDFA

-- e.g.
--   submatches "([0-9]+): ([0-9]+)" "123: 456"
-- returns ["123", "456"]
submatches :: String -> String -> Maybe [String]
submatches regex s =
  case getAllTextSubmatches (s =~ regex) of
    [] -> Nothing
    matches -> Just (tail matches)

parseLine :: String -> (Int,Int,Int)
parseLine (submatches "([0-9]+)x([0-9]+)x([0-9]+)" -> Just [a,b,c]) = (read a, read b, read c)

paper (a,b,c) = 2*(a*b + a*c + b*c) + x*y
  where [x,y,_] = sort [a,b,c]

ribbon (a,b,c) = 2*(x+y) + a*b*c
  where [x,y,_] = sort [a,b,c]

main = do
  boxes <- map parseLine . lines <$> readFile "input.txt"
  print . sum . map paper $ boxes
  print . sum . map ribbon $ boxes
