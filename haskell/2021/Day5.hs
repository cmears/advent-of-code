import Data.Char
import qualified Data.Map as M
f c = if isDigit c then c else ' '
main = mapM_ (\b -> print . length . filter (>1) . M.elems . loop b M.empty =<< map read . words . map f <$> readFile "input5") [b1,b2]
loop b m [] = m
loop b m (x1:y1:x2:y2:xs) =
  let i = foldl (\a c -> M.insert c (1 + M.findWithDefault 0 c a) a) m
      m' = i $ if x1 == (x2::Int) then ((x1,)<$>b2 y1 y2)
               else if y1 == y2 then ((,y1)<$>b2 x1 x2)
                    else (zip (b x1 x2) (b y1 y2))
  in loop b m' xs
b1 a b = []
b2 a b = if a <= b then [a..b] else reverse [b..a]

