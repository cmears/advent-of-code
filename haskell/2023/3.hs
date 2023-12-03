import Control.Monad
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe

main = do
  ls <- lines <$> readFile "3.txt"
  let pairs = [ ((r,c),x) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ]
  let m = M.fromList pairs

  let hasDigit coord = maybe False isDigit (M.lookup coord m)
  let hasSymbol coord = maybe False (\c -> not (isDigit c) && c /= '.') (M.lookup coord m)

  let numbers = [ ((r,c), read x :: Int) | (r,c) <- M.keys m, Just x <- pure (number (r,c) m), not (hasDigit (r,c-1)) ]

  print $ sum $ map snd $ filter (\(coord,x) -> any hasSymbol (neighbours coord m)) numbers

  print $ sum $ do coord <- M.keys (M.filter (=='*') m)
                   case [ x | (ncoord,x) <- numbers, any (==coord) (neighbours ncoord m) ] of
                     [a,b] -> pure (a*b)
                     _ -> mzero

number :: (Int,Int) -> Map (Int,Int) Char -> Maybe String
number (r,c) m =
    case M.lookup (r,c) m of
      Just d | isDigit d -> Just (d : fromMaybe [] (number (r,c+1) m))
      _ -> Nothing

neighbours (r,c) m =
    case number (r,c) m of
      Nothing -> []
      Just x -> (,) <$> [r-1, r, r+1] <*> [c-1 .. c+length x]
