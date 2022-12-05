import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M

parseCommand l = let [_,n,_,f,_,t] = words l in (read n, read f, read t)
parseInput c = ( M.fromList . zip [1..] . map (filter isAlpha . concat) . transpose . map (chunksOf 4) . init $ state
               , map parseCommand commands )
  where [state, commands] = splitOn [""] (lines c)

execute m (n,f,t) = let (xs,ys) = splitAt n (m M.! f) in M.insert f ys . M.update (Just . (xs++)) t $ m

main = do
  (initS, commands) <- parseInput <$> readFile "5.txt"
  let go = print . map head . M.elems . foldl execute initS
  go (concatMap (\(n,f,t) -> replicate n (1,f,t)) commands) >> go commands
