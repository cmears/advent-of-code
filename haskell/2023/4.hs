import Control.Monad.State
import Data.List
import Data.List.Split
import qualified Data.Map as M

main = do
  ls <- lines <$> readFile "4.txt"
  let cards = M.fromList $ zip [1..] (map readCard ls)
  print cards
  let n = length ls

  let b = flip execState (M.fromList [(i,1)|i<-[1..n]]) $ do
                forM_ [1..n] $ \i -> do
                  m <- get
                  let mult = m M.! i
                  let c = cards M.! i
                  let x = scoreCard c
                  forM_ [i+1..i+x] $ \j -> do
                    modify (M.adjust (+mult) j)
  print $ sum $ M.elems b


readCard :: String -> ([Int],[Int])
readCard l =
  let [_,r] = splitOn ":" l
      [a,b] = splitOn "|" r
  in (map read (words a), map read (words b))

scoreCard (winning, numbers) =
  let i = intersect winning numbers
      n = length i
  in n
