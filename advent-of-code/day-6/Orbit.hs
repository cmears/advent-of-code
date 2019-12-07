import Data.List.Split
import qualified Data.Map as M

parseOrbit s =
  let [a,b] = splitOn ")" s
  in (a,b)

main = do
  ls <- lines <$> readFile "input"
  let pairs = map parseOrbit ls

  let parent = M.fromList (map (\(a,b) -> (b,a)) pairs)
  let ancestors = M.fromList $ (("COM",0):) $ do
                    k <- M.keys parent
                    let v = 1 + ancestors M.! (parent M.! k)
                    pure (k,v)

  -- Part 1
  print $ sum $ M.elems ancestors

  -- Part 2
  let path x =
          x : case M.lookup x parent of
                Nothing -> []
                Just y -> path y

  let p1 = reverse $ path "YOU"
  let p2 = reverse $ path "SAN"
  let nCommon = length $ filter (uncurry (==)) (zip p1 p2)
  print (length p1 - nCommon - 1 + length p2 - nCommon - 1)
