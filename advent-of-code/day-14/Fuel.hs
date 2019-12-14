import Data.Function
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Debug.Trace

s = "9 GJSNW, 9 GHJHK => 6 DRKW"

type Ingredient = (Integer, String)
type Recipe = ([Ingredient], Ingredient)

parseLine :: String -> Recipe
parseLine s =
    let [inputs, output] = splitOn " => " s
        inputs2 = chunksOf 2 . splitOn " " . filter (/=',') $ inputs
        output2 = splitOn " " output
        parsePair [n,x] = (read n, x)
    in (map parsePair inputs2, parsePair output2)

dependencies :: M.Map String (Integer, [Ingredient]) -> String -> [String]
dependencies dict "ORE" = []
dependencies dict x =
    let (_, ings) = dict M.! x
        elements = map snd ings
    in nub $ elements ++ concatMap (dependencies dict) elements
    

main = do
  recipes <- map parseLine . lines <$> readFile "input"
  let dict = M.fromList [ (output, (n, inputs)) | (inputs,(n,output)) <- recipes ]
--      loop queue | trace (show queue) False = undefined
      loop [(n,"ORE")] = n
      loop queue =
          let x = fromJust $ find (\(n,e) -> e /= "ORE" && not (e `elem` concat [ dependencies dict y | (_,y) <- queue, y /= e ])) queue
              q = delete x queue
              (n,e) = x
              (m,ings) = dict M.! e
              r = (n+m-1) `div` m
              ings2 = map (\(a,b) -> (a*r,b)) ings
          in loop (consolidate (ings2 ++ q))
  print (loop [(1, "FUEL")])
  print (loop [(7863863, "FUEL")])
  print 1000000000000
  

consolidate xs = map (\ps -> (sum (map fst ps), snd (head ps))) . groupBy ((==) `on` snd) . sortBy (comparing snd) $ xs
  
