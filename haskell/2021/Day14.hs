import qualified Data.Map as M
import Data.List

main = do
  (template:_:rules) <- lines <$> readFile "input14"
  let ruleMap = M.fromList [ (w!!0,w!!2) | r <- rules, let w = words r ]
  let step = M.fromListWith (+) . concatMap (\(k@[a,b],v) -> maybe [(k,v)] (\s -> [(a:s, v), (s++[b], v)]) (M.lookup k ruleMap)) . M.toList
  let pairMap = M.fromListWith (+) ((,1)<$>takeWhile ((>=2).length) (take 2 <$> tails ("X"++template++"X")))
  let results = iterate step pairMap
  let f = ((-)<$>maximum<*>minimum) . map (`div`2) . M.elems . M.delete 'X' . M.fromListWith (+) . concatMap (\([c1,c2],v) -> [(c1,v),(c2,v)]) . M.toList . (results!!)
  print (f 10)
  print (f 40)
