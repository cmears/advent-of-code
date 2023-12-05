import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Debug.Trace
import Text.Printf

main = do
  ls <- lines <$> readFile "5.txt"
  let ([seedLine]:mapChunks) = splitOn [""] ls
      seeds = map read (tail (words seedLine)) :: [Integer]
      seedPairs = chunksOf 2 seeds
      ranges = map (\[a,b] -> Range a b) seedPairs
      mappings0 = map (parseMap . tail) mapChunks
      mappings = map normaliseMap mappings0
  -- print $ head mappings0
  -- print $ head mappings
  let locations = map (applyMappings mappings) seeds
--  print $ minimum locations
  let locationRanges = applyMappingsRange mappings ranges
      sorted = sort locationRanges
      (Range m _) = head sorted
  print m

-- Range start length
data Range = Range Integer Integer
  deriving (Eq, Ord)

instance Show Range where
  show (Range start len) = "<" ++ show start ++ ".." ++ show (start+len-1) ++ ">"

applyMappings :: [Mapping] -> Integer -> Integer
applyMappings mappings x = foldl f x mappings
  where f y m = applyMapping m y

applyMappingsRange :: [Mapping] -> [Range] -> [Range]
applyMappingsRange mappings ranges = foldl f ranges mappings
  where f rs m = concatMap (applyMappingRange m) rs

applyMapping :: Mapping -> Integer -> Integer
applyMapping (Mapping sections) x =
  case mapMaybe f sections of
    (y:_) -> y
    [] -> x
  where
    f (dest, source, len) =
        if source <= x && x < source+len
        then Just (dest + (x - source))
        else Nothing

applyMappingRangeDebug m r =
  let result = applyMappingRange m r
  in trace (show (m,r,result)) $ result

applyMappingRange :: Mapping -> Range -> [Range]
applyMappingRange _ (Range _ 0) = []
applyMappingRange (Mapping sections) (Range start len) =
  case find f sections of
    Just (mdest,msource,mlen) ->
        let offset = start - msource
            coverage = mlen - offset
            coverage2 = min coverage len
            remaining = len - coverage2
            r1 = Range (mdest + offset) coverage2
        in r1 : applyMappingRange (Mapping sections) (Range (start+coverage2) remaining)
    Nothing -> error "nyi"
  where
    f (dest, source, len) = source <= start && start < source+len

data Mapping = Mapping [(Integer,Integer,Integer)]

instance Show Mapping where
    show (Mapping sections) = concatMap (\(dest,source,len) ->
                                             printf "{%d..%s}{%+d}" source (f (source+len-1)) (dest-source)) sections
      where f x | x >= 10^12 = "inf"
                | otherwise = show x

parseMap :: [String] -> Mapping
parseMap ls = Mapping $ do
                l <- ls
                let [dest, source, len] = map read (words l)
                pure (dest, source, len)

normaliseMap :: Mapping -> Mapping
normaliseMap (Mapping sections) =
    let sorted = sortBy (comparing (\(dest,source,len) -> source)) sections
    in Mapping (loop 0 sorted)
  where
    loop :: Integer -> [(Integer,Integer,Integer)] -> [(Integer,Integer,Integer)]
    loop x [] = [(x, x, 10^20)]
--    loop x sections | trace (show sections) False = undefined
    loop x ((dest,source,len):sections) | x == source =
                                            (dest,source,len) : loop (source+len) sections
                                        | x < source =
                                            (x,x,source-x) : loop source ((dest,source,len):sections)
