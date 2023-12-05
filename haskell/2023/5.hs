import Data.List.Split
import Data.Maybe

main = do
  ls <- lines <$> readFile "5.txt"
  let ([seedLine]:mapChunks) = splitOn [""] ls
      seeds = map read (tail (words seedLine)) :: [Integer]
      mappings = map (parseMap . tail) mapChunks
  let locations = map (applyMappings mappings) seeds
  print $ minimum locations

applyMappings :: [Mapping] -> Integer -> Integer
applyMappings mappings x = foldl f x mappings
  where f y m = applyMapping m y

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

data Mapping = Mapping [(Integer,Integer,Integer)]
  deriving (Show)

parseMap :: [String] -> Mapping
parseMap ls = Mapping $ do
                l <- ls
                let [dest, source, len] = map read (words l)
                pure (dest, source, len)
