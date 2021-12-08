import Data.Maybe
import Data.List

main = do
  entries <- map (((,) <$> take 10 <*> drop 11) . words) . lines <$> readFile "input8"
  print . length . filter (`elem` [2,3,4,7]) . concatMap (map length . snd) $ entries
  print . sum $ process <$> entries
process :: ([String],[String]) -> Int
process (inputs,outputs) =
  let goodMapping = fromJust . find (\p -> all isJust (apply2 p <$> inputs)) $ permutations "abcdefg"
  in read . concatMap show $ fromJust <$> apply2 goodMapping <$> outputs
apply permutation = map (fromJust . (`elemIndex` permutation))
apply2 permutation input = sort (apply permutation input) `elemIndex` digits
digits = apply "abcdefg" <$> [ "abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg" ]
