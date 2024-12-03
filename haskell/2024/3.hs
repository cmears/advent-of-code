import Text.Regex.TDFA
import Control.Monad.State

main = do
  let regex = "mul\\(([0-9]+),([0-9]+)\\)|do|don't"
  blocks <- getAllTextMatches . (=~ regex) <$> readFile "3.txt"
  let part1 = filter ((/='d').head) blocks
      g "do" = put True >> pure False
      g "don't" = put False >> pure False
      g mul = get
      part2 = evalState (filterM g blocks) True
  forM_ [part1, part2] $
    print . sum . map (\[_,a,b] -> read a*read b) . map (\m -> getAllTextSubmatches (m =~ regex))
