import Data.List.Split
main = do
  let { execute x ["noop"] = x ; execute x ["addx",y] = x+read y
      ; expand ["noop"] = [["noop"]] ; expand ["addx",x] = [["noop"],["addx",x]] }
  values <- scanl execute 1 . concatMap expand . map words . lines <$> readFile "10.txt"
  print . sum $ map (\n -> values !! (n-1) * n) [20,60..220]
  mapM_ print . chunksOf 40 $ map (\n -> ".#" !! fromEnum (abs (values !! n - (n `mod` 40)) <= 1)) [0..239]
