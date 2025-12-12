import Data.List.Split

data Outcome = Sat | Unsat | Unknown deriving (Show, Eq)

main = do
  chunks <- splitOn [""] . lines <$> readFile "12.txt"
  let (shapes,[cases]) = splitAt (length chunks-1) chunks
  let shapeAreas = map (length . filter (=='#') . concat) shapes
  let f x = let (size:counts0) = words x
                counts = map read counts0
                totalArea = sum (zipWith (*) counts shapeAreas)
                [w::Int,h] = read <$> splitOn "x" (init size)
                blocksAvailable = (w`div`3) * (h`div`3)
                blocksRequired = sum counts
            in if totalArea > w*h then Unsat
               else if blocksRequired <= blocksAvailable
                    then Sat
                    else Unknown
  let outcomes = map f cases
  print $ length $ filter (==Sat) outcomes
  print $ length $ filter (==Unsat) outcomes
  print $ length $ filter (==Unknown) outcomes
