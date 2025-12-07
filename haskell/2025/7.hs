import Data.Array.IArray
import Data.Array.Unboxed

main = do
  ls <- lines <$> readFile "7.txt"
  let n = length (ls !! 0)
  let inputArrays = [ listArray (0,n-1) l :: UArray Int Char | l <- ls ]
  let initialCounts = genArray (0,n-1) (\i -> if (inputArrays !! 0) ! i == 'S' then 1 else 0) :: UArray Int Int
  let f :: (Int, UArray Int Int) -> UArray Int Char -> (Int, UArray Int Int)
      f (splitCount, particleCounts) inputArray =
        let splits = sum [ if particleCounts ! i > 0 && inputArray ! i == '^' then 1 else 0 | i <- [0..n-1] ]
            newCounts = genArray (0,n-1) (\i -> sum [ if i > 0 && inputArray ! (i-1) == '^' then particleCounts ! (i-1) else 0
                                                    , if inputArray ! i /= '^' then particleCounts ! i else 0
                                                    , if i < n-1 && inputArray ! (i+1) == '^' then particleCounts ! (i+1) else 0
                                                    ])
        in (splitCount+splits, newCounts)
  let (splitCount, finalCounts) = foldl f (0,initialCounts) inputArrays
  print splitCount
  print . sum $ elems finalCounts
