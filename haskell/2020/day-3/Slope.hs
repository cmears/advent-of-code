import qualified Data.Map as M

main = do
  ls <- lines <$> readFile "input.txt"
  let m = M.fromList [ ((i,j),t) | (i,l) <- zip [0..] ls, (j,t) <- zip [0..] l ]
  let r = length ls
      c = length (head ls)
  let go right down =
        let coords = takeWhile (\(i,_) -> i < r) [ (down*i, (right*i) `mod` c) | i <- [0..] ]
            cells = map (m M.!) coords
        in length (filter (=='#') cells)
  print $ go 3 1
  print $ go 1 1 * go 3 1 * go 5 1 * go 7 1 * go 1 2
