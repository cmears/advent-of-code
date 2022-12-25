import Data.List
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "23.txt"
  let elves = S.fromList [ (x,y) | (y,l) <- zip [5,4..] ls, (x,c) <- zip [0..] l, c == '#' ]

  let states = scanl step elves (take 4 <$> tails (cycle [N,S,W,E]))
  let e = S.toList $ states !! 10
      minx = minimum (fst <$> e)
      maxx = maximum (fst <$> e)
      miny = minimum (snd <$> e)
      maxy = maximum (snd <$> e)
  print $ (maxx-minx+1)*(maxy-miny+1) - S.size elves

  let Just i = findIndex (uncurry (==)) (zip states (tail states))
  print (i+1)

type Coord = (Int,Int)
data Dir = N | S | W | E

propose :: S.Set Coord -> [Dir] -> Coord -> Coord
propose elves dirs c | S.null (S.intersection (adjacent c) elves) = c
                     | otherwise = propose' elves dirs c

propose' :: S.Set Coord -> [Dir] -> Coord -> Coord
propose' elves [] c = c
propose' elves (d:dirs) c | S.null (S.intersection (sweep d c) elves) = move d c
                          | otherwise = propose' elves dirs c

sweep N (x,y) = S.fromList [(x-1,y+1),(x,y+1),(x+1,y+1)]
sweep S (x,y) = S.fromList [(x-1,y-1),(x,y-1),(x+1,y-1)]
sweep W (x,y) = S.fromList [(x-1,y+1),(x-1,y),(x-1,y-1)]
sweep E (x,y) = S.fromList [(x+1,y+1),(x+1,y),(x+1,y-1)]

adjacent c = S.insert (move W c) . S.insert (move E c) $ S.union (sweep N c) (sweep S c)

move N (x,y) = (x,y+1)
move S (x,y) = (x,y-1)
move W (x,y) = (x-1,y)
move E (x,y) = (x+1,y)

step :: S.Set Coord -> [Dir] -> S.Set Coord
step elves dirs =
    let list = S.toList elves
        proposals = map (propose elves (take 4 dirs)) list
        pairs = zip list proposals
        collisions = S.fromList . map head . filter ((>1).length) . group . sort $ proposals
    in S.fromList $ do
          (current,proposal) <- pairs
          pure $ if proposal `S.member` collisions then current else proposal
