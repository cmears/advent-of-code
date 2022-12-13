import Data.Char
import Data.List
import Data.List.Split

data P = L [P] | I Int
  deriving (Show, Eq)

parse s = let (p, "") = parseP s
          in p

parseP :: String -> (P, String)
parseP ('[':rest) = parseHead rest
parseP xs = let (ds,rest) = span isDigit xs
            in (I (read ds), rest)

parseHead (']':rest) = (L [], rest)
parseHead xs =
    let (p,rest) = parseP xs
        (ys, r) = parseTail rest
    in (L (p:ys), r)

-- parseTail (',':xs) =
--     let (ds,rest) = span isDigit xs
--         h = I (read ds)
--         (ys, r) = parseTail rest
--     in ((h:ys), r)
-- parseTail (']':rest) = ([], rest)

parseTail (',':xs) =
    let (p,rest) = parseP xs
        (ys,r) = parseTail rest
    in (p:ys, r)
parseTail (']':rest) = ([], rest)

inorder :: P -> P -> Ordering
inorder (I x) (I y) = compare x y
inorder (L xs) (L ys) = compareLists xs ys
inorder (I x) (L ys) = inorder (L [I x]) (L ys)
inorder (L xs) (I y) = inorder (L xs) (L [I y])

compareLists :: [P] -> [P] -> Ordering
compareLists (x:xs) (y:ys) =
  case inorder x y of
    LT -> LT
    GT -> GT
    EQ -> compareLists xs ys
compareLists [] (y:ys) = LT
compareLists (x:xs) [] = GT
compareLists [] [] = EQ

part1 = do
  xs <- map (map parse) . splitOn [""] . lines <$> readFile "13otavio.txt"
  let indexes = map fst $ filter (\(i,[x,y]) -> inorder x y == LT) $ zip [1..] xs
  print $ xs !! 38
  print indexes
  print $ sum indexes

main = do
  xs0 <- map parse . filter (not . null) . lines <$> readFile "13.txt"
  let xs = L [L [I 2]] : L [L [I 6]] : xs0
--  print xs
  let sorted = sortBy inorder xs
  let Just a = findIndex (\p -> p == L [L [I 2]]) sorted
  let Just b = findIndex (\p -> p == L [L [I 6]]) sorted
  print $ (a+1) * (b+1)
--  mapM_ print sorted
