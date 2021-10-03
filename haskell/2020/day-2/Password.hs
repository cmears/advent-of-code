import Text.Regex.TDFA

main = do
  inputs <- map parseLine . lines <$> readFile "input.txt"
  print (length (filter valid inputs))
  print (length (filter valid2 inputs))

parseLine :: String -> (Int, Int, Char, String)
parseLine s =
  let (_, _, _, matches) = (s =~ "([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)") :: (String, String, String, [String])
      [a,b,c,d] = matches
  in (read a, read b, head c, d)

-- Part 1 validation scheme.
valid :: (Int, Int, Char, String) -> Bool
valid (l, u, x, xs) =
  let n = length (filter (==x) xs)
  in l <= n && n <= u

-- Part 2 validation scheme.
valid2 :: (Int, Int, Char, String) -> Bool
valid2 (i, j, x, xs) =
  (xs !! (i-1) == x) /= (xs !! (j-1) == x)
