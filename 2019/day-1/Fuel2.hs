main = do
  c <- getContents
  let masses = map (read :: String -> Integer) . lines $ c
  let fuels = map fuel masses
  print (sum fuels)

fuel m =
  let f = m `div` 3 - 2
  in if f < 0
     then 0
     else f + fuel f
