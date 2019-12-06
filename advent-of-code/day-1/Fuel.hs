main = do
  c <- getContents
  let masses = map (read :: String -> Integer) . lines $ c
  let fuels = map (\m -> m `div` 3 - 2) masses
  print (sum fuels)
