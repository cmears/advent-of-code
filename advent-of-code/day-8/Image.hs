import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

main = do
  c <- filter isDigit <$> readFile "input"
  print $ length c
  let layers = chunksOf (25*6) c
  print (length layers)
  let taggedLayers = map (\l -> (length (filter (=='0') l), l)) layers
  let fewestZeroes = minimumBy (comparing fst) taggedLayers
  let nones = length (filter (=='1') (snd fewestZeroes))
  let ntwos = length (filter (=='2') (snd fewestZeroes))
  print (nones * ntwos)

  let pixelStacks = transpose layers
      pixels = map processStack pixelStacks
  mapM_ print (chunksOf 25 pixels)


processStack ('2':xs) = processStack xs
processStack ('1':xs) = 'o'
processStack ('0':xs) = ' '
processStack [] = '?'
