import Data.Maybe

input = (12578151, 5051300)
ex = (5764801, 17807724)

transform :: Integer -> [Integer]
transform subject = iterate step 1
  where
    step x = (x * subject) `mod` 20201227

hunt (key1, key2) =
    head . mapMaybe f . zip [0..] $ transform 7
  where
    f (i,k) | k == key1 = Just $ transform key2 !! i
            | k == key2 = Just $ transform key1 !! i
            | otherwise = Nothing

main = print (hunt input)
