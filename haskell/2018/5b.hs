import Data.Char
process :: String -> String
process s = f ([], s)

f ([], (y:ys)) = f ([y], ys)
f (xs, []) = reverse xs
f ((x:xs), (y:ys)) | x `reactsWith` y = f (xs, ys)
                   | otherwise = f ((y:x:xs), ys)

-- >>> ['a' `reactsWith` 'A', 'a' `reactsWith` 'a', 'a' `reactsWith` 'b']
-- [True,False,False]
a `reactsWith` b = isLower a /= isLower b && toLower a == toLower b

score' = length . process

score x = score' . filter (\c -> toLower c /= x)

main = do
    polymer <- head . lines <$> readFile "5.txt"
    
    -- Part 1
    print $ score' polymer

    -- Part 2
    print $ minimum $ [ score c polymer | c <- ['a'..'z']]
