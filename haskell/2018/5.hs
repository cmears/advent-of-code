import Data.Char
process polymer =
    let polymer' = step polymer
    in if polymer' == polymer
        then polymer
        else process polymer'

step [] = []
step [x] = [x]
step (a:b:rest) | a `reactsWith` b = step rest
                | otherwise = a : step (b:rest)

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
