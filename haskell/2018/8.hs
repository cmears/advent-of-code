import Control.Monad.State
import Data.Maybe

data Node = Node [Node] [Int]

type S = [Int]

eat :: State S Int
eat = do
    xs <- get
    put $ tail xs
    pure $ head xs

parseNode :: State S Node
parseNode = do
    nc <- eat
    nm <- eat
    children <- replicateM nc parseNode
    metadata <- replicateM nm eat
    pure $ Node children metadata

sumMetadata (Node children metadata) =
    sum metadata + sum (map sumMetadata children)

nodeValue :: Node -> Int
nodeValue (Node [] metadata) = sum metadata
nodeValue (Node children metadata) =
    sum $ map nodeValue $ mapMaybe (\i -> dereference children i) metadata

dereference xs i | i > length xs = Nothing
                 | otherwise = Just (xs !! (i-1))

main = do
    input <- readFile "8.txt"
    --let input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let xs = (read <$> words input) :: [Int]
    let node = evalState parseNode xs
    print $ sumMetadata node
    print $ nodeValue node
