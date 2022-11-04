{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Util
import qualified Data.Map as M
import Data.List
type Coord = (Int, Int)

main = do
    coords <- map parseCoord . lines <$> readFile "6.txt"
    let minX = minimum $ map fst coords
    let maxX = maximum $ map fst coords
    let minY = minimum $ map snd coords
    let maxY = maximum $ map snd coords

    let m = M.mapMaybe id . M.fromList $ do
        c <- (,) <$> [minX..maxX] <*> [minY..maxY]
        pure (c, findNearestCoord c coords)

    -- These coordinates meet the edge and so go on forever.
    let forbiddenCoords = map snd $ filter (\((x,y),_) -> x == minX || x == maxX || y == minY || y == maxY) $ M.toList m

    let m' = M.filter (not . (`elem` forbiddenCoords)) m

    -- Part 1
    print $ snd $ last $ sortOn snd $ count $ M.elems m'
    -- Part 2
    print $ length $ filter (\c -> sum (map (distance c) coords) < 10000) $ (,) <$> [minX..maxX] <*> [minY..maxY]

findNearestCoord c coords =
    let sorted = sortOn (distance c . snd) $ zip [0..] coords
    in case sorted of
        ((c1,d1):(c2,d2):_) | d1 == d2 -> Nothing
                            | otherwise -> Just c1

distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

parseCoord :: String -> Coord
parseCoord (submatches "(.*), (.*)" -> Just [x,y]) = (read x, read y)

