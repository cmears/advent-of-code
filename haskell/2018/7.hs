{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
import Util
import Data.List
import Data.Char
import Data.Bifunctor
import Debug.Trace

parseConstraint :: String -> (Char, Char)
parseConstraint (submatches "Step (.) must be finished before step (.) can begin\\." -> Just [[x],[y]]) = (x, y)
parseConstraint _ = undefined

topsort :: [Char] -> [(Char, Char)] -> [Char]
topsort [] [] = []
topsort chars constraints =
    let rights = nub $ map snd constraints
        sources = chars \\ rights
        next = minimum sources
    in next : topsort (delete next chars) (filter (\(l,r) -> l /= next) constraints)

loop t [] [] nworkers constraints = []
loop t chars tasks nworkers constraints =
--    trace (show (t, sources, spareWorkers)) $
      map (t,) startedTasks
           ++ loop (t+1) (chars \\ startedChars) (startedTasks ++ ongoingTasks) nworkers constraints'
    where
        tasks2 = map (second (subtract 1)) tasks
        (finishedTasks, ongoingTasks) = partition ((==0) . snd) tasks2
        finishedChars = map fst finishedTasks
        constraints' = filter (\(l,r) -> l `notElem` finishedChars) constraints
        spareWorkers = nworkers - length ongoingTasks
        rights = nub $ map snd constraints'
        sources = chars \\ rights
        startedChars = take spareWorkers $ sort sources
        startedTasks = map (\c -> (c, taskCost c)) startedChars
        taskCost c = 60 + ord c - ord 'A' + 1



main = do
    constraints <- map parseConstraint . lines <$> readFile "7.txt"
    let chars = nub $ map fst constraints ++ map snd constraints
    print $ topsort chars constraints
    let log = loop 0 chars [] 5 constraints
    print $ maximum $ map (\(t,(_,l)) -> t+l) log