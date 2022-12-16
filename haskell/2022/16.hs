{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Util

type Valve = String
type Path = [(Valve, Int)]

parseLine :: String -> (Valve, (Int, [Valve]))
parseLine (submatches "Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)" -> Just [v,r,vs]) =
    (v, (read r, splitOn ", " vs))
parseLine x = error x

-- Breadth-first traversal.
-- Given an origin node and a neighbour function, gives the list of
-- nodes visited and their distance from the origin.
bfs :: (Eq a, Ord a) => a -> (a -> [a]) -> [(a, Int)]
bfs origin neighbours = bfs' (Seq.singleton (origin, 0)) S.empty
  where bfs' queue seen =
            case Seq.viewl queue of
              Seq.EmptyL -> []
              (x,d) Seq.:< rest | x `S.member` seen -> bfs' rest seen
                                | otherwise -> 
                                    let q' = (rest <> Seq.fromList [(n,d+1) | n <- neighbours x])
                                        seen' = S.insert x seen
                                    in (x,d) : bfs' q' seen'

main :: IO ()
main = do
  ls <- lines <$> readFile "16.txt"

  -- input is a map representing each valve in the input.  For example,
  -- the input line
  --   Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
  -- becomes the entry
  --   "AA" -> (0, ["DD", "II", "BB"])
  let input :: M.Map Valve (Int, [Valve])
      input = M.fromList (map parseLine ls)

  -- Only some valves are worth opening: those with non-zero flow rate.
  let positiveValves :: [Valve]
      positiveValves = M.keys (M.filter ((>0) . fst) input)

  -- As we move around the valves, we should not wander aimlessly but
  -- instead move with purpose towards a particular positive valve.
  -- That is, we don't care about the individual steps between rooms
  -- but only about larger "leaps" between positive valves. We compute
  -- the distance between each pair of positive valves (and the "AA"
  -- valve too because that's where we start.
  --
  -- We calculate the distance by doing a breadth-first traversal from
  -- each valve.
  let distance :: M.Map (Valve, Valve) Int
      distance = M.fromList $ do
                   origin <- "AA":positiveValves
                   (dest, dist) <- bfs origin (\n -> snd (input M.! n))
                   guard (dest `elem` positiveValves)
                   pure ((origin, dest), dist)

  -- Now instead of thinking about moving from room to room, we can
  -- think about "which valves do I open, and in what order?"  We call
  -- that a "path", and it is a list of (v,t) pairs, where v is a
  -- positive valves and t is the minute during which the valve is
  -- opened.

  let findPaths :: Valve -> Int -> Int -> S.Set Valve -> [Path]
      findPaths currentValve currentTime remainingTime unusedValves = do
         -- Which valves can we get to (and open) within the remaining time?
         let validValves = S.filter (\v -> distance M.! (currentValve, v) + 1 <= remainingTime) unusedValves
         -- Now we can either simply stop the path here...
         pure [] ++ do
           -- Or move to one of the within-reach valves.
           v <- S.toList validValves
           let timeUsed = distance M.! (currentValve, v) + 1
           restOfPath <- findPaths v (currentTime + timeUsed) (remainingTime - timeUsed) (S.delete v validValves)
           pure ((v, currentTime + timeUsed):restOfPath)

  -- We can calculate the "score" of a path: the total amount of
  -- pressure released by opening the valves in the given order.
  let scorePath timeLimit path =
          sum $ do
            (valve, timeOpened) <- path
            let rate = fst (input M.! valve)
                openDuration = timeLimit - timeOpened
            pure (rate * openDuration)

  -- Now part 1 can be solved: find the maximum score given by any
  -- path.
  let part1Paths :: [Path]
      part1Paths = findPaths "AA" 0 30 (S.fromList positiveValves)
  
  print (maximum (map (scorePath 30) part1Paths))

  -- In part 2 we must find two non-overlapping paths.  Again we score
  -- each possible path, but where two paths use the same set of nodes
  -- we only record the one with the better score. (We also discard
  -- the paths themselves, keeping only the sets of nodes they use.)
  let part2Paths = findPaths "AA" 0 26 (S.fromList positiveValves)
  let setScore :: M.Map (S.Set Valve) Int
      setScore = foldl (\m p ->
                            let valves = S.fromList (map fst p)
                            in M.insertWith max valves (scorePath 26 p) m) M.empty part2Paths

  -- Now we must find two sets that don't overlap that give a good
  -- combined score.  We sort them with the best score first...
  let setsOrdered :: [(S.Set Valve, Int)]
      setsOrdered = sortBy (comparing (Down . snd)) (M.toList setScore)

  -- Then loop over them to find a good pair.
  let bestScore = loop setsOrdered 0
        where loop [] bestSoFar = bestSoFar
              loop ((set,score):rest) bestSoFar =
                  -- We've picked "set" as the first set and now we're
                  -- looking for its partner. Its best partner is the
                  -- first one in the list that doesn't overlap.
                  -- In addition, we can stop looking as soon as the
                  -- candidate partner's score is too low to beat the
                  -- incumbent best score.
                  let threshold = bestSoFar - score
                      partners = takeWhile (\(_set',score') -> score' > threshold) setsOrdered
                      -- The new best is either the incumbent or is
                      -- replaced by this set and its partner.
                      bestSoFar' = case find (\(set',_score') -> S.null (S.intersection set set')) partners of
                                     Nothing -> bestSoFar
                                     Just (_set',score') -> score + score'
                  in loop rest bestSoFar'

  print bestScore
