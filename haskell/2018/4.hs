{-# LANGUAGE ViewPatterns #-}
import Data.Function
import Data.List.Split
import Data.List
import Data.Ord
import Util

type Record = (Timestamp, Event)
-- y,m,d,h,m
type Timestamp = (Integer, Integer, Integer, Integer, Integer)
data Event = Begin Integer
           | Sleep
           | Awake
  deriving (Show, Ord, Eq)

isBegin (Begin x) = True
isBegin _ = False

parseLine :: String -> Record
parseLine (submatches "\\[(.*)\\] (.*)" -> Just [t,e]) = (parseTimestamp t, parseEvent e)

parseTimestamp :: String -> Timestamp
parseTimestamp (submatches "([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)" -> Just [y,m,d,h,i]) = (read y, read m, read d, read h, read i)

parseEvent :: String -> Event
parseEvent (submatches "Guard #([0-9]+) begins shift" -> Just [i]) = Begin (read i)
parseEvent (submatches "falls asleep" -> Just []) = Sleep
parseEvent (submatches "wakes up" -> Just []) = Awake
parseEvent s = error s

groupByShift :: [Record] -> [(Integer, [Record])]
groupByShift [] = []
groupByShift (r@(_,Begin i):records) =
  let (rs,rest) = break (isBegin . snd) records
  in (i, rs) : groupByShift rest

computeNaps :: [Record] -> [(Integer, Integer)]
computeNaps [] = []
computeNaps (((_,_,_,_,m1),Sleep):((_,_,_,_,m2),Awake):rs) =
    (m1,m2):computeNaps rs

computeTotalSnooze = sum . map (\(x,y) -> y-x)

findSleepiestMinute :: [(Integer, Integer)] -> Integer
findSleepiestMinute pairs = fst $ maximumBy (comparing snd) $ count $ concat [ [x..y-1] | (x,y) <- pairs ]

computeTriples (g, pairs) =
    let cs = count $ concat [ [x..y-1] | (x,y) <- pairs ]
    in map (\(m,c) -> (g,m,c)) cs


main = do
  records <- sort . map parseLine . lines <$> readFile "4.txt"
  let naps = map (\(i,rs) -> (i, computeNaps rs)) $ groupByShift records
  let guards = map (\g -> (fst (head g), concat (map snd g))) $ groupBy ((==) `on` fst) (sort naps)

  let sleepiestGuard = maximumBy (comparing (computeTotalSnooze . snd)) guards
  let guardId = fst sleepiestGuard
  let sleepiestMinute = findSleepiestMinute (snd sleepiestGuard)
  print $ guardId * sleepiestMinute

  let triples = concatMap computeTriples guards
--  print triples

  let (g,m,c) = maximumBy (comparing (\(a,b,c) -> c)) triples
  print $ g*m
