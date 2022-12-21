{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Maybe
import Util
import Debug.Trace
import Data.List
import qualified Data.Set as S

import qualified Data.PQueue.Max as Q

type Cost = (Int,Int)




data Blueprint = Blueprint {
      number :: Int
    , oreCost :: Cost
    , clayCost :: Cost
    , obsidianCost :: Cost
    , geodeCost :: Cost
} deriving (Show)

parseLine :: String -> Blueprint
parseLine (submatches "Blueprint (.*): Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." -> Just [n,oreOre,clayOre,obsidianOre,obsidianClay,geodeOre,geodeClay]) = Blueprint (read n) (read oreOre, 0) (read clayOre, 0) (read obsidianOre, read obsidianClay) (read geodeOre, read geodeClay)
parseLine x = error x

data S = S {
      timeLeft :: Int
    , ore :: Int
    , clay :: Int
    , obsidian :: Int
    , geode :: Int
    , oreRate :: Int
    , clayRate :: Int
    , obsidianRate :: Int
    , geodeRate :: Int
    } deriving (Show, Eq)

instance Ord S where
  s1 <= s2 =
      (geode s1, geodeRate s1 * timeLeft s1, timeLeft s1, geodeRate s1, obsidian s1, obsidianRate s1, clay s1, clayRate s1, ore s1, oreRate s1, timeLeft s1)
      <=
      (geode s2, geodeRate s2 * timeLeft s2, timeLeft s2, geodeRate s2, obsidian s2, obsidianRate s2, clay s2, clayRate s2, ore s2, oreRate s2, timeLeft s2)





pareto blueprint frontier =
    let next = nub $ concatMap (neighbours blueprint) frontier
        undominated = loop next
    in undominated
  where
    loop [] = []
    loop (s:ss) =
        if any (\t -> t `dominates` s) ss
        then loop ss
        else s : loop ss

s1 `dominates` s2 =
    and [ f s1 >= f s2 | f <- [geode, geodeRate, obsidian, obsidianRate, clay, clayRate, ore, oreRate] ]



main = do
  blueprints <- map parseLine . lines <$> readFile "19.txt"
  let scores = map process $ blueprints
  print scores
  let total = zipWith (*) [1..] scores
  print $ sum total
  -- let blueprint = blueprints !! 1
  -- let p = pareto blueprint
  -- mapM_ print $ map length $ (iterate p [initS])
--  mapM_ print $ (iterate p [initS]) !! 15
--  print $ dfs blueprint [initS] Nothing

dfs :: Blueprint -> [S] -> Maybe S -> Maybe S
dfs blueprint [] incumbent = incumbent
--dfs blueprint ss incumbent | trace (show (geode (head ss))) False = undefined
dfs blueprint (s:ss) incumbent =
    let incumbent' = if geode s > maybe 0 geode incumbent then trace ("new best: " ++ show (geode s)) (Just s) else incumbent
        ns = reverse $ neighbours blueprint s
    in dfs blueprint (ns++ss) incumbent'
    

initS = (S 24 0 0 0 0 1 0 0 0)

process :: Blueprint -> Int
process blueprint | trace (show blueprint) False = undefined
process blueprint = maybe 0 geode $ search blueprint (Q.singleton initS) Nothing S.empty

type Q = Q.MaxQueue S

search :: Blueprint -> Q -> Maybe S -> S.Set S -> Maybe S
--search blueprint q incumbent seen | trace (show (maybe 0 geode incumbent, Q.size q)) False = undefined
search blueprint q incumbent seen =
    case Q.maxView q of
      Nothing -> incumbent
      Just (s, q') | s `S.member` seen -> search blueprint q' incumbent seen
      Just (s, q') -> -- trace (show (maybe 0 geode incumbent, Q.size q', s)) $
    --trace (show s) $
          let incumbent' = if geode s > maybe 0 geode incumbent then trace ("new best: " ++ show (geode s)) (Just s) else incumbent
              ss = reverse $ neighbours blueprint s
              q'' = Q.union q' (Q.fromList ss)
          in search blueprint q'' incumbent' (S.insert s seen)

neighbours :: Blueprint -> S -> [S]
neighbours blueprint s | timeLeft s == 0 = []
--neighbours blueprint s | trace (show s) False = undefined
neighbours blueprint s = do
  -- let o = ore s
  --     c = clay s
  -- oreRobots <- [0..o `div` fst (oreCost blueprint)]
  -- let o' = o - oreRobots * fst (oreCost blueprint)
  -- clayRobots <- [0..o' `div` fst (clayCost blueprint)]
  -- let o'' = o' - clayRobots * fst (clayCost blueprint)
  -- obsidianRobots <- [0..min (o'' `div` fst (obsidianCost blueprint)) (c `div` snd (obsidianCost blueprint))]
  -- let o''' = o'' - obsidianRobots * fst (obsidianCost blueprint)
  --     c' = c - obsidianRobots * snd (obsidianCost blueprint)
  -- geodeRobots <- [0..min (o''' `div` fst (geodeCost blueprint)) (obsidian s `div` snd (geodeCost blueprint))]
  -- let o'''' = o''' - geodeRobots * fst (geodeCost blueprint)
  --     obs' = obsidian s - geodeRobots * snd (geodeCost blueprint)
  let canBuyOre = ore s >= fst (oreCost blueprint)
      canBuyClay = ore s >= fst (clayCost blueprint)
      canBuyObsidian = ore s >= fst (obsidianCost blueprint) && clay s >= snd (obsidianCost blueprint)
      canBuyGeode = ore s >= fst (geodeCost blueprint) && obsidian s >= snd (geodeCost blueprint)

  oreRobots <- if canBuyOre then [0,1] else [0]
  clayRobots <- if canBuyClay && oreRobots == 0 then [0,1] else [0]
  obsidianRobots <- if canBuyObsidian && oreRobots == 0 && clayRobots == 0 then [0,1] else [0]
  geodeRobots <- if canBuyGeode && oreRobots+clayRobots+obsidianRobots == 0 then [0,1] else [0]

  let finalOre = ore s + oreRate s - (oreRobots*fst (oreCost blueprint)) - (clayRobots*fst (clayCost blueprint)) - (obsidianRobots*fst (obsidianCost blueprint)) - (geodeRobots*fst (geodeCost blueprint))
      finalClay = clay s + clayRate s - (obsidianRobots*snd(obsidianCost blueprint))
      finalObsidian = obsidian s + obsidianRate s - (geodeRobots*snd(geodeCost blueprint))
      finalGeode = geode s + geodeRate s

  let orr = oreRate s + oreRobots
      cr = clayRate s + clayRobots
      obr = obsidianRate s + obsidianRobots
      gr = geodeRate s + geodeRobots

  let maxOreCost = maximum (map (\f -> fst (f blueprint)) [clayCost, obsidianCost, geodeCost])
  guard (not (oreRate s >= maxOreCost && oreRobots == 1))

  guard (oreRobots + clayRobots + obsidianRobots + geodeRobots <= 1)

--  guard (not (timeLeft s > 3 && oreRobots + clayRobots + obsidianRobots + geodeRobots == 0 && (canBuyOre && canBuyClay && canBuyObsidian && canBuyGeode)))

--  guard (not (canBuyGeode && geodeRobots == 0))
--  guard (not (canBuyObsidian && not canBuyGeode && obsidianRobots == 0))
--  guard (not (canBuyClay && not canBuyGeode && not canBuyObsidian && clayRobots == 0))
                        

  -- guard (not (cr == 0 && ore s >= fst (clayCost blueprint) && clayRobots == 0))
  -- guard (not (obr == 0 && clay s >= snd (obsidianCost blueprint) && ore s >= fst (obsidianCost blueprint) && obsidianRobots == 0))
  -- guard (not (gr == 0 && obsidian s >= snd (geodeCost blueprint) && ore s >= fst (geodeCost blueprint) && geodeRobots == 0))

  guard (not (timeLeft s == 1 && (oreRobots + clayRobots + obsidianRobots + geodeRobots == 1)))
  guard (not (timeLeft s == 2 && (oreRobots + clayRobots + obsidianRobots == 1)))
  guard (not (timeLeft s == 3 && (clayRobots == 1)))

--  guard (not (clayRobots == 1 && clay s >= snd (obsidianCost blueprint)))

  pure (S (timeLeft s - 1) finalOre finalClay finalObsidian finalGeode orr cr obr gr)

part2 = 25 * 19 * 31
