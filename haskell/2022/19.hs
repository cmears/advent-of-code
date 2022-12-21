{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.Maybe
import Util
import qualified Data.Set as S

type Cost = [Int]
type Blueprint = [Cost]

parseLine :: String -> Blueprint
parseLine (submatches "Blueprint .*: Each ore robot costs (.*) ore. Each clay robot costs (.*) ore. Each obsidian robot costs (.*) ore and (.*) clay. Each geode robot costs (.*) ore and (.*) obsidian." -> Just [a,b,c,d,e,f]) = [[read a,0,0],[read b,0,0],[read c,read d,0],[read e,0,read f]]
parseLine x = error x

data S = S {
      timeLeft :: Int
    , choice :: Int
    , inventory :: [Int]
    , rates :: [Int]
} deriving (Show, Eq)

main = do
  blueprints <- map parseLine . lines <$> readFile "19.txt"
  let initS t = [S t 0 [0,0,0,0] [1,0,0], S t 1 [0,0,0,0] [1,0,0]]
  let score t b = maybe 0 ((!!3).inventory) (dfs b (initS t) Nothing)
  print . sum . zipWith (*) [1..] $ map (score 24) blueprints
  print . product $ map (score 32) (take 3 blueprints)

dfs :: Blueprint -> [S] -> Maybe S -> Maybe S
dfs blueprint [] incumbent = incumbent
dfs blueprint (s:ss) incumbent | potential <= incumbentScore = dfs blueprint ss incumbent
                               | otherwise = dfs blueprint (ns++ss) incumbent'
  where
    incumbentScore = maybe 0 ((!!3).inventory) incumbent
    incumbent' = if (inventory s !! 3) > incumbentScore then Just s else incumbent
    ns = neighbours blueprint s
    potential = triangle ((timeLeft s)-1) + (inventory s !! 3)

triangle n = ((1+n) * n) `div` 2

collect s =
    s { timeLeft = pred (timeLeft s)
      , inventory = inventory s `plus` rates s }

plus [a,b,c,d] [e,f,g] = [a+e,b+f,c+g,d]
minus [a,b,c,d] [e,f,g] = [a-e,b-f,c-g,d]
covers [a,b,c,d] [e,f,g] = a>=e && b>=f && c>=g

update :: Int -> (a -> a) -> [a] -> [a]
update i f xs = take i xs ++ [f (xs !! i)] ++ drop (i+1) xs

neighbours :: Blueprint -> S -> [S]
neighbours blueprint s | timeLeft s == 0 = []
neighbours blueprint s = do
  if (inventory s) `covers` (blueprint !! (choice s))
    then do
      let s1 = collect s
          s2 = s1 { inventory = (inventory s1) `minus` (blueprint !! choice s)
                   , rates = if choice s < 3 then update (choice s) succ (rates s1) else rates s1 }
          s3 = if choice s == 3 then s2 { inventory = update 3 (+(timeLeft s2)) (inventory s2) } else s2

      c <- [3,2,1,0]

      -- Don't try to build a geode robot if we're not making any obsidian.
      guard (not (rates s3 !! 2 == 0 && c == 3))
      -- Don't try to build an obsidian robot if we're not making any clay.
      guard (not (rates s3 !! 1 == 0 && c == 2))
      -- Don't try to build an ore robot if we are already making
      -- enough ore per minute to buy whatever we want.
      guard (not (rates s3 !! 0 >= maximum (map head blueprint) && c == 0))

      pure (s3 { choice = c })
    else
      neighbours blueprint (collect s)
