import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

data Amphipod = A | B | C | D
  deriving (Eq, Ord, Show)
data Loc = Hallway Int | Home Amphipod Int
  deriving (Eq, Ord, Show)

weight A = 1
weight B = 10
weight C = 100
weight D = 1000

neighbours :: Loc -> [Loc]
neighbours (Hallway x) = concat [
    if x > 1 then [Hallway (x-1)] else []
    , if x < 11 then [Hallway (x+1)] else []
    , if x == 3 then [Home A 1] else []
    , if x == 5 then [Home B 1] else []
    , if x == 7 then [Home C 1] else []
    , if x == 9 then [Home D 1] else []
    ]
neighbours (Home amphipod 1) = concat [
      [Home amphipod 2]
    , if amphipod == A then [Hallway 3] else []
    , if amphipod == B then [Hallway 5] else []
    , if amphipod == C then [Hallway 7] else []
    , if amphipod == D then [Hallway 9] else []
    ]
neighbours (Home amphipod 2) = [Home amphipod 1, Home amphipod 3]
neighbours (Home amphipod 3) = [Home amphipod 2, Home amphipod 4]
neighbours (Home amphipod 4) = [Home amphipod 3]

type S = M.Map Loc Amphipod

unoccupied :: S -> Loc -> Bool
unoccupied s loc = not (M.member loc s)

reachable :: S -> Loc -> [(Loc, Int)]
reachable s loc0 = loop Nothing 0 loc0
  where loop prev dist loc =
            (loc,dist) : do l <- neighbours loc
                            guard (prev /= Just l)
                            guard (unoccupied s l)
                            loop (Just loc) (dist+1) l

example1 :: S
example1 = M.fromList (zip [Home a n | a <- [A,B,C,D], n <- [1..4]] [B,A,A,A,C,D,B,B,B,C,C,C,D,A,D,D])

example2 :: S
example2 = M.fromList (zip [Home a n | a <- [A,B,C,D], n <- [1..4]] [B,D,D,A,C,C,B,D,B,B,A,C,D,A,C,A])

input1 :: S
input1 = M.fromList (zip [Home a n | a <- [A,B,C,D], n <- [1..4]] [C,D,A,A,A,C,B,B,B,A,C,C,D,B,D,D])

input2 :: S
input2 = M.fromList (zip [Home a n | a <- [A,B,C,D], n <- [1..4]] [C,D,D,D,A,C,B,C,B,B,A,A,D,A,C,B])

isHallway (Hallway _) = True
isHallway _ = False

isHome a (Home x _) = x == a
isHome a _ = False

isAnyHome (Home _ _) = True
isAnyHome _ = False

search :: S -> (Int, Int)
search initS = loop Nothing (S.singleton (h initS, 0, initS)) 0
  where loop best queue stats =
            case S.minView queue of
              Just ((heur,cost,s),queue') ->
                  if goal s
                  then if maybe True (\b -> cost < b) best
                       then loop (Just cost) queue' (stats+1)
                       else loop best queue' (stats+1)
                  else if maybe False (\b -> heur >= b) best
                       then (fromJust best, stats)
                       else let pairs = do
                                        (loc,amphipod) <- M.toList s
                                        guard $ case loc of
                                                  Home a 4 -> amphipod /= a
                                                  Home a 3 -> amphipod /= a || (maybe False (/= a) (M.lookup (Home a 4) s))
                                                  Home a 2 -> amphipod /= a || (maybe False (/= a) (M.lookup (Home a 4) s) || maybe False (/= a) (M.lookup (Home a 3) s))
                                                  Home a 1 -> amphipod /= a || (maybe False (/= a) (M.lookup (Home a 4) s) || maybe False (/= a) (M.lookup (Home a 3) s) || maybe False (/= a) (M.lookup (Home a 2) s))
                                                  _ -> True
                                        (dest,pathlen) <- tail (reachable s loc)
                                        guard $ case dest of
                                                  Hallway n -> not (n `elem` [3,5,7,9]) && not (isHallway loc)
                                                  Home a 1 ->
                                                      and [ amphipod == a
                                                          , M.lookup (Home a 2) s == Just a
                                                          , M.lookup (Home a 3) s == Just a
                                                          , M.lookup (Home a 4) s == Just a
                                                          ]
                                                  Home a 2 ->
                                                      and [ amphipod == a
                                                          , M.lookup (Home a 3) s == Just a
                                                          , M.lookup (Home a 4) s == Just a
                                                          ]
                                                  Home a 3 ->
                                                      and [ amphipod == a
                                                          , M.lookup (Home a 4) s == Just a
                                                          ]
                                                  Home a 4 ->
                                                      and [ amphipod == a ]
                                        let pathcost = weight amphipod * pathlen
                                        let s' = M.insert dest amphipod (M.delete loc s)
                                        pure (dest, (h s' + cost + pathcost, cost + pathcost, s'))
                            in case find (isAnyHome . fst) pairs of
                                 Just (_, trip) -> loop best (S.insert trip queue') (stats+1)
                                 Nothing -> loop best (S.union queue' (S.fromList (map snd pairs))) (stats+1)

goal :: S -> Bool
goal s = M.toList s == [ (Home amphipod n, amphipod) | amphipod <- [A,B,C,D], n <- [1..4] ]

h :: S -> Int
h s = let misplaced = [ amphipod | (loc, amphipod) <- M.toList s, not (isHome amphipod loc) ]
          grouped = group (sort misplaced)
          blocking = do
            (Home a n, amphipod) <- M.toList s
            guard (a == amphipod)
            guard (any (/=a) (catMaybes [ M.lookup (Home a n2) s | n2 <- [n+1..4] ]))
            pure (Home a n)
      in 
      sum [ weight (head g) * t (length g - 1) | g <- grouped ]
      + sum [ weight a * (2*n+1) | Home a n <- blocking ]
      + sum (do
              (loc, amphipod) <- M.toList s
              let minpathlen = h' amphipod loc
              pure (minpathlen * weight amphipod))

t n = n*(n+1)`div`2

h' amphipod (Hallway n) = abs (n - destination amphipod) + 1
h' amphipod (Home a _) | a == amphipod = 0
h' amphipod (Home a n) = n + abs (destination amphipod - destination a) + 1

destination A = 3
destination B = 5
destination C = 7
destination D = 9

main = mapM_ (print . fst . search) [input1, input2]
