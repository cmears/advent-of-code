{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Data.Set as S

p1 = [23,32,46,47,27,35,1,16,37,50,15,11,14,31,4,38,21,39,26,22,3,2,8,45,19]
p2 = [13,20,12,28,9,10,30,25,18,36,48,41,29,24,49,33,44,40,6,34,7,43,42,17,5]

p1e = [9,2,6,3,1]
p2e = [5,8,4,7,10]

p1r = [43,19]
p2r = [2,29,14]

score = sum . zipWith (*) [1..] . reverse

go p1 [] = score p1
go [] p2 = -score p2
go (x:xs) (y:ys) | x > y = go (xs ++ [x,y]) ys
                 | x < y = go xs (ys ++ [y,x])

type Config = ([Int],[Int])
type GameMemory = S.Set Config

-- Evaluate a game/round of recursive combat.
-- Positive score if player 1 won, negative score if player 2 won.
-- The game memory records which configurations we have seen in *this* game (for the infinite-avoidance rule).
go2 :: GameMemory -> Config -> Int
go2 game (xs,ys) = do
  case (xs,ys) of
    (_,[]) -> score xs
    ([],_) -> -score ys
    _ | S.member (xs,ys) game -> score xs
    (x:xs',y:ys') ->
      let game' = S.insert (xs,ys) game
          p1wins = if length xs' >= x && length ys' >= y
                   then go2 S.empty (take x xs', take y ys') > 0
                   else x > y
          config = if p1wins then ((xs'++[x,y]), ys') else (xs', (ys'++[y,x]))
      in go2 game' config

main = do
  print $ go p1 p2
  print $ go2 S.empty (p1,p2)
