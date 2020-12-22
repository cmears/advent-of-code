{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State
import qualified Data.Map as M
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
type St = StateT GameMemory IO

logEnabled = False

-- Evaluate a game/round of recursive combat.
-- Positive score if player 1 won, negative score if player 2 won.
-- "depth" is the depth of recursion for nicer output.
-- The game memory records which configurations we have seen in *this* game (for the infinite-avoidance rule).
go2 :: Int -> Config -> St Int
go2 depth (xs,ys) = do
  game <- get
  case (xs,ys) of
    -- Player 1 wins.
    (_,[]) -> pure $ score xs
    -- Player 1 wins by the infinite-avoidance rule.
    _ | S.member (xs,ys) game -> pure $ score xs
    -- Player 2 wins.
    ([],_) -> pure $ -score ys
    -- No one has won yet.
    (x:xs',y:ys') -> do
      -- Figure out who won this round.  (True for player 1, False for player 2)
      p1wins <- do
        -- If both players have enough cards to recurse...
        if length xs' >= x && length ys' >= y
        then do let subconfig = (take x xs', take y ys')
                log $ "recursive: " ++ show subconfig
                -- Play the subconfig a "fresh" game, with an empty "game" memory.
                s <- liftIO $ evalStateT (go2 (depth+2) subconfig) S.empty
                -- Return the score.
                pure (s > 0)
        -- If not enough cards to recurse, just compare the first two cards.
        else pure (x > y)
      -- Collect the cards and move to the next round.
      modify $ S.insert (xs,ys)
      if p1wins
        then go2 depth ((xs'++[x,y]), ys')
        else go2 depth (xs', (ys'++[y,x]))
      
  where
    align = putStr (replicate depth ' ')
    log s | logEnabled = liftIO $ align >> putStrLn s
          | otherwise = pure ()

main = do
  print $ go p1 p2
  print =<< evalStateT (go2 0 (p1,p2)) S.empty
