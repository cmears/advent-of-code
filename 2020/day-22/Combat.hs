{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

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

-- Memory (configs seen this game, memory table)
go2 memory p1 [] = (score p1,snd memory)
go2 memory [] p2 = (-score p2,snd memory)
go2 memory (x:xs) (y:ys) -- | trace (show (M.size (snd memory),S.size (fst memory),x:xs,y:ys)) False = undefined
                         | S.member ((x:xs),(y:ys)) (fst memory) = win1 (snd memory)
                         | M.member ((x:xs),(y:ys)) (snd memory) = (snd memory M.! ((x:xs),(y:ys)), snd memory)
                         | x <= length xs && y <= length ys =
                             let (subscore,table') = go2 (S.empty,snd memory) (take x xs) (take y ys)
                                 table'' = M.insert (take x xs,take y ys) subscore table'
                             in if subscore > 0
                                then win1 table''
                                else win2 table''
                         | x > y = win1 (snd memory)
                         | x < y = win2 (snd memory)
  where
    win1 table' = go2 (memory' table') (xs ++ [x,y]) ys
    win2 table' = go2 (memory' table') xs (ys ++ [y,x])
    memory' table' = (S.insert ((x:xs),(y:ys)) (fst memory), M.union table' (snd memory))

type Config = ([Int],[Int])
type GameMemory = S.Set Config
type GlobalMemory = M.Map Config Int
type S = (GameMemory, GlobalMemory)
type St = StateT S IO
go3 :: Int -> Config -> St Int
go3 depth (xs,ys) = do
--  liftIO $ align >> print (xs,ys)
  (game,global) <- get
  case (xs,ys) of
    (_,[]) -> pure $ score xs
    _ | S.member (xs,ys) game -> pure $ score xs
    ([],_) -> pure $ -score ys
    (x:xs',y:ys') -> do
      if length xs' >= x && length ys' >= y
      then do liftIO $ align >> putStrLn "recursive game"
              subscore <- case M.lookup (take x xs', take y ys') global of
                            Just cached -> do liftIO $ align >> putStrLn "retrieved from cache"
                                              pure cached
                            Nothing -> do               
                              s <- fresh $ go3 (depth+2) (take x xs', take y ys')
                              record (take x xs', take y ys') s
                              pure s
              if subscore > 0
              then nextRound ((xs'++[x,y]), ys')
              else nextRound (xs', (ys'++[y,x]))
      else if x > y
           then nextRound ((xs'++[x,y]), ys')
           else nextRound (xs', (ys'++[y,x]))
      
  where
    nextRound :: Config -> St Int
    nextRound c = do
      (game,global) <- get
      put (S.insert (xs,ys) game, global)
      go3 depth c
    record :: Config -> Int -> St ()
    record c subscore = do
      (game,global) <- get
      put (game, M.insert c subscore global)
      liftIO $ align >> (putStrLn $ "recorded " ++ show (c,subscore))
    fresh :: St a -> St a
    fresh a = do
      (game,global) <- get
      put (S.empty, global)
      a
    align = putStr (replicate depth ' ')

--main = print $ go2 (S.empty,M.empty) p1 p2
main = print =<< evalStateT (go3 0 (p1,p2)) (S.empty, M.empty)
