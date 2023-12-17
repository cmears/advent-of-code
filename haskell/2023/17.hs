import Control.Monad
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

main = do
  ls <- lines <$> readFile "17.txt"
  let m = M.fromList [ ((r,c),read [x]) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ] :: M.Map (Int,Int) Int
      n1 = Node 0 (m M.! (1,0)) (1,0) (1,0) 1
      n2 = Node 0 (m M.! (0,1)) (0,1) (0,1) 1
      q = S.fromList [n1,n2]
      t = fst (M.findMax m)
      hl = heurloop m M.empty (S.fromList [(m M.! t, t)])
      (i,s) = runState (search 0 m hl) (St Nothing q M.empty)
--  print hl
  print (best s)
  print i


data St = St {
      best :: Maybe Int
    , queue :: S.Set Node
    , seen :: M.Map ((Int,Int),(Int,Int),Int) Int
    }
  deriving (Show)

data Node = Node {
      score :: Int
    , cost :: Int
    , coord :: (Int, Int)
    , direction :: (Int, Int)
    , moves :: Int
    }
  deriving (Eq, Ord, Show)

type S = State St

search :: Int -> M.Map (Int,Int) Int -> M.Map (Int,Int) Int -> S Int
search i grid hm = do
  St mb q sn <- get
--  trace (show (mb,q)) $
  case S.minView q of
    Nothing -> pure i
    Just (node, q') -> (if (i `mod` 1000 == 0 && False) then trace (show (mb,S.size q',node)) else id) $ do
      if (maybe True (\b -> cost node < b) mb)
      then do
        if finish grid (coord node) -- && (moves node >= 4)
        then put (St (Just (cost node)) q' sn) >> search (i+1) grid hm
        else if maybe False (\c -> cost node >= c) (M.lookup (coord node, direction node, moves node) sn)
             then put (St mb q' sn) >> search (i+1) grid hm
             else do
               let ns = neighbours grid hm node
               put (St mb (S.union (S.fromList ns) q') (M.insert (coord node, direction node, moves node) (cost node) sn))
               search (i+1) grid hm
      else put (St mb q' sn) >> search (i+1) grid hm

finish grid (r,c) = fst (M.findMax grid) == (r,c)

neighbours grid hm node = do
  (dr,dc) <- [(0,1),(0,-1),(1,0),(-1,0)]
  let (dr0,dc0) = direction node
  guard ((dr+dr0,dc+dc0) /= (0,0))
  guard (moves node < 3 || ((dr,dc) /= (dr0,dc0)))
  let (r0,c0) = coord node
  let (r,c) = (r0+dr,c0+dc)
  guard (M.member (r,c) grid)
  let cst = cost node + grid M.! (r,c)
      scr = cst + heur grid hm (r,c)
      mvs = if (dr0,dc0) == (dr,dc) then moves node + 1 else 1
  pure $ Node {
             score = scr
           , cost = cst
           , coord = (r,c)
           , direction = (dr,dc)
           , moves = mvs
           }

neighbours2 grid hm node = do
  (dr,dc) <- [(0,1),(0,-1),(1,0),(-1,0)]
  let (dr0,dc0) = direction node
  guard ((dr+dr0,dc+dc0) /= (0,0))
  guard (if moves node < 4 then ((dr,dc) == (dr0,dc0)) else True)
  guard (if moves node == 10 then ((dr,dc) /= (dr0,dc0)) else True)
  let (r0,c0) = coord node
  let (r,c) = (r0+dr,c0+dc)
  guard (M.member (r,c) grid)
  let cst = cost node + grid M.! (r,c)
      scr = cst + heur grid hm (r,c)
      mvs = if (dr0,dc0) == (dr,dc) then moves node + 1 else 1
  pure $ Node {
             score = scr
           , cost = cst
           , coord = (r,c)
           , direction = (dr,dc)
           , moves = mvs
           }

--heur grid hm (r,c) = 0
heur grid hm (r,c) = hm M.! (r,c)
    -- let (rt,ct) = fst (M.findMax grid)
    --     dist = (rt-r) + (ct-c)
    -- in dist
        

heurloop :: M.Map (Int,Int) Int -> M.Map (Int,Int) Int -> S.Set (Int, (Int,Int)) -> M.Map (Int,Int) Int
heurloop grid acc frontier =
    case S.minView frontier of
      Nothing -> acc
      Just ((cst,(r,c)),front') ->
          let acc' = M.insert (r,c) cst acc
              cst' = cst + grid M.! (r,c)
              ns = do (dr,dc) <- [(1,0),(0,1),(-1,0),(0,-1)]
                      let (r',c') = (r+dr,c+dc)
                      guard (M.member (r',c') grid)
                      guard (not (M.member (r',c') acc'))
                      pure (cst',(r+dr,c+dc))
          in heurloop grid acc' (S.union (S.fromList ns) front')
