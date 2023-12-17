import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

main = do
  ls <- lines <$> readFile "17.txt"
  let m = M.fromList [ ((r,c),read [x]) | (r,l) <- zip [0..] ls, (c,x) <- zip [0..] l ] :: M.Map (Int,Int) Int
      q = S.fromList [Node (m M.! (1,0)) (1,0) (1,0) 1, Node (m M.! (0,1)) (0,1) (0,1) 1]
  mapM_ (\p -> print $ best $ execState (search p m) (St Nothing q M.empty)) [1,2]

data St = St {
      best :: Maybe Int
    , queue :: S.Set Node
    , seen :: M.Map ((Int,Int),(Int,Int),Int) Int
    }
  deriving (Show)

data Node = Node {
      cost :: Int
    , coord :: (Int, Int)
    , direction :: (Int, Int)
    , moves :: Int
    }
  deriving (Eq, Ord, Show)

search :: Int -> M.Map (Int,Int) Int -> State St ()
search part grid = do
  St mb q sn <- get
  case S.minView q of
    Nothing -> pure ()
    Just (node, q') -> do
      let key = (coord node, direction node, moves node)
      if (maybe True (\b -> cost node < b) mb)
      then do
        if finish part grid node
        then put (St (Just (cost node)) q' sn) >> search part grid
        else if maybe False (\c -> cost node >= c) (M.lookup key sn)
             then put (St mb q' sn) >> search part grid
             else do
               let ns = S.fromList $ neighbours part grid node
               put (St mb (S.union ns q') (M.insert key (cost node) sn))
               search part grid
      else put (St mb q' sn) >> search part grid

finish part grid node = fst (M.findMax grid) == coord node && (if part == 2 then moves node >= 4 else True)

neighbours part grid node = do
  (dr,dc) <- [(0,1),(0,-1),(1,0),(-1,0)]
  let (dr0,dc0) = direction node
  guard ((dr+dr0,dc+dc0) /= (0,0))
  if part == 1
  then guard (moves node < 3 || ((dr,dc) /= (dr0,dc0)))
  else do guard (if moves node < 4 then ((dr,dc) == (dr0,dc0)) else True)
          guard (if moves node == 10 then ((dr,dc) /= (dr0,dc0)) else True)
  let (r0,c0) = coord node
  let (r,c) = (r0+dr,c0+dc)
  guard (M.member (r,c) grid)
  let cst = cost node + grid M.! (r,c)
      mvs = if (dr0,dc0) == (dr,dc) then moves node + 1 else 1
  pure $ Node {
             cost = cst
           , coord = (r,c)
           , direction = (dr,dc)
           , moves = mvs
           }
