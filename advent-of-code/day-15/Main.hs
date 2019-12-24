import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import Text.Printf

import IntCode

main = test2
--   program <- readFile "input"

--   let (es,os) = runProgram program []

-- --  loop (es,os) (M.singleton (0,0) Clear, (0,0)) []
--   pure ()




loop :: (MonadIO m) => StateT S m ()
loop = do
  draw
  coord <- getCurrentCoord
  currentTile <- getCurrentTile
  liftIO $ printf "I am at %s which is %s\n" (show coord) (show currentTile)
  if currentTile == Oxygen
    then do
      score <- getCurrentScore
      liftIO (print (coord, score))
      let initialMap = M.fromList [
                         ((0,0), (Clear, 0))
                       , ((1,0), (Unknown, 1))
                       , ((-1,0), (Unknown, 1))
                       , ((0,1), (Unknown, 1))
                       , ((0,-1), (Unknown, 1))
                       ]
      modify (\s -> s { _map = initialMap, _coord = (0,0) })
      loop2
    else do
      target <- fromJust <$> findBestTarget
      liftIO $ printf "I want to go to %s\n" (show target)
      path <- findPath coord target
      followPath path
      loop

loop2 :: (MonadIO m) => StateT S m ()
loop2 = do
  coord <- getCurrentCoord
  mt <- findBestTarget
  case mt of
    Nothing -> draw >> pure ()
    Just t -> do
      p <- findPath coord t
      followPath p
      loop2

data Tile = Wall | Clear | Oxygen | Unknown
  deriving (Eq, Show)

type Coord = (Integer, Integer)
data Direction = North | South | East | West
  deriving (Eq, Show)

data S = S {
      _coord :: Coord
    , _map :: M.Map Coord (Tile, Integer)
    , _robot :: ExecutionStatus
    }

getCurrentCoord :: Monad m => StateT S m Coord
getCurrentCoord = _coord <$> get

getCurrentTile :: Monad m => StateT S m Tile
getCurrentTile = do
  m <- _map <$> get
  c <- getCurrentCoord
  pure (fst (m M.! c))

getCurrentScore :: Monad m => StateT S m Integer
getCurrentScore = do
  m <- _map <$> get
  c <- getCurrentCoord
  pure (snd (m M.! c))

getRobot :: Monad m => StateT S m ExecutionStatus
getRobot = _robot <$> get

findBestTarget :: MonadIO m => StateT S m (Maybe Coord)
findBestTarget = do
  m <- _map <$> get
  let candidates = [ coord | (coord, (tile, _)) <- M.toList m, tile == Unknown ]
  c <- _coord <$> get
--  liftIO $ printf "findBestTarget: candidates are %s\n" (show candidates)
  pure $ case candidates of
           [] -> Nothing
           _ -> Just (minimumBy (comparing (manhattan c)) candidates)

manhattan (a,b) (x,y) = abs (a-x) + abs (b-y)

findPath :: Monad m => Coord -> Coord -> StateT S m [Direction]
findPath origin destination = do
  m <- _map <$> get
  pure (bfs origin destination m)

-- Shortest path from o to d, traversing only already-explored tiles.
bfs :: Coord -> Coord -> M.Map Coord (Tile, Integer) -> [Direction]
bfs o d m = bfs' (map (\(c,d') -> (c,[d'])) (ns o)) (S.singleton o)
  where
    bfs' ((c,path):cs) seen | c == d = reverse path
                            | S.member c seen = bfs' cs seen
                            | otherwise =
        case M.lookup c m of
          Nothing -> bfs' cs seen
          Just (Wall, _) -> bfs' cs seen
          Just _ -> bfs' (cs ++ (map (\(c,d') -> (c,d':path)) (ns c))) (S.insert c seen)
    ns c = [ (c',d) | d <- [North, South, West, East], let c' = neighbour c d ]

followPath :: Monad m => [Direction] -> StateT S m ()
followPath = mapM_ execute

execute :: Monad m => Direction -> StateT S m ()
execute d = do
  c <- getCurrentCoord
  let c' = neighbour c d
  r <- getRobot
  let input = 1 + fromIntegral (fromJust (d `elemIndex` [North, South, West, East]))
  let (es, [o]) = case r of
                    ExecutionWaiting continuation -> continuation [input]
  s <- get
  let m = _map s
  let bestScore = case M.lookup c' m of
                    Just (Unknown,_) -> 1 + snd (m M.! c)
                    Just (_,s) -> s
  let (m', c'') =
          case o of
            0 -> (M.insert c' (Wall, bestScore) m, c)
            1 -> (M.insert c' (Clear, bestScore) m, c')
            2 -> (M.insert c' (Oxygen, bestScore) m, c')

  let m'' = if c'' == c'
            then foldl' (\m c -> if isNothing (M.lookup c m) then M.insert c (Unknown, 0) m else m) m' (neighbours c'')
            else m'

  let s' = s { _map = m'', _coord = c'', _robot = es }
  put s'

neighbour :: Coord -> Direction -> Coord
neighbour (x,y) North = (x,y+1)
neighbour (x,y) South = (x,y-1)
neighbour (x,y) West = (x-1,y)
neighbour (x,y) East = (x+1,y)

neighbours c = [ neighbour c d | d <- [North, South, West, East] ]

draw :: MonadIO m => StateT S m ()
draw = do
  s <- get
  liftIO $ do
    let minX = -60
        maxX = 30
        minY = minimum [ y | (x,y) <- M.keys (_map s) ]
        maxY = maximum [ y | (x,y) <- M.keys (_map s) ]
    forM_ (reverse [ minY .. maxY ]) $ \y -> do
      forM_ [ minX .. maxX ] $ \x -> do
        case M.lookup (x,y) (_map s) of
          Nothing -> putStr " "
          Just (t,x) -> putStr $ case t of
                                   _ | _coord s == (x,y) -> "*"
                                   Wall -> " "
                                   Clear -> show ((x `div` 10) `mod` 10)
                                   Oxygen -> "O"
                                   Unknown -> "?"
      putStrLn ""

test2 = do
  program <- readFile "input"
  let (es,os) = runProgram program []
  let initialMap = M.fromList [
                     ((0,0), (Clear, 0))
                   , ((1,0), (Unknown, 1))
                   , ((-1,0), (Unknown, 1))
                   , ((0,1), (Unknown, 1))
                   , ((0,-1), (Unknown, 1))
                   ]
  let initialState = S { _coord = (0,0), _map = initialMap, _robot = es }
  s <- execStateT loop initialState
  let furthest = maximum $ map snd $ filter (\(t,_) -> t == Clear) (M.elems (_map s))
  print furthest

test = do
  program <- readFile "input"

  let (es,os) = runProgram program []

  let initialMap = M.fromList [
                     ((0,0), (Clear, 0))
                   , ((1,0), (Unknown, 1))
                   , ((-1,0), (Unknown, 1))
                   , ((0,1), (Unknown, 1))
                   , ((0,-1), (Unknown, 1))
                   ]
  let initialState = S { _coord = (0,0), _map = initialMap, _robot = es }
--  loop (es,os) (M.singleton (0,0) Clear, (0,0)) []
  
  flip execStateT initialState $ do
    let loop (d:ds) = do
          draw
          liftIO $ putStrLn ""
          execute d
          loop ds
    loop $ concat [ concatMap (replicate i) [North, East, South, West] | i <- [1..10] ]
    -- draw
    -- liftIO $ putStrLn ""
    -- followPath [North, East, South, West]
    -- draw
    

-- randomDir = do
-- --  i <- randomRIO (0,3)
--   let i = 0
--   pure ([North, South, West, East] !! i)

-- loop (ExecutionWaiting continuation, []) (m, coord) path =
--   if m M.! coord == Oxygen then (
--   case path of
--     [] -> 
--       let path' = chooseDir m coord
--       in 
--       (es', [o]) = continuation [dir]
--       (m', coord') = apply dir o m coord
--   in loop (es', []) (m', coord')

-- chooseDir m coord = loop [(coord, [])] S.empty
--   where
--     loop ((c,path):queue) seen =
--         case M.lookup c m of
--           Nothing -> path
--           Just Wall -> loop queue seen
--           Just Clear ->
--               let neighbours = [ (c',d) | d <- [1..4], let c' = neighbour c d ]
--                   neighbours' = filter (\(c',d) -> not (S.member c' seen)) neighbours
--               in loop (queue ++ neighbours') (S.union (S.fromList neighbours') seen)

-- apply dir output m coord =
--   let coord' = neighbour coord dir
--   in case output of
--        0 -> (M.insert coord' Wall m, coord)
--        1 -> (M.insert coord' Clear m, coord')
--        2 -> (M.insert coord' Oxygen m, coord')
  
    
