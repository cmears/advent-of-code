import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Text.Printf

import Debug.Trace

ex = unlines ["#######",
              "#E..G.#",
              "#...#.#",
              "#.G.#G#",
              "#######"]

ex2 = unlines [
       "#########",
       "#G..G..G#",
       "#.......#",
       "#.......#",
       "#G..E..G#",
       "#.......#",
       "#.......#",
       "#G..G..G#",
       "#########"]

ex3 = unlines ["#######",
               "#.G...#",
               "#...EG#",
               "#.#.#G#",
               "#..G#E#",
               "#.....#",
               "#######"]

main = do
  grid <- parseInput <$> readFile "15.txt"
  let countElves g = length $ do
                       (Creature Elf h) <- M.elems g
                       pure ()
  let initialElves = countElves grid
  print $ initialElves

  let loop :: Int -> IO ()
      loop elfAttackPower = do
        let grids = executeWholeCombat elfAttackPower grid
            final = last grids
            numFullRounds = length grids - 2
            remainingHealth = sum $ do
                               (Creature _ h) <- M.elems (last grids)
                               pure h
            numRemainingElves = countElves final
            outcome = numFullRounds * remainingHealth
        printf "attack=%d  numRemainingElves=%d  outcome=%d\n" elfAttackPower numRemainingElves outcome
        when (numRemainingElves < initialElves) (loop (elfAttackPower+1))
  loop 3

data Object = Wall
            | Creature Species Health

data Species = Elf | Goblin
  deriving (Eq, Show)

type Health = Int

-- (row, column)
type Coord = (Int,Int)

type Grid = Map Coord Object

parseInput :: String -> Grid
parseInput s = M.fromList $ do
                 (r,l) <- zip [0..] (lines s)
                 (c,x) <- zip [0..] l
                 let y = case x of
                           '#' -> Just Wall
                           'G' -> Just (Creature Goblin 200)
                           'E' -> Just (Creature Elf 200)
                           '.' -> Nothing
                           _ -> error "?"
                 Just z <- pure y
                 pure ((r,c),z)

showGrid :: Grid -> String
showGrid g =
    let coords = M.keys g
        maxR = maximum (fst <$> coords)
        maxC = maximum (snd <$> coords)
        showLine r =
          let (letters, annotations) = unzip $ do
                c <- [0..maxC]
                pure $ case M.lookup (r,c) g of
                   Nothing -> ('.', Nothing)
                   Just Wall -> ('#', Nothing)
                   Just (Creature species health) ->
                       (head (showSpecies species),
                        Just ("  "++showSpecies species++"("++show health++")"))
          in letters ++ concat (catMaybes annotations)
    in unlines $ map showLine [0..maxR]

showSpecies :: Species -> String
showSpecies Elf = "E"
showSpecies Goblin = "G"

targets :: Species -> Grid -> [Coord]
targets species g =
  map fst $ filter (isCreatureOfSpecies (enemy species) . snd) (M.toList g)

-- A destination is a square "in range" of a target
destinations :: [Coord] -> Grid -> [Coord]
destinations ts g =
  filter (\coord -> not (M.member coord g)) $ nub $ (concatMap neighbours ts)


-- From a coordinate, find the shortest path (with top-left tie
-- breaking) to each reachable coordinate.
pathMap :: Coord -> Grid -> Map Coord [Coord]
pathMap origin g =
  let originNeighbours = [ (c,[c,origin]) | c <- neighbours origin ]    
  in loop (Seq.fromList originNeighbours) M.empty
  where
    loop :: Seq.Seq (Coord, [Coord]) -> Map Coord [Coord] -> Map Coord [Coord]
    loop queue acc =
      case Seq.viewl queue of
        Seq.EmptyL -> acc
        (coord,path) Seq.:< queue' ->
            case M.member coord acc of
                True -> loop queue' acc
                False -> 
                    case M.lookup coord g of
                      Just _ -> loop queue' acc
                      Nothing -> 
                          let ns = [ (c,c:path) | c <- neighbours coord ]
                          in loop (queue' <> Seq.fromList ns) (M.insert coord path acc)

data Decision = Move Coord
              | NoTargets
              | Unreachable
              | AlreadyInRange
  deriving (Show)

move :: Coord -> Species -> Grid -> Decision
move coord species g =
  case targets species g of
    [] -> NoTargets
    ts -> 
        case destinations ts g of
          [] -> Unreachable
          ds ->
              case any (\c -> holdsSpecies (enemy species) c g) (neighbours coord) of
                True -> AlreadyInRange
                False ->
                    let m = pathMap coord g
                    in case catMaybes (map (\d -> M.lookup d m) ds) of
                         [] -> Unreachable
                         paths -> let bestLength = minimum (length <$> paths)
                                      bestPaths = filter (\p -> length p == bestLength) paths
                                      bestPath = head (sortOn head bestPaths)
                                  in Move (last (init bestPath))

holdsSpecies :: Species -> Coord -> Grid -> Bool
holdsSpecies species coord g =
    case M.lookup coord g of
      Just (Creature s _) | species == s -> True
      _ -> False

-- Order here is important: must be up, left, right, down
-- (for shortest path tiebreaking)
neighbours (r,c) = [(r-1,c),(r,c-1),(r,c+1),(r+1,c)]

data Status = StillGoing | GameOver

move' c s g =
    let d = move c s g
    in trace (show (c,s,d)) d

attack :: Int -> Coord -> Species -> Grid -> Grid
attack eap coord species g =
    let enemiesWithinReach = do
          c <- neighbours coord
          Just (Creature s h) <- pure $ M.lookup c g
          guard (s == enemy species)
          pure (c, Creature s h)
    in case enemiesWithinReach of
         [] -> g --trace (show coord ++ " no enemies in reach") $ g
         es -> let minHealth = minimum $ map (\(c,Creature s h) -> h) es
                   weakest = filter (\(c,Creature s h) -> h == minHealth) es
                   (victimCoord, Creature s h) = head weakest
                   attackPower = if species == Elf then eap else 3
                   h' = h - attackPower
               in --trace (show coord ++ " attacking " ++ show victimCoord) $
                  if h' <= 0
                  then M.delete victimCoord g
                  else M.insert victimCoord (Creature s h') g


takeTurn :: Int -> Coord -> Grid -> (Status, Grid)
takeTurn eap coord g =
    case M.lookup coord g of
      Nothing -> (StillGoing, g)
      Just Wall -> error "?"
      Just (Creature species health) ->
          let (status,coord',g') = case move coord species g of
                                     Move coord' -> (StillGoing, coord', M.delete coord . M.insert coord' (Creature species health) $ g)
                                     NoTargets -> (GameOver, coord, g)
                                     Unreachable -> (StillGoing, coord, g)
                                     AlreadyInRange -> (StillGoing, coord, g)
              g2 = attack eap coord' species g'
          in (status, g2)

executeWholeRound :: Int -> Grid -> (Status, Grid)
executeWholeRound eap g =
    let creatureCoords = sort $ map fst $ filter (\(coord,obj) -> isCreature obj) $ M.toList g
    in loop creatureCoords g
  where
    loop [] g' = (StillGoing, g')
    loop (c:cs) g' = case takeTurn eap c g' of
                       (StillGoing, g2) -> loop cs g2
                       (GameOver, g2) -> (GameOver, g2)

executeWholeCombat :: Int -> Grid -> [Grid]
executeWholeCombat eap g = g :
    case executeWholeRound eap g of
      (StillGoing, g') -> executeWholeCombat eap g'
      (GameOver, g') -> [g']

isCreature (Creature _ _) = True
isCreature _ = False

isCreatureOfSpecies :: Species -> Object -> Bool
isCreatureOfSpecies s (Creature t _) | s == t = True
isCreatureOfSpecies _ _ = False
    
enemy Elf = Goblin
enemy Goblin = Elf
