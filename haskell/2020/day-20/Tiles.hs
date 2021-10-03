import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Text.Regex.TDFA
import Debug.Trace

submatches :: String -> String -> Maybe [String]
submatches regex s =
  case getAllTextSubmatches (s =~ regex) of
    [] -> Nothing
    matches -> Just (tail matches)

type Grid = M.Map (Int, Int) Char

data Tile = Tile Int Grid
  deriving (Show)
tileID (Tile n _) = n

readTiles :: FilePath -> IO [Tile]
readTiles path = do
  c <- readFile path
  let ls = lines c
  let tiles = splitOn [""] ls
  pure (map readTile tiles)

readTile :: [String] -> Tile
readTile (h:xs) =
  let Just [n] = submatches "Tile ([0-9]+):" h
      m = M.fromList $ do
            (r,row) <- zip [1..] xs
            (c,x) <- zip [1..] row
            pure ((r,c),x)
  in Tile (read n) m

printTile :: Tile -> String
printTile (Tile n grid) =
  ("Tile " ++ show n ++ ":\n") ++
  (unlines $ do
    r <- [1..10]
    pure $ [grid M.! (r,c) | c <- [1..10]])


-- Let there be 8 orientations:
-- 0: identity
-- 1,2,3: rotations
-- 4: reflection (horiz)
-- 5,6,7: rotations of 4
orient o grid = M.mapKeys (f o) grid
  where
    f 0 (r,c) = (r,c)
    f 1 (r,c) = (c,11-r)
    f 2 (r,c) = f 1 (f 1 (r,c))
    f 3 (r,c) = f 1 (f 1 (f 1 (r,c)))
    f 4 (r,c) = (r,11-c)
    f 5 (r,c) = f 1 (f 4 (r,c))
    f 6 (r,c) = f 1 (f 1 (f 4 (r,c)))
    f 7 (r,c) = f 1 (f 1 (f 1 (f 4 (r,c))))

orientTile o (Tile n grid) = Tile n (orient o grid)

orientBig o grid = M.mapKeys (f o) grid
  where
    f 0 (r,c) = (r,c)
    f 1 (r,c) = (c,96-r)
    f 2 (r,c) = f 1 (f 1 (r,c))
    f 3 (r,c) = f 1 (f 1 (f 1 (r,c)))
    f 4 (r,c) = (r,96-c)
    f 5 (r,c) = f 1 (f 4 (r,c))
    f 6 (r,c) = f 1 (f 1 (f 4 (r,c)))
    f 7 (r,c) = f 1 (f 1 (f 1 (f 4 (r,c))))


-- Are two tiles left-right compatible?
horizCompat (Tile n1 grid1) (Tile n2 grid2) =
    -- Right edge of tile 1 must match left edge of tile 2.
    let right1 = [ grid1 M.! (r,10) | r <- [1..10] ]
        left2  = [ grid2 M.! (r,1)  | r <- [1..10] ]
    in right1 == left2

vertCompat (Tile n1 grid1) (Tile n2 grid2) =
    -- Bottom edge of tile 1 must match top edge of tile 2.
    let bottom1 = [ grid1 M.! (10,c) | c <- [1..10] ]
        top2    = [ grid2 M.! (1,c)  | c <- [1..10] ]
    in bottom1 == top2

type Relation = M.Map (Int,Int) [(Int,Int)]

horizRelation :: [Tile] -> Relation
horizRelation tiles = M.fromListWith (++) $ do
  t1 <- tiles
  o1 <- [0..7]
  let tile1 = orientTile o1 t1
  t2 <- tiles
  guard (tileID t1 /= tileID t2)
  o2 <- [0..7]
  let tile2 = orientTile o2 t2
  guard (horizCompat tile1 tile2)
  pure ((tileID t1,o1),[(tileID t2,o2)])

vertRelation :: [Tile] -> Relation
vertRelation tiles = M.fromListWith (++) $ do
  t1 <- tiles
  o1 <- [0..7]
  let tile1 = orientTile o1 t1
  t2 <- tiles
  guard (tileID t1 /= tileID t2)
  o2 <- [0..7]
  let tile2 = orientTile o2 t2
  guard (vertCompat tile1 tile2)
  pure ((tileID t1,o1),[(tileID t2,o2)])


assign :: [Int] -> Relation -> Relation -> [M.Map (Int,Int) (Int,Int)]
assign tileIDs h v = loop [ (r,c) | r <- [1..12], c <- [1..12] ] M.empty
  where
    loop :: [(Int,Int)] -> M.Map (Int,Int) (Int,Int) -> [M.Map (Int,Int) (Int,Int)]
--    loop coords m | trace (show (length coords)) False = undefined
    loop [] m = pure m
    loop ((r,c):coords) m = do
        let usedTIDs :: [Int]
            usedTIDs = map fst (M.keys m)
            candidates = [ (tid,o) | tid <- tileIDs, not (tid `elem` usedTIDs), o <- [0..7] ]
            filterH = case M.lookup (r,c-1) m of
                        Nothing -> const True
                        Just otherTidO -> (\(tid,o) -> (tid,o) `elem` (M.findWithDefault [] otherTidO h))
            filterV = case M.lookup (r-1,c) m of
                        Nothing -> const True
                        Just otherTidO -> (\(tid,o) -> (tid,o) `elem` (M.findWithDefault [] otherTidO v))
            candidates' = filter filterV . filter filterH $ candidates
        (tid,o) <- candidates'
        loop coords (M.insert (r,c) (tid,o) m)

seaMonster = [ (r,c) | (r,l) <- zip [0..] s, (c,x) <- zip [0..] l, x == '#' ]
  where
    s = [ "                  # "
        , "#    ##    ##    ###"
        , " #  #  #  #  #  #   "
        ]

glue assignment tiles =
  let gridMap = M.fromList [ (tid,grid) | Tile tid grid <- tiles ]
      bigGrid = M.fromList $ do
                  tr <- [1..12]
                  tc <- [1..12]
                  let (tid,o) = assignment M.! (tr,tc)
                  r <- [2..9]
                  c <- [2..9]
                  let x = (orient o (gridMap M.! tid)) M.! (r,c)
                  pure (((tr-1)*8+r-2,(tc-1)*8+c-2), x)
  in bigGrid

hunt grid = do
  (r,c) <- M.keys grid
  guard (all (\(rd,cd) -> M.findWithDefault '.' (r+rd,c+cd) grid == '#') seaMonster)
  pure [ (r+rd,c+cd) | (rd,cd) <- seaMonster ]
    

main = do
  tiles <- readTiles "input.txt"
  let h = horizRelation tiles
      v = vertRelation tiles
--      a = head $ assign (map tileID tiles) h v
      a = part1
--  mapM_ print $ M.toList h
--  mapM_ print $ M.toList v
  let n = product $ map (\(r,c) -> fst (a M.! (r,c))) [(1,1),(1,12),(12,1),(12,12)]
  print n
  
--  mapM_ print $ (chunksOf (12*8) $ M.elems (glue a tiles))

  let bigGrid0 = glue a tiles
      bigGrid = orientBig 0 bigGrid0
  let locations = hunt bigGrid
  print locations

  let hashes = [ k | (k,v) <- M.toList bigGrid, v == '#' ]
      waves = hashes \\ concat locations
  print waves
  print (length waves)

  



part1 = M.fromList [((1,1),(3343,3)),((1,2),(2729,6)),((1,3),(1531,5)),((1,4),(1543,6)),((1,5),(1319,5)),((1,6),(2851,5)),((1,7),(3137,5)),((1,8),(3001,6)),((1,9),(3673,2)),((1,10),(3389,7)),((1,11),(3163,7)),((1,12),(3677,3)),((2,1),(2693,7)),((2,2),(3923,4)),((2,3),(3571,5)),((2,4),(2671,7)),((2,5),(3491,2)),((2,6),(2161,1)),((2,7),(2081,3)),((2,8),(2969,3)),((2,9),(2131,3)),((2,10),(1931,1)),((2,11),(2753,4)),((2,12),(2003,1)),((3,1),(1451,4)),((3,2),(2711,5)),((3,3),(2213,3)),((3,4),(2503,7)),((3,5),(1381,4)),((3,6),(3701,3)),((3,7),(1759,3)),((3,8),(1877,1)),((3,9),(2713,2)),((3,10),(1049,4)),((3,11),(1823,7)),((3,12),(2179,1)),((4,1),(3847,5)),((4,2),(1129,3)),((4,3),(3019,1)),((4,4),(1879,2)),((4,5),(2377,1)),((4,6),(2311,2)),((4,7),(1913,7)),((4,8),(1777,1)),((4,9),(1789,4)),((4,10),(2297,6)),((4,11),(3613,2)),((4,12),(2339,7)),((5,1),(2347,1)),((5,2),(3541,5)),((5,3),(3917,3)),((5,4),(3391,3)),((5,5),(1933,1)),((5,6),(2203,5)),((5,7),(1889,2)),((5,8),(1103,6)),((5,9),(2903,3)),((5,10),(3793,6)),((5,11),(1163,7)),((5,12),(1327,5)),((6,1),(1249,7)),((6,2),(3907,6)),((6,3),(2411,7)),((6,4),(3329,4)),((6,5),(2341,2)),((6,6),(3853,1)),((6,7),(1489,4)),((6,8),(1831,7)),((6,9),(1607,2)),((6,10),(3259,4)),((6,11),(1453,6)),((6,12),(1481,1)),((7,1),(2687,2)),((7,2),(2833,3)),((7,3),(2437,4)),((7,4),(3323,2)),((7,5),(1493,2)),((7,6),(2879,6)),((7,7),(1663,5)),((7,8),(2647,6)),((7,9),(1259,7)),((7,10),(2371,1)),((7,11),(1223,6)),((7,12),(2683,5)),((8,1),(2089,3)),((8,2),(3733,4)),((8,3),(2579,5)),((8,4),(3499,7)),((8,5),(1021,6)),((8,6),(3803,6)),((8,7),(2837,2)),((8,8),(2927,7)),((8,9),(3767,3)),((8,10),(1597,7)),((8,11),(3881,6)),((8,12),(1973,6)),((9,1),(3373,2)),((9,2),(2473,2)),((9,3),(3631,1)),((9,4),(1367,1)),((9,5),(1231,7)),((9,6),(2521,7)),((9,7),(3989,5)),((9,8),(2971,4)),((9,9),(2441,1)),((9,10),(1031,1)),((9,11),(2609,3)),((9,12),(1283,3)),((10,1),(2129,4)),((10,2),(3299,1)),((10,3),(1483,1)),((10,4),(2273,5)),((10,5),(2719,3)),((10,6),(3533,7)),((10,7),(1321,1)),((10,8),(3413,2)),((10,9),(1867,2)),((10,10),(3943,7)),((10,11),(2207,5)),((10,12),(2593,5)),((11,1),(1123,6)),((11,2),(1373,2)),((11,3),(2153,4)),((11,4),(3643,1)),((11,5),(1579,5)),((11,6),(3593,1)),((11,7),(2657,3)),((11,8),(1549,3)),((11,9),(1811,6)),((11,10),(2333,3)),((11,11),(1667,5)),((11,12),(1871,6)),((12,1),(3821,4)),((12,2),(2113,5)),((12,3),(2039,4)),((12,4),(3457,2)),((12,5),(3109,2)),((12,6),(3889,3)),((12,7),(3359,2)),((12,8),(2141,2)),((12,9),(1999,5)),((12,10),(3217,7)),((12,11),(1619,3)),((12,12),(3709,6))]
