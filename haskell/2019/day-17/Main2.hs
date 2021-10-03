import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as M
import Text.Printf
import Debug.Trace

import IntCode

main = do
  (ExecutionFinished, outputs) <- runFile "input" []
  let chars = map (chr . fromIntegral) outputs
  let ls = lines chars
      m = M.fromList $ do
            (r,l) <- zip [0..] ls
            (c,x) <- zip [0..] l
            pure ((r,c), x)
  let intersections = findIntersections m
      alignments = map (\(x,y) -> x*y) intersections
  draw m intersections
--  print (sum alignments)
  let p = determinePath m
  print p
--  print m

p = [L,F 4,L,F 4,L,F 10,R,F 4,R,F 4,L,F 4,L,F 4,R,F 8,R,F 10,L,F 4,L,F 4,L,F 10,R,F 4,R,F 4,L,F 10,R,F 10,L,F 4,L,F 4,L,F 10,R,F 4,R,F 4,L,F 10,R,F 10,R,F 4,L,F 4,L,F 4,R,F 8,R,F 10,R,F 4,L,F 10,R,F 10,R,F 4,L,F 10,R,F 10,R,F 4,L,F 4,L,F 4,R,F 8,R,F 10]

compress p = do
  a <- reverse (inits p)
  guard (size a <= 20)
  guard (size a >= 4)
  let p' = gobble [a] p
  b <- reverse (inits p')
  guard (size b <= 20)
  guard (size b >= 4)
  guard (not (a `isPrefixOf` b || b `isPrefixOf` a))
  let p'' = gobble [a,b] p'
  c <- reverse (inits p'')
  guard (size c <= 20)
  guard (size c >= 4)
  guard (not (a `isPrefixOf` c || c `isPrefixOf` a
           || b `isPrefixOf` c || c `isPrefixOf` b))
  let p''' = gobble [a,b,c] p''
--  guard $ trace ("        " ++ show (showPath c, p''')) (null p''')
--  guard (null p''')
  guard (null p''')
  pure (a,b,c)

showPath p = intercalate "," (map showMove p)

showMove L = "L"
showMove R = "R"
showMove (F n) = show n

size = length . showPath

gobble words p =
    let candidates = filter (\x -> x `isPrefixOf` p) words
    in case candidates of
         [] -> p
         [c] -> gobble words (drop (length c) p)
         xs -> error ("choice! " ++ show xs)
  

interactive = do
  (es, outputs) <- runFile "input2" []
  print (map (chr . fromIntegral) outputs)
  loop es
  where
    loop ExecutionFinished = pure ()
    loop (ExecutionWaiting cont) = do
                input <- getLine
                let (es, outputs) = cont (map (fromIntegral . ord) (input ++ "\n"))
                print (map (chr . fromIntegral) outputs)
                print (last outputs)
                loop es
      

findIntersections m = M.keys (M.filterWithKey f m)
  where f (x,y) '#' = all (== (Just '#')) [ M.lookup k m | k <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)] ]
        f _ _ = False

determinePath m =
    let [((r,c),d)] = M.toList (M.filter (`elem` "^>v<") m)
    in consolidate $ determinePath' m (r,c) d

data Move = F Integer | L | R
  deriving (Show, Eq)

determinePath' m (r,c) d | canForward m (r,c) d = F 1 : determinePath' m (go (r,c) d) d
                         | canForward m (r,c) (turn L d) = L : determinePath' m (r,c) (turn L d)
                         | canForward m (r,c) (turn R d) = R : determinePath' m (r,c) (turn R d)
                         | otherwise = []

consolidate (F x : F y : rest) = consolidate (F (x+y) : rest)
consolidate (m:ms) = m : consolidate ms
consolidate [] = []

turn :: Move -> Char -> Char
turn L '^' = '<'
turn L '<' = 'v'
turn L 'v' = '>'
turn L '>' = '^'

turn R d = (turn L . turn L . turn L) d

go (r,c) '^' = (r-1,c)
go (r,c) 'v' = (r+1,c)
go (r,c) '>' = (r,c+1)
go (r,c) '<' = (r,c-1)

canForward m (r,c) d = M.lookup (go (r,c) d) m == Just '#'

canPath m (r,c) d [] = True
canPath m (r,c) d (x:xs) =
    case x of
      F 0 -> canPath m (r,c) d xs
      F n -> canForward m (r,c) d && canPath m (go (r,c) d) d (F (n-1):xs)
      t -> canPath m (r,c) (turn t d) xs

draw m intersections = do
  let coords = M.keys m
      maxC = maximum (map snd coords)
      maxR = maximum (map fst coords)
  forM_ [0..maxR] $ \r -> do
    forM_ [0..maxC] $ \c -> do
      printf "%c" (if (r,c) `elem` intersections then 'I' else m M.! (r,c))
    putStrLn ""

-- scaffolds = M.keys (M.filter (\t -> t `elem` "^v<>#") m)

-- points = [ (c,d) | c <- scaffolds, d <- "^v<>" ]

-- check p = length $ filter (\(c,d) -> canPath m c d p) points


-- 12345678901234567890

-- [
-- L,4,L,4,L,10,R,4,R,4,L,4,L,4,



-- R,8,R,10,

-- L,4,L,4,L,10,R,4,R,4,L,10,R,10,L,4,L,4,L,10,R,4,R,4,

--             L,10,R,10,R,4,L,4,L,4,R,8,R,10,R,4,L,10,R,10,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10]


-- L,4,R,8,R,10
-- L,4,L,4,L,10,R,4,R,4,L,4,L,4,R,6/8



-- ####^..................................
-- #......................................
-- #......................................
-- #......................................
-- ###########...###########..............
-- ..........#...#........................
-- ..........#...#........................
-- ..........#...#........................
-- ......#####...#........................
-- ......#.......#........................
-- ......#.......#.###########...#########
-- ......#.......#.#.........#...#.......#
-- ......#####...##I##.......#...#.......#
-- ..........#.....#.#.......#...#.......#
-- ....#####.#.....#.#.......#####.......#
-- ....#...#.#.....#.#...................#
-- ....#...#.#...##I##...................#
-- ....#...#.#...#.#.....................#
-- ....#...##I###I#I##...................#
-- ....#.....#...#.#.#...................#
-- ####I######.##I##.#...............#####
-- #...#.......#.#...#...............#....
-- #...#.......#.#...#...............#....
-- #...#.......#.#...#...............#....
-- #####.......##I###I####...........#....
-- ..............#...#...#...........#....
-- ..............####I###I##.........#....
-- ..................#...#.#.........#....
-- ..................#####.#.........#....
-- ........................#.........#....
-- ........................###########....

-- A: [L,F 4,L,F 4,L,F 10,R,F 4]
-- B: [R,F 4,L,F 4,L,F 4,R,F 8,R,F 10]
-- C: [R,F 4,L,F 10,R,F 10]

-- [A,B,A,C,A,C,B,C,C,B]
