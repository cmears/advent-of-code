import Data.List
import Data.List.Split
import qualified Data.Set as S
import Debug.Trace
import Text.Printf

parse :: String -> (Bool, ((Integer,Integer),(Integer,Integer),(Integer,Integer)))
parse line =
    let [onoff, range] = words line
        [x,y,z] = map (\s -> let [a,b] = splitOn ".." (drop 2 s) in (read a,read b)) (splitOn "," range)
    in (onoff == "on", (x,y,z))

main = do
  cubes <- map parse . lines <$> readFile "input22"
  let f (a,b) = [a..b]
      g (xr,yr,zr) = S.fromAscList [ (x,y,z) | x <- f xr, y <- f yr, z <- f zr ]
  let p1 = filter (\(_, (x,y,z)) -> all (\(a,b) -> -50 <= a && b <= 50) [x,y,z])
--  let p1 = const $ [
--            (True,((-20,26),(-36,17),(-47,7))),
--             (True,((0,0),(0,11),(-26,28))),
-- --            (True,((-22,28),(-29,23),(-38,16))),
--             (True,((-46,0),(-6,40),(-50,-1))),
-- --            (True,((-49,1),(-3,46),(-24,28))),
-- --            (True,((2,47),(-22,22),(-23,27))),
--             (True,((-27,0),(-28,26),(-21,29))),
--             (True,((-39,5),(-6,45),(-2,0))),
-- --            (True,((-30,21),(-8,43),(-13,34))),
-- --            (True,((-22,26),(-27,20),(-29,19))),
-- --            (False,((-48,-32),(26,41),(-47,-37))),
-- --            (True,((-12,35),(6,50),(-50,-2))),
-- --            (False,((-48,-32),(-32,-16),(-15,-5))),
-- --            (True,((-18,26),(-33,15),(-7,46))),
-- --            (False,((-40,-22),(-38,-28),(23,41))),
-- --            (True,((-16,35),(-41,10),(-47,6))),
--             (False,((-32,0),(11,30),(-4,0)))
--            ]
  -- let s = foldl (\acc (b,(x,y,z)) -> (if b then S.union else S.difference) acc (g (x,y,z))) S.empty (p1 cubes)
  -- print $ S.size s
  let xxx = processAll [] cubes
  print $ sum $ map volume xxx
--  p2 (p1 cubes)
  -- let s2 = S.fromList $ concatMap (\(x,y,z) -> (,,) <$> f x <*> f y <*> f z) xxx
  -- print $ S.size s2
  -- print $ S.difference s s2
  -- print $ S.difference s2 s

p2 cs = do
  let xxx = processAll [] cs
  print $ sum $ map volume xxx
  

volume (x,y,z) = f x * f y * f z
  where f (l,u) = u-l+1

-- process existing (b,new) = loop existing [new]
--   where
--     loop1 [] n = if b then n else []
--     loop1 (e:es) ns =
--         let (e',ns') = loop2 [e] ns
--         in e' ++ loop1 es ns'

--     loop2 es [] = es
--     loop2 [] ns = if b then ns else []
--     loop2 ((x1,y1,z1):es) ((x2,y2,z2):ns) =
--         case map intersection [(x1,x2),(y1,y2),(z1,z2)] of
--           [Just xi, Just yi, Just zi] ->
--               if b && (xi,yi,zi) == (x2,y2,z2) then (x1,y1,z1):loop es ns
--               else if not b && (xi,yi,zi) == (x1,y1,z1) then loop es ns
--                    else let ones = cleave (xi,yi,zi) (x1,y1,z1)
--                             twos = cleave (xi,yi,zi) (x2,y2,z2)
--                         in loop (ones++es) (twos++ns)
--           _ -> (x1,y1,z1)loop2 ((x1,y1,z1):es) ns
-- concatMap (cleave (xi,yi,zi)) [(x1,y1,z1), (x2,y2,z2)]
--           _ -> [(x1,y1,z1), (x2,y2,z2)]

--processAll cubes bcubes | trace (printf "processAll: (%s,%s)\n" (show cubes) (show bcubes)) False = undefined
--processAll cubes bcubes | trace (printf "processAll: (%d,%d)\n" (length cubes) (length bcubes)) False = undefined
processAll cubes [] = cubes
processAll cubes (bcube:bcubes) =
    let result = process cubes bcube
    in --trace (printf "process %s %s == %s\n" (show (nub cubes)) (show bcube) (show result)) $
        case result of
         Updated cubes' -> processAll cubes' bcubes
         Split (c,cs) bcs -> processAll (cs ++ delete c cubes) (bcs ++ bcubes)


type Cube = ((Integer,Integer),(Integer,Integer),(Integer,Integer))
type BCube = (Bool, Cube)
data Result = Updated [Cube] | Split (Cube, [Cube]) [BCube]
  deriving (Show)

process :: [Cube] -> BCube -> Result
--process existing (b,(x2,y2,z2)) | trace (show (length existing)) False = undefined
process existing (b,(x2,y2,z2)) = loop existing []
  where
    loop [] acc = if b then Updated ((x2,y2,z2):acc) else Updated acc
    loop ((x1,y1,z1):es) acc =
        case map intersection [(x1,x2),(y1,y2),(z1,z2)] of
          [Just xi, Just yi, Just zi] ->
              if b && (xi,yi,zi) == (x2,y2,z2) then Updated existing
              else if not b && (xi,yi,zi) == (x1,y1,z1) then loop es acc
                   else let ones = cleave (xi,yi,zi) (x1,y1,z1)
                            twos = cleave (xi,yi,zi) (x2,y2,z2)
                        in Split ((x1,y1,z1), ones) (map (b,) twos)
          _ -> loop es ((x1,y1,z1):acc)
          
          
    

-- subcubes (x1,y1,z1) (x2,y2,z2) =
--     case map intersection [(x1,x2),(y1,y2),(z1,z2)] of
--       [Just xi, Just yi, Just zi] -> concatMap (cleave (xi,yi,zi)) [(x1,y1,z1), (x2,y2,z2)]
--       _ -> [(x1,y1,z1), (x2,y2,z2)]

intersection ((l1,u1),(l2,u2)) | u1 < l2 = Nothing
                               | u2 < l1 = Nothing
                               | otherwise = Just (max l1 l2, min u1 u2)

cleave (xi,yi,zi) (x,y,z) =
--  trace (printf "cleave: %s %s %s\n" (show (xi,yi,zi)) (show (x,y,z)) (show result)) $
    if sum (map volume result) == volume (x,y,z)
    then result
    else error "!!!"
  where result = do
          let f (il,iu) (l,u) =
                  if l == il && iu == u
                  then [(l,u)]
                  else filter nonzero [(l,il-1), (il,iu), (iu+1,u)]
          x' <- f xi x
          y' <- f yi y
          z' <- f zi z
          pure (x',y',z')

nonzero (l,u) = l <= u
