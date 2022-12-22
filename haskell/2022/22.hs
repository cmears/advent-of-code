import Control.Monad
import Data.Char
import Data.List.Split
import qualified Data.Map as M

type Coord = (Int,Int)
data Dir = R | D | L | U
  deriving (Show, Enum, Bounded)
type S = (Coord, Dir)

turn 'R' U = R
turn 'R' x = succ x
turn 'L' x = (turn 'R' . turn 'R' . turn 'R') x

apply R (r,c) = (r,c+1)
apply D (r,c) = (r+1,c)
apply L (r,c) = (r,c-1)
apply U (r,c) = (r-1,c)

type Maze = M.Map Coord Char
forward maze wrap (coord, dir) =
    let coord' = apply dir coord
        (coord'',d') = case M.lookup coord' maze of
                         Just '.' -> (coord',dir)
                         Just '#' -> (coord,dir)
                         Nothing -> let (wrapped,d) = wrap maze coord dir
                                    in case M.lookup wrapped maze of
                                         Just '.' -> (wrapped,d)
                                         Just '#' -> (coord,dir)
    in (coord'', d')

wrap1 maze coord R = (minimum (filter (\(r,c) -> r == fst coord) (M.keys maze)), R)
wrap1 maze coord L = (maximum (filter (\(r,c) -> r == fst coord) (M.keys maze)), L)
wrap1 maze coord D = (minimum (filter (\(r,c) -> c == snd coord) (M.keys maze)), D)
wrap1 maze coord U = (maximum (filter (\(r,c) -> c == snd coord) (M.keys maze)), U)

--    AB
--    C
--   ED
--   F

-- B D
wrap2 maze (n,150) R =                        ((150-n+1,100), L)
wrap2 maze (n,100) R | 101 <= n && n <= 150 = ((150-n+1,150), L)

-- C B
wrap2 maze (n,100) R | 51 <= n && n <= 100 = ((50,n+50), U)
wrap2 maze (50,n) D =                        ((n-50,100),L)

-- F D
wrap2 maze (n,50) R =  ((150,n-100),U)
wrap2 maze (150,n) D = ((100+n,50),L)

-- E A
wrap2 maze (n,1) L | 101 <= n && n <= 150 = ((150-n+1,51),R)
wrap2 maze (n,51) L | 1 <= n && n <= 50 =   ((150-n+1,1),R)

-- F A
wrap2 maze (n,1) L | 151 <= n && n <= 200 = ((1,n-100),D)
wrap2 maze (1,n) U | 51 <= n && n <= 100 =  ((n+100,1),R)

-- C E
wrap2 maze (n,51) L | 51 <= n && n <= 100 = ((101,n-50),D)
wrap2 maze (101,n) U =                      ((n+50,51),R)

-- B F
wrap2 maze (1,n) U | 101 <= n && n <= 150 = ((200,n-100),U)
wrap2 maze (200,n) D =                      ((1,n+100),D)

execute maze wrap [] s = s
execute maze wrap ('L':rest) s = execute maze wrap rest (fst s, turn 'L' (snd s))
execute maze wrap ('R':rest) s = execute maze wrap rest (fst s, turn 'R' (snd s))
execute maze wrap inst s = let (n, rest) = span isDigit inst
                           in execute maze wrap rest (iterate (forward maze wrap) s !! (read n))

main = do
  [mazeLines,[instructions]] <- splitOn [""] . lines <$> readFile "22.txt"
  let maze = M.fromList $ do
         (r,l) <- zip [1..] mazeLines
         (c,x) <- zip [1..] l
         guard (x /= ' ')
         pure ((r,c),x)

  let initS = (fst (head (filter ((=='.').snd) (M.toList maze))), R)

  let ((r,c),d) = execute maze wrap1 instructions initS
  print $ 1000*r + 4*c + fromEnum d

  let ((r,c),d) = execute maze wrap2 instructions initS
  print $ 1000*r + 4*c + fromEnum d
