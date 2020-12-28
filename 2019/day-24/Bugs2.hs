import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

-- Real input
input = "..###\n.####\n...#.\n.#..#\n#.###"
-- Example input
--input = "....#\n#..#.\n#..##\n..#..\n#...."

--      |     |         |     |     
--   1  |  2  |    3    |  4  |  5  
--      |     |         |     |     
-- -----+-----+---------+-----+-----
--      |     |         |     |     
--   6  |  7  |    8    |  9  |  10 
--      |     |         |     |     
-- -----+-----+---------+-----+-----
--      |     |A|B|C|D|E|     |     
--      |     |-+-+-+-+-|     |     
--      |     |F|G|H|I|J|     |     
--      |     |-+-+-+-+-|     |     
--  11  | 12  |K|L|?|N|O|  14 |  15 
--      |     |-+-+-+-+-|     |     
--      |     |P|Q|R|S|T|     |     
--      |     |-+-+-+-+-|     |     
--      |     |U|V|W|X|Y|     |     
-- -----+-----+---------+-----+-----
--      |     |         |     |     
--  16  | 17  |    18   |  19 |  20 
--      |     |         |     |     
-- -----+-----+---------+-----+-----
--      |     |         |     |     
--  21  | 22  |    23   |  24 |  25 
--      |     |         |     |     

-- Row, column, depth
-- When you go into the centre cell, you go "deeper"
-- So (0,1,0) is adjacent to (-2,2,1), (-1,2,1), (0,2,1), (1,2,1), (2,2,1)
--   as well as (-1,1,0), (1,1,0), (0,2,0)
-- i.e N is adjacent to the right edge of the inner "?" grid
type Coord = (Int,Int,Int)
type Grid = S.Set Coord

initial :: Grid
initial = S.fromList $ do
            (r,l) <- zip [-2,-1,0,1,2] (lines input)
            (c,x) <- zip [-2,-1,0,1,2] l
            guard (x=='#')
            pure (r,c,0)

step :: Grid -> Grid
step bugs =
    let adjacents = S.unions (map neighbours (S.toList bugs))
        empties = adjacents S.\\ bugs
        bugF n = n == 1
        empF n = n == 1 || n == 2
    in S.union (S.filter (\c -> bugF (S.size (neighbours c `S.intersection` bugs))) bugs)
               (S.filter (\c -> empF (S.size (neighbours c `S.intersection` bugs))) empties)

neighbours :: Coord -> S.Set Coord
neighbours (r,c,d) = S.fromList $ concatMap (expand (r,c,d)) [(r-1,c,d),(r+1,c,d),(r,c-1,d),(r,c+1,d)]

expand _ (-3,_,d) = [(-1, 0, d-1)]
expand _ ( 3,_,d) = [( 1, 0, d-1)]
expand _ (_,-3,d) = [( 0,-1, d-1)]
expand _ (_, 3,d) = [( 0, 1, d-1)]

expand ( 1, 0,d) (0,0,_) = [ ( 2,c,d+1) | c <- [-2..2] ]
expand (-1, 0,d) (0,0,_) = [ (-2,c,d+1) | c <- [-2..2] ]
expand ( 0,-1,d) (0,0,_) = [ (r,-2,d+1) | r <- [-2..2] ]
expand ( 0, 1,d) (0,0,_) = [ (r, 2,d+1) | r <- [-2..2] ]

expand _ c = [c]

testNeighbours c = all (\c' -> S.member c (neighbours c')) (S.toList (neighbours c))
testNeighbours2 = all testNeighbours [ (r,c,d) | r <- [-2..2], c <- [-2..2], d <- [-1..1], (r /= 0 || c /= 0) ]

main = print . S.size $ iterate' step initial !! 200
