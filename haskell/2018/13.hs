{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Debug.Trace
import Data.Maybe

type Coord = (Int, Int)

type S = (M.Map Coord Char, [(Coord, Char, Int)])

main :: IO ()
main = do
    linePairs <- zip [0..] . lines <$> readFile "13.txt"
    let pairs :: [(Coord, Char)]
        pairs = [((x,y),c) | (y,l) <- linePairs, (x,c) <- zip [0..] l]
    let s = parsePairs pairs
    print $ simulate1 s
    print $ simulate2 s

parsePairs :: [(Coord, Char)] -> S
parsePairs pairs = loop pairs (M.empty, [])
  where
    loop [] s = s
    loop ((coord,c):ps) (m,l) =
        let s' = case c of
                    '-' -> (M.insert coord '-' m, l)
                    '|' -> (M.insert coord '|' m, l)
                    '+' -> (M.insert coord '+' m, l)
                    '/' -> (M.insert coord '/' m, l)
                    '\\' -> (M.insert coord '\\' m, l)
                    '>' -> (M.insert coord '-' m, (coord,'>',0):l)
                    '<' -> (M.insert coord '-' m, (coord,'<',0):l)
                    '^' -> (M.insert coord '|' m, (coord,'^',0):l)
                    'v' -> (M.insert coord '|' m, (coord,'v',0):l)
                    _ -> (m,l)
        in loop ps s'

simulate1 :: S -> Coord
simulate1 (m,l) = --trace (showS (m,l)) $
  --trace (show l) $
    let occupied = S.fromList (map (\(coord,_,_) -> coord) l)
        toProcess = sortOn (\((x,y),c,t) -> (y,x)) l

    in case loop toProcess occupied [] of
         Left coord -> coord
         Right xs -> simulate1 (m,xs)
    where
        loop [] _ acc = Right acc
        loop ((coord,c,t):xs) occupied acc =
            let (coord',c',t') = move m (coord,c,t)
            in if S.member coord' occupied
                then Left coord'
                else loop xs (S.insert coord' (S.delete coord occupied)) ((coord',c',t'):acc)

simulate2 :: S -> Coord
simulate2 (m,[(coord,_,_)]) = coord
simulate2 (m,l) = -- trace (showS (m,l)) $
  --trace (show l) $
    let occupied = S.fromList (map (\(coord,_,_) -> coord) l)
        toProcess = sortOn (\((x,y),c,t) -> (y,x)) l
        indexed = M.fromList $ zip [0..] toProcess

    in simulate2 (m, M.elems (loop (M.keys indexed) indexed))
    where
        loop [] idx = idx
        loop (i:is) idx | isNothing (M.lookup i idx) = loop is idx
        loop (i:is) idx =
            let (coord,c,t) = idx M.! i
                (coord',c',t') = move m (coord,c,t)
            in case find (\j -> let (cc,_,_) = idx M.! j in cc == coord') (M.keys idx) of
                Just j -> loop is (M.delete i . M.delete j $ idx)
                Nothing -> loop is (M.insert i (coord',c',t') idx)

move :: M.Map Coord Char -> (Coord, Char, Int) -> (Coord, Char, Int)
move m ((x,y),c,t) =
    let coord' = case c of
                    '>' -> (x+1,y)
                    '<' -> (x-1,y)
                    '^' -> (x,y-1)
                    'v' -> (x,y+1)
        (c',t') = case ((m M.! coord'),c) of
                    ('-',c) -> (c,t)
                    ('|',c) -> (c,t)
                    ('+',c) -> case (c, t) of
                        ('>',0) -> ('^',1)
                        ('<',0) -> ('v',1)
                        ('^',0) -> ('<',1)
                        ('v',0) -> ('>',1)
                        (c,1) -> (c,2)
                        ('>',2) -> ('v',0)
                        ('<',2) -> ('^',0)
                        ('^',2) -> ('>',0)
                        ('v',2) -> ('<',0)
                    ('/','>') -> ('^',t)
                    ('/','<') -> ('v',t)
                    ('/','^') -> ('>',t)
                    ('/','v') -> ('<',t)
                    ('\\','>') -> ('v',t)
                    ('\\','<') -> ('^',t)
                    ('\\','^') -> ('<',t)
                    ('\\','v') -> ('>',t)
    in (coord', c',t')
        
showS :: S -> String
showS (m,l) =
    let coords = M.keys m
        maxX = maximum (map fst coords)
        maxY = maximum (map snd coords)
        m' = M.union (M.fromList (map (\(coord,c,t) -> (coord,c)) l)) m
    in unlines $ do
        y <- [0..maxY]
        let s = do
              x <- [0..maxX]
              case M.lookup (x,y) m' of
                Nothing -> pure ' '
                Just c -> pure c
        pure s