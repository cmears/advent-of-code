import Control.Monad
import qualified Data.Set as S

main = pure ()

input = lines "##...#.#\n####.#.#\n#...####\n..#.#.#.\n####.#..\n#.#.#..#\n.####.##\n..#...##"

initial = S.fromList $ do
  (y,l) <- zip [0..] input
  (x,c) <- zip [0..] l
  guard (c == '#')
  pure (x,y,0,0)

neighbours (x,y,z,w) = do
  x' <- [-1,0,1]
  y' <- [-1,0,1]
  z' <- [-1,0,1]
  w' <- [-1,0,1]
  guard (x' /= 0 || y' /= 0 || z' /= 0 || w' /= 0)
  pure (x+x',y+y',z+z',w+w')

possible s = S.unions (s : [ S.fromList (neighbours c) | c <- S.toList s ])

step s = S.fromList $ do
  c <- S.toList (possible s)
  let active = S.member c s
      neighbouringStates = map (\c' -> S.member c' s) (neighbours c)
      n = length (filter id neighbouringStates)
  guard $ (active && n == 2) || n == 3
  pure c

part1 = S.size $ step $ step $ step $ step $ step $ step $ initial
