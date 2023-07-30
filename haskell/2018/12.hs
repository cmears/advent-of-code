import Data.List
import qualified Data.Map as M

-- example
-- initial = "#..#.#..##......###...###"

-- rules = M.fromList $
--           [ ("...##", '#')
--           , ("..#..", '#')
--           , (".#...", '#')
--           , (".#.#.", '#')
--           , (".#.##", '#')
--           , (".##..", '#')
--           , (".####", '#')
--           , ("#.#.#", '#')
--           , ("#.###", '#')
--           , ("##.#.", '#')
--           , ("##.##", '#')
--           , ("###..", '#')
--           , ("###.#", '#')
--           , ("####.", '#') ]

initial = "#.##.##.##.##.......###..####..#....#...#.##...##.#.####...#..##..###...##.#..#.##.#.#.#.#..####..#"
rules = M.fromList $
                       [ ("#####", '#')
                       , ("####.", '.')
                       , ("###.#", '#')
                       , ("###..", '#')
                       , ("##.##", '.')
                       , ("##.#.", '.')
                       , ("##..#", '#')
                       , ("##...", '#')
                       , ("#.###", '.')
                       , ("#.##.", '.')
                       , ("#.#.#", '#')
                       , ("#.#..", '#')
                       , ("#..##", '.')
                       , ("#..#.", '#')
                       , ("#...#", '.')
                       , ("#....", '.')
                       , (".####", '.')
                       , (".###.", '#')
                       , (".##.#", '#')
                       , (".##..", '#')
                       , (".#.##", '#')
                       , (".#.#.", '.')
                       , (".#..#", '.')
                       , (".#...", '#')
                       , ("..###", '.')
                       , ("..##.", '#')
                       , ("..#.#", '#')
                       , ("..#..", '.')
                       , ("...##", '.')
                       , ("...#.", '#')
                       , ("....#", '.')
                       , (".....", '.') ]

step state =
  map (\k -> M.findWithDefault '.' k rules) $ filter ((==5) . length) $ map (take 5) $ tails $ "...." ++ state ++ "...."

step2 (state, offset) =
  let state' = step state
      added = length (takeWhile (=='.') state') - 2
  in (reverse $ dropWhile (=='.') $ reverse $ dropWhile (=='.') state', offset + added)

part1 = do
  let n = 20
      steps = take (n+1) $ iterate step initial
      final = last steps
      pairs = zip [-2*n..] final
      x = sum (map fst (filter ((=='#').snd) pairs))
--  mapM_ print steps
--  print final
  print x

part2 = do
  let n = 500
  let steps = iterate step2 (initial, 0)
--  mapM_ print $ take n steps
  -- after 500 steps
  let (final,offset) = iterate step2 (initial, 0) !! n
  let nplants = length $ filter (=='#') final
  let answer500 = sum $ map fst $ filter ((=='#').snd) $ zip [offset..] final
--  print (final, offset)
--  print answer500
  -- 500 36768
  -- 501 36837
  -- 502 36906
  -- diff 69
  -- Formula derived by observation
  print $ answer500 + 69*(50*10^9 - 500)

main = part1 >> part2
