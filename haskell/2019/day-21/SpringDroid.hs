import Data.Char
import IntCode

ord' = fromIntegral . ord
chr' = chr . fromIntegral

part1 = do
  program <- readFile "program-part1.txt"
  (result,outputs) <- runFile "input.txt" (map ord' program)
  print result
  putStrLn (map chr' (filter (<=127) outputs))
  print (filter (>127) outputs)

part2 = do
  program <- readFile "program-part2.txt"
  (result,outputs) <- runFile "input.txt" (map ord' program)
  print result
  putStrLn (map chr' (filter (<=127) outputs))
  print (filter (>127) outputs)

main = part1 >> part2

-- Droid logic:

-- 4=dot -> don't jump
-- 1=dot -> must jump

-- 123456789
-- #??#?????
-- #####.#..
-- #####.#.#..##.###
-- #####.#.#..##.###
-- #####..##.##.####
--
-- Heuristic: if 3=dot and 4=safe, jump

-- If you have 1=dot and 4=dot, you're dead
-- So if 5=dot and 8=dot, don't jump
-- So if 2=dot and 5=dot, you must jump
