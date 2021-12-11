import Data.List
import Data.Maybe
import qualified Data.Map as M

-- Convenient type aliases.
type Coord = (Int,Int) 
neighbours :: Coord -> [Coord]
neighbours (r,c) = [ (r',c') | r' <- [r-1,r,r+1], c' <- [c-1,c,c+1], (r',c') /= (r,c) ]

type Grid = M.Map Coord Int

main = do
  -- Read the input as a list of lists of integers.
  xs <- map (map (\c -> read [c])) . lines <$> readFile "input11"
  -- Build a map of (row,column) -> integer.
  let m = M.fromList $ [ ((r,c),x) | (r,l) <- zip [0..] xs, (c,x) <- zip [0..] l ]

  -- Execute the steps.
  -- Compute a list of how many cells flashed at each step.
  let loop m = let (m',f) = step m in f : loop m'
      fs = loop m
  -- How many in the first 100 steps?
  print . sum . take 100 $ fs
  -- When do we first get 100 flashers in one step?
  -- (The +1 is to convert from zero-based index to step number.)
  print . (+1) . fromJust . elemIndex 100 $ fs

-- Execute a "flash": take a Grid, and a list of coords that have flashed, and increment all their neighbours.
flash :: Grid -> [Coord] -> Grid
flash = foldl' (\acc rc -> foldr (M.update (Just . (+1))) acc (neighbours rc))

-- Reset all the flashed cells.
reset :: Grid -> Grid
reset = M.map (\x -> if x > 9 then 0 else x)

-- Execute a single step:
--   increment all the cells
--   flash any >9 cells
--   flash any cells that have gone over 9 due to previous flashes, until stable
-- Return the new grid, and the number of cells that flashed.
step :: Grid -> (Grid, Int)
step m =
    let m' = M.map (+1) m
        -- Flash until stable.
        -- We have a list of cells that we know will flash in this step.
        -- We flash them and increase their neighbours,
        --   then see what cells are above 9.
        -- If there are *new* cells now above 9, we keep going.
        loop flashers =
            -- Flash the known flashers.
            let m'' = flash m' flashers
            -- Compute the set of cells known to flash this step.
                flashers' = M.keys $ M.filter (>9) m''
            -- If there are no more, step; else keep going.
            in if length flashers == length flashers' then flashers else loop flashers'
        flashers = loop []
    -- Now we've computed the full set of flashers, execute their flash, reset the flashers, and stop.
    in (reset (flash m' flashers), length flashers)

