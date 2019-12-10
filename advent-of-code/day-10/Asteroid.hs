import Data.List
import Data.Ord
import qualified Data.Set as S
import Data.Set (Set)

type Coord = (Int,Int)

readField :: FilePath -> IO (Set Coord, (Int,Int))
readField path = do
  c <- readFile path
  let rows = lines c
  let size = (length (head rows), length rows)
  let tagged = do
        (y, row) <- zip [0..] rows
        (x, value) <- zip [0..] row
        pure ((x,y), value)
  let asteroids = S.fromList [ coord | (coord, value) <- tagged, value == '#' ]
  pure (asteroids, size)

main = do
  (asteroids, (cols,rows)) <- readField "input"
  let inBounds (x,y) = 0 <= x && x < cols && 0 <= y && y < rows
  let blocked (vx,vy) (ax,ay) =
          let dx0 = ax-vx
              dy0 = ay-vy
              g = gcd dx0 dy0
              dx = dx0 `div` g
              dy = dy0 `div` g
          in takeWhile inBounds [ (ax + i*dx, ay + i*dy) | i <- [1..] ]
  let visibleFrom viewer =
          let others = S.delete viewer asteroids
              allBlocked = S.fromList (concat [ blocked viewer asteroid | asteroid <- S.toList others ])
          in others S.\\ allBlocked
  let score viewer = S.size (visibleFrom viewer)
  let scoredAsteroids = [ (asteroid, score asteroid) | asteroid <- S.toList asteroids ]
  let bestAsteroid = maximumBy (comparing snd) scoredAsteroids
  putStrLn $ "Best viewing asteroid: " ++ show bestAsteroid
  let laser = fst (bestAsteroid)
  let (lx,ly) = laser

  let loop remaining | S.null remaining = []
      loop remaining =
          let allBlocked = S.fromList (concat [ blocked laser asteroid | asteroid <- S.toList remaining ])
              visible = remaining S.\\ allBlocked
              ang (ax,ay) = angle (ax-lx, ay-ly)
              sorted = sortBy (comparing ang) (S.toList visible)
          in sorted ++ loop (remaining S.\\ visible)

  let firingSequence = loop (S.delete laser asteroids)
  putStrLn $ "200th asteroid vaporised: " ++ show (firingSequence !! 199)

angle (x,y) = clamp (90 - (180/pi) * atan2 (- fromIntegral y) (fromIntegral x))
  where clamp a | a < 0 = a + 360
                | a >= 360 = a - 360
                | otherwise = a
