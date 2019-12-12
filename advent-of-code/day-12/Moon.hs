import Data.Function
import Data.List
import Data.Ord

myinput = [ (14, 15, -2), (17, -3, 4), (6, 12, -13), (-2, 10, -8) ]

sample1 = [
 (-1, 0, 2),
 (2, -10, -7),
 (4, -8, 8),
 (3, 5, -1)
 ]

sample2 = [
  (-8, -10, 0),
  (5, 5, 10),
  (2, -7, 3),
  (9, -8, -3)
  ]

energy ((x,y,z),(vx,vy,vz)) = (abs x + abs y + abs z) * (abs vx + abs vy + abs vz)

findRepeat3 :: [Integer] -> Integer
findRepeat3 xs0 = loop 1 (stepDim (xs0, replicate 4 0))
  where loop n (pos,vel) =
            if vel == [0,0,0,0] && pos == xs0
            then n
            else loop (n+1) (stepDim (pos,vel))

stepDim (pos,vel) =
    let n = length pos
        adjustments = do
          i <- [0..n-1]
          j <- [i+1..n-1]
          let (xi) = pos !! i
              (xj) = pos !! j
              (dxi,dxj) = delta xi xj
          [ (i, (dxi)), (j, (dxj)) ]
        grouped = groupBy ((==) `on` fst) . sortBy (comparing fst) $ adjustments
        summed = map (sum . map snd) grouped
        vel' = zipWith (+) vel summed
        pos' = zipWith (+) pos vel'
    in (pos', vel')
           

main = do
  -- Part 1
  let (pos, vel) = iterate step (myinput, replicate 4 (0,0,0)) !! 1000
      totalE = sum . map energy $ zip pos vel
  print totalE

  -- Part2
  let pos = myinput
  let xs = map (\(a,b,c) -> a) pos
      ys = map (\(a,b,c) -> b) pos
      zs = map (\(a,b,c) -> c) pos
      cx = findRepeat3 xs
      cy = findRepeat3 ys
      cz = findRepeat3 zs
      c = cx `lcm` cy `lcm` cz
  print c

step (pos, vel) =
    let n = length pos
        adjustments = do
          i <- [0..n-1]
          j <- [i+1..n-1]
          let (xi,yi,zi) = pos !! i
              (xj,yj,zj) = pos !! j
              (dxi,dxj) = delta xi xj
              (dyi,dyj) = delta yi yj
              (dzi,dzj) = delta zi zj
          [ (i, (dxi, dyi, dzi)), (j, (dxj, dyj, dzj)) ]
        grouped = groupBy ((==) `on` fst) . sortBy (comparing fst) $ adjustments
        summed = map (sumVectors . map snd) grouped
        vel' = zipWith plus vel summed
        pos' = zipWith plus pos vel'
    in (pos', vel')

delta xi xj | xi > xj = (-1,1)
            | xi == xj = (0,0)
            | otherwise = (1,-1)

sumVectors = foldl plus (0,0,0)

plus (a,b,c) (d,e,f) = (a+d, b+e, c+f)
