main = do
  let input@((tx1,tx2),(ty1,ty2)) = ((240,292),(-90,-57))
  
      -- What x positions does a given starting vx visit?
      xs vx = scanl1 (+) [vx,vx-1..1]
      -- What y positions does a given starting vy visit?
      ys vy = takeWhile (>= ty1) (scanl1 (+) [vy,vy-1..])

      goodXs = filter (\vx -> (any (`within` (tx1,tx2)) (xs vx))) [1 .. tx2]
      goodYs = filter (\vy -> (any (`within` (ty1,ty2)) (ys vy))) [ty1 .. -ty1]

      trajectories = filter (hits input . path) ((,) <$> goodXs <*> goodYs)

      y = maximum (map snd trajectories)

  print (y*(y+1)`div`2)
  print (length trajectories)

x `within` (a,b) = a <= x && x <= b

sign x = if x > 0 then 1 else if x < 0 then -1 else 0

path trajectory = loop (0,0) trajectory
  where loop (x,y) (vx,vy) = (x,y) : loop (x + vx, y + vy) (vx - sign vx, vy - 1)

hits ((tx1,tx2),(ty1,ty2)) = any (\(x,y) -> x `within` (tx1,tx2) && y `within` (ty1,ty2)) . takeWhile (\(x,y) -> y >= ty1)
