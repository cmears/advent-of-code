input = [("53","333"),("83","1635"),("72","1289"),("88","1532")]
main = print (f input) >> print (f [((\(a,b) -> (concat a, concat b)) (unzip input))])
  where f input = product (g <$> input)
        g (t,r) = let [x,y] = q (read t,read r) in floor x - ceiling y + 1
        q (t,r) = [ (t `op` sqrt (t^2 - 4*r)) / 2 | op <- [(+),(-)] ]
