main = do
  ls <- lines <$> readFile "input.txt"
  let (e,n,h) = foldl f (0,0,90) ls
  print (abs n + abs e)

  let (e2,n2,we,wn) = foldl g (0,0,10,1) ls
  print (abs n2 + abs e2)

x +! y = (x + y) `mod` 360

f (e,n,h) i =
  case i of
    "L90" -> (e,n,h+!270)
    "L180" -> (e,n,h+!180)
    "L270" -> (e,n,h+!90)
    "R90" -> (e,n,h+!90)
    "R180" -> (e,n,h+!180)
    "R270" -> (e,n,h+!270)
    ('F':x) | h == 0 -> (e,n+read x,h)
            | h == 90 -> (e+read x,n,h)
            | h == 180 -> (e,n-read x,h)
            | h == 270 -> (e-read x,n,h)
    ('N':x) -> (e,n+read x,h)
    ('S':x) -> (e,n-read x,h)
    ('E':x) -> (e+read x,n,h)
    ('W':x) -> (e-read x,n,h)

g (e,n,we,wn) i =
  case i of
    "L90" -> (e,n,-wn,we)
    "L180" -> (e,n,-we,-wn)
    "L270" -> (e,n,wn,-we)
    "R90" -> (e,n,wn,-we)
    "R180" -> (e,n,-we,-wn)
    "R270" -> (e,n,-wn,we)
    ('F':x) -> (e+we*read x,n+wn*read x,we,wn)
    ('N':x) -> (e,n,we,wn+read x)
    ('S':x) -> (e,n,we,wn-read x)
    ('E':x) -> (e,n,we+read x,wn)
    ('W':x) -> (e,n,we-read x,wn)
  
