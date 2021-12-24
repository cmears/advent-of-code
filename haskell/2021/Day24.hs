import Control.Monad

-- Each block of the input program computes this function.
f (a, b, c) z input =
    let nonmatch = if (z `mod` 26)+b == input then 0 else 1
    in ((z `div` a) * (25*nonmatch+1)) + ((nonmatch*(input+c)))

-- Each block has three parameters that vary from block to block.
parameters = 
  [
    (1,   12,   6 ),
    (1,   10,   6 ),
    (1,   13,   3 ),
    (26,  -11,  11),
    (1,   13,   9 ),
    (26,  -1,   3 ),
    (1,   10,   13),
    (1,   11,   6 ),
    (26,  0,    14),
    (1,   10,   10),
    (26,  -5,   12),
    (26,  -16,  10),
    (26,  -7,   11),
    (26,  -11,  15)
  ]

-- At each step, we can stop if z has become too big to make it back
-- to zero by the end.
bounds = tail $ reverse $ scanl (\acc x -> if x == 26 then acc+1 else acc) 0 $ reverse $ map (\(a,b,c) -> a) parameters
                  
search order = head (loop (zip bounds parameters) 0 [])
  where loop [] z inputs = do
          guard (z == 0)
          pure (concatMap show (reverse inputs))
        loop ((bound, (a,b,c)):rest) z inputs = do
          i <- order
          let z' = f (a,b,c) z i
          guard (z' <= 26^bound)
          loop rest z' (i:inputs)

main = putStrLn (search [9,8..1]) >> putStrLn (search [1..9])
