import Control.Applicative

import IntCode

permutations :: [a] -> [[a]]
permutations [] = pure []
permutations xs = do
  (y,ys) <- select xs
  ys' <- permutations ys
  pure (y:ys')

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = pure (x,xs) <|> do
                  (y,ys) <- select xs
                  pure (y, x:ys)

phaseSequences = permutations [0..4]

main = do
  program <- readFile "input"
  let results = do
         ps <- phaseSequences
         let inputs = 0:outputs
             outputs = [ head (runProgram program [p,i]) | (p,i) <- zip ps inputs ]
         pure (last outputs)
  print (maximum results)
