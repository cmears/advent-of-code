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

phaseSequences = permutations [5..9]

-- NEED TO CHANGE INTERPRETER TO PRODUCT INTERMEDIATE OUTPUTS
-- Instead of returning Bool, move the outputs out of the
-- state and return a list of outputs.
-- (Termination is indicated by the end of the output list.)

runPhaseSequence program ps =
    let outputsA = runProgram program ((ps !! 0):0:outputsE)
        outputsB = runProgram program ((ps !! 1):outputsA)
        outputsC = runProgram program ((ps !! 2):outputsB)
        outputsD = runProgram program ((ps !! 3):outputsC)
        outputsE = runProgram program ((ps !! 4):outputsD)
     in print outputsA

-- main = do
--   program <- readFile "input"
--   let results = do
--          ps <- phaseSequences
--          pure (runPhaseSequence program ps)
--   print (maximum results)
