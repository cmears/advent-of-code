import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace

main = do
  c <- getContents
  let numbers = unfoldr (\s -> case span (/= ',') s of
                                 ("", _) -> Nothing
                                 (x,rest) -> Just (read x, drop 1 rest)) c
  print (numbers :: [Integer])

  let initialState = M.fromList (zip [0..] numbers)

  let finalState = execute initialState 0

  let minKey = fst (fromJust $ M.lookupMin finalState)
      maxKey = fst (fromJust $ M.lookupMax finalState)

  let finalNumbers = map (\i -> finalState M.! i) [minKey..maxKey]

  print finalNumbers

--execute m i | trace (show (i,m)) False = undefined
execute m i =
    let opcode = m M.! i
    in case opcode of
         99 -> m
         _ ->
             let loc1 = m M.! (i+1)
                 loc2 = m M.! (i+2)
                 loc3 = m M.! (i+3)
                 arg1 = m M.! loc1
                 arg2 = m M.! loc2
                 op = case opcode of
                        1 -> (+)
                        2 -> (*)
                 result = op arg1 arg2
                 m' = M.insert loc3 result m
             in execute m' (i+4)
