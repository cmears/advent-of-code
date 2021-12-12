import Control.Monad
import Control.Monad.State
import Data.Char
import qualified Data.Map as M
import Data.Map (Map, (!))
import qualified Data.Set as S
import Data.Set (Set)

main = do
  pairs <- map (\l -> let (a,'-':b) = break (=='-') l in (a,b)) . lines <$> readFile "input12"
  -- "edges" is a Map from each cave to all its neighbour caves.
  let edges = M.fromListWith (++) $ map (\(a,b) -> (a,[b])) $ pairs ++ map (\(a,b) -> (b,a)) pairs

  -- Part 1 and Part 2 are the same, except in Part 1 we don't have a revisit pass.
  forM_ [False, True] $ \pass -> print $ evalState (paths "start" edges pass S.empty) M.empty

-- We record the answers in a store.
-- The key is
--   * the cave we are at,
--   * whether we've used our revisit pass,
--   * what caves we've already visited.
-- The value is how many paths there are to the end from that situation.
type Key = (String, Bool, Set String)
type Store = Map Key Integer

-- Compute the number of paths from the given situation (like the key above)
-- to the end cave, using and updating the store as we go along.
paths :: String -> M.Map String [String] -> Bool -> Set String -> State Store Integer
-- The base case: we're at the end, so there's one path.
paths "end" _ _ _ = pure 1
paths cave edges pass visited = do
  let key = (cave, pass, visited)
  alreadyComputed <- gets (M.member key)
  -- If we haven't recorded the answer, we have to compute it.
  when (not alreadyComputed) $ do
    subanswers <- sequence $ do
                    -- We're going from "cave" to "cave2".
                    cave2 <- edges ! cave
                    -- Don't go back to the start.
                    guard (cave2 /= "start")
                    -- Are we spending our revisit pass right now?
                    let spend = cave2 `S.member` visited
                    -- We can't spend the revisit pass if we don't have one.
                    guard (not spend || pass)
                    -- Add the cave we're leaving to the visited set (if a small cave).
                    let visited' = if isUpper (head cave) then visited else S.insert cave visited
                    -- Do we have a revisit pass any more?
                    let pass' = pass && not spend
                    -- Recurse down the subpath.
                    pure $ paths cave2 edges pass' visited'
    -- Record the answer.
    modify (M.insert key (sum subanswers))
  -- Lookup the answer.
  gets (! key)
