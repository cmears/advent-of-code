import qualified Data.Map as M
import Control.Monad.State

-- Simple implementation of a "ring" (a double-ended queue)
-- The ring is never empty.  The head of the second list is the "current" element.
-- For example, the state 1 2 3 (4) 5 6 is represented as
-- ([3,2,1], [4,5,6])
-- Imagine you are the current marble, the "4"; what you can see to the right
-- is the tail of the second list; what you can see to the left is the first list.
type Ring = ([Int], [Int])

initial :: Ring
initial = ([], [0])

-- Move the "current" cursor to the right.
-- Special case to guarantee the second list is never empty.
moveRight (left, [x]) = ([], reverse (x:left))
moveRight (left, current:right) = (current:left, right)
moveRight _ = undefined

-- Move the "current" cursor to the left.
moveLeft (x:xs, right) = (xs, x:right)
moveLeft ([], right) = moveLeft (reverse right, [])

-- Insert a new element "before" the current one; i.e. between the
-- current cursor and the element to its left.
add next (left, right) = (left, next:right)

-- Execute one step of the game, given the next marble to be inserted.
-- Returns the new state, and the (player,score) pair indicating
-- which player moved and what score they got for that turn.
step :: Int -> Int -> Ring -> (Ring, (Int, Int))
step players next ring | next `mod` 23 == 0 =
    let (left, x:xs) = apply 7 moveLeft ring
    in ((left, xs), (next `mod` players, next + x))
step players next ring =
    ((add next . apply 2 moveRight) ring, (0,0))

-- Apply a function n times.
apply n f = foldl (.) id (replicate n f)

steps :: Int -> Int -> [(Int, Int)]
steps players limit =
    let f :: Int -> State Ring (Int, Int)
        f n = do
          (r', s) <- gets (step players n)
          put r'
          pure s
    in evalState (mapM f [1..limit]) initial

playGame limit players =
     maximum $ M.elems $ M.fromListWith (+) $ steps players limit

main = do
    print $ playGame 71522 446
    print $ playGame 7152200 446