import Control.Monad
import Data.Array.IArray
import Data.Foldable
import qualified Data.MultiSet as S
import Data.List
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), (|>), ViewL((:<)))
import Data.Time
import Test.QuickCheck
import Text.Printf

main' = do
  xs <- map read . lines <$> readFile "input.txt"
  let invalid = findInvalid xs
  measure "findInvalid" $ print invalid
  measure "findInvalid2" $ print (findInvalid2 xs)
  measure "findSeq"  $ let Just seq = findSeq  invalid xs in print (minimum seq + maximum seq)
  measure "findSeq2" $ let Just seq = findSeq2 invalid xs in print (minimum seq + maximum seq)
  measure "findSeq3" $ let Just seq = findSeq3 invalid xs in print (minimum seq + maximum seq)
  measure "findSeq4" $ let Just seq = findSeq4 invalid xs in print (minimum seq + maximum seq)
  print (findSeq4 invalid xs)


-- Find the invalid number.
findInvalid :: [Integer] -> Integer
findInvalid = last . fromJust . find bad . map (take 26) . tails

-- Is this sequence of 26 numbers (a length 25 preamble followed by
-- a candidate number) bad?
-- Not very clever: checks every pair from the preamble.
bad :: [Integer] -> Bool
bad xs =
    let (preamble,[x]) = splitAt 25 xs
    -- Try to find a "good" pair; if none exists, we're bad.
    in not . or $ do
      -- Choose 2 numbers from the preamble.
      (a:bs) <- tails preamble
      b <- bs
      -- Check that they worked.
      pure (a+b == x)

-- Faster version, probably too complicated for its own good.
-- Maintains a set of the preamble as we go along, then has a faster
-- lookup to find a matching pair.
findInvalid2 :: [Integer] -> Integer
findInvalid2 xs =
  let (preamble, xs') = splitAt 25 xs
      loop s sq [] = error "oops"
      loop s sq (y:ys) =
          case find (\x -> (S.member (y-x) s)) (S.toAscList s) of
            Nothing -> y
            Just _ ->
                let (d :< sq') = Seq.viewl sq
                in loop (S.insert y (S.delete d s)) (sq' |> y) ys
  in loop (S.fromList preamble) (Seq.fromList preamble) xs'


headMay (x:_) = Just x
headMay [] = Nothing

-- findSeq t xs
-- Find a subsequence of xs that sums to t.
-- Pretty slow; O(N³)
findSeq :: Integer -> [Integer] -> Maybe [Integer]
findSeq t xs =
    headMay $ do ys <- tails xs
                 zs <- inits ys
                 guard (sum zs == t)
                 guard (length zs >= 2)
                 pure zs

-- Slight improvement: do a running total as you check a prefix.
-- Won't work if there are negative numbers.
-- O(N²).
findSeq2 :: Integer -> [Integer] -> Maybe [Integer]
findSeq2 t xs =
    headMay $ do ys <- init $ tails xs
                 case tail $ takeWhile (<= t) $ scanl (+) 0 ys of
                   [] -> []
                   zs | last zs == t && length zs >= 2 -> pure (take (length zs) ys)
                   _ -> []

-- Fastest: O(N).
-- Won't work if there are negative numbers.
findSeq3 :: Integer -> [Integer] -> Maybe [Integer]
findSeq3 t xs = loop 0 0 0
  where
    n = length xs
    a = listArray (0,n-1) xs :: Array Int Integer
    loop i j s | s == t && i <= j-2 = Just (take (j-i) (drop i xs))
               | s  > t = loop (i+1) j (s-a!i)
               | j >= n = Nothing
               | s <= t = loop i (j+1) (s+a!j)

-- Fastest: O(N).
-- Won't work if there are negative numbers.
-- Uses a queue instead.
findSeq4 :: Integer -> [Integer] -> Maybe [Integer]
findSeq4 t = loop 0 Seq.empty
  where
    -- Case 1: found the answer
    loop s q _ | s == t && Seq.length q >= 2 = Just (toList q)
    -- Case 2: too big, need to remove first element from queue
    loop s (y :<| q') xs | s >= t = loop (s-y) q' xs
    -- Case 3: too small, need to add an element to the queue
    loop s q (x:xs) | s < t = loop (s+x) (q |> x) xs
    -- Case 4: ran out of options
    loop s q [] = Nothing

-- Coarse timing.
measure :: String -> IO a -> IO a
measure s a = do
  (t1,r,t2) <- (,,) <$> getCurrentTime <*> a <*> getCurrentTime
  printf "%s: %s\n" s (show (diffUTCTime t2 t1))
  pure r


-- Do the various versions of findSeq find the same answer?
agreement = \(Positive x) (NonEmpty xs0) ->
            let xs = map getPositive xs0
                a = findSeq x xs
                b = findSeq2 x xs
                c = findSeq3 x xs
                d = findSeq4 x xs
            in collect (isJust a) $ a == b && a == c && a == d

runTest = quickCheckWithResult (stdArgs { maxSuccess = 5000 }) agreement
main = runTest
