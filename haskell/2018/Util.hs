{-# LANGUAGE ViewPatterns #-}
module Util where

import Data.List
import Text.Regex.TDFA

{-
Example of use:

parseLine :: String -> Integer
parseLine (submatches "n = (.*)" -> Just [x]) = read x
parseLine (submatches "(.*) \\+ (.*)" -> Just [x,y]) = read x + read y
-}
submatches :: String -> String -> Maybe [String]
submatches regex s =
  case getAllTextSubmatches (s =~ regex) of
    [] -> Nothing
    matches -> Just (tail matches)

count' :: (Eq a) => [a] -> [(a, Int)]
count' xs = map (\g -> (head g, length g)) $ group xs

count :: (Eq a, Ord a) => [a] -> [(a, Int)]
count = count' . sort

primes :: [Integer]
primes = f (2:[3,5..])
  where f (x:xs) = x : f (filter (\y -> y `mod` x /= 0) xs)
        f _ = error "unreachable"