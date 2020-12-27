{-# LANGUAGE ViewPatterns #-}
import Control.Monad
import Data.List
import qualified Data.Map as M
import Text.Regex.TDFA
import Debug.Trace

-- Parsing
----------

-- e.g.
--   submatches "([0-9]+): ([0-9]+)" "123: 456"
-- returns ["123", "456"]
submatches :: String -> String -> Maybe [String]
submatches regex s =
  case getAllTextSubmatches (s =~ regex) of
    [] -> Nothing
    matches -> Just (tail matches)


data Command = Cut Integer
             | Deal
             | DealInc Integer
  deriving (Show)

parseCommand :: String -> Command
parseCommand (submatches "cut ([0-9-]+)" -> Just [n]) = Cut (read n)
parseCommand (submatches "deal with increment ([0-9]+)" -> Just [n]) = DealInc (read n)
parseCommand (submatches "deal into new stack" -> Just []) = Deal
parseCommand x = error $ "couldn't parse: " ++ show x

-- Explicit list representation
--   (good enough for part 1)
------------------------------- 

type Deck = [Int]

execute :: Command -> Deck -> Deck
execute Deal xs = reverse xs
execute (Cut n) xs | n < 0 = execute (Cut (genericLength xs + n)) xs
                   | otherwise = let (cut,rest) = genericSplitAt n xs
                                 in rest ++ cut
execute (DealInc n) xs = loop M.empty 0 xs
  where loop m i [] = M.elems m
        loop m i (x:xs) = loop (M.insert i x m) ((i + n) `mod` l) xs
        l = genericLength xs

runCommands :: [Command] -> Deck -> [Deck]
runCommands commands deck = scanl (flip execute) deck commands

-- Modular arithmetic approach
------------------------------

-- Modular multiplicative inverse
--  i.e. a `invMod` m = x such that (a*x) mod m = 1
--  (a and m must be co-prime)
--  (0 <= a < m)
invMod a m =
    -- (s,t) are the Bézout coefficients
    --   i.e. s*m + t*a = gcd(a,m) = 1
    --              t*a ≡ 1 mod m
    let (s,t) = loop (m, a, 1, 0, 0, 1)
    in t
  where
    loop (ra,0,sa,sb,ta,tb) = (sa,ta)
    loop (ra,rb,sa,sb,ta,tb) =
        let q = ra `div` rb
        in loop (rb,ra-q*rb,
                 sb,sa-q*sb,
                 tb,ta-q*tb)

testInvMod = and $ do
  (a,m) <- testCases
  let x = invMod a m
  pure ((a*x) `mod` m == 1)
  where
    testCases = do
      m <- [2..20]
      a <- [0..m-1]
      guard (gcd a m == 1)
      pure (a,m)

-- Execute a command "forwards"
--   i.e. track at what position a particular card ends up.
-- n is the size of the deck
-- p is the current position of the card
-- returns the new position of that card
forwards :: Integer -> Integer -> Command -> Integer
forwards n p Deal = n-p-1
forwards n p (Cut c) = (p-c) `mod` n
forwards n p (DealInc i) = (p*i) `mod` n

runForwards :: Integer -> Integer -> [Command] -> [Integer]
runForwards n p commands = scanl (forwards n) p commands

-- Execute a command "backwards"
--   i.e. if after executing this command a card ended up in position p,
--        which position was it in before the command?
backwards n p Deal = n-p-1
backwards n p (Cut c) = (p+c) `mod` n
backwards n p (DealInc i) = (p*invMod i n) `mod` n

runBackwards :: Integer -> Integer -> [Command] -> [Integer]
runBackwards n p commands = scanr (\c p -> backwards n p c) p commands

-- Each command either adds or multiplies (modulo some n).  Therefore,
-- running the whole sequence of commands is equivalent to one
-- addition & multiplication.
--
-- Deal      === \x -> x * (-1) - 1
-- Cut c     === \x -> x - c
-- DealInc c === \x -> x * c

-- An operation (a,b) represents the function \x -> x*a + b (mod n)
type Operation = (Integer, Integer)

commandOp Deal = (-1,-1)
commandOp (Cut c) = (1,-c)
commandOp (DealInc c) = (c,0)

combine :: Integer -> Operation -> Operation -> Operation
-- \x -> (x*a + b) * c + d
--     = (x*a*c) + (b*c + d)
combine n (a,b) (c,d) = ((a*c) `mod` n, (b*c+d) `mod` n)

commandsToOp n = foldl1 (combine n) . map commandOp

-- -- Applying a command once:
-- (x*a)+b
-- -- Applying a command twice:
-- (x*a + b) * a + b
-- (x*a*a) + b*a + b
-- (x*a^2) + b*(a+1)

-- Raise an operation to a given power.
--   (where op²(x) = op(op(x)))
opExp :: Integer -> Operation -> Integer -> Operation
--opExp n (a,b) i | trace (show (n, (a,b), i)) False = undefined
opExp n (a,b) 1 = (a,b)
opExp n (a,b) i =
    let opHalf = opExp n (a,b) (i `div` 2)
        opEven = combine n opHalf opHalf
        result = if even i
                 then opEven
                 else combine n opEven (a,b)
    in result

testOpExp = and $ do
  (n,a,b,i) <- testCases
  let x = opExp n (a,b) i
      y = foldl1 (combine n) (genericReplicate i (a,b))
  if x /= y then error (show (n,a,b,i)) else pure ()
  pure (x == y)
  where
    testCases = do
      a <- [0..6]
      b <- [0..6]
      i <- [1..6]
      pure (7,a,b,i)

main = do
  commands <- map parseCommand . lines <$> readFile "input.txt"

  -- Part 1, explicit representation
  let positions1a = map (2019 `elemIndex`) $ runCommands commands [0..10007-1]
  print $ last positions1a

  -- Part 1, modular arithmetic
  let positions1b = runForwards 10007 2019 commands
  print $ last positions1b

  -- Convert the sequence of instructions into a single "operation".
  let operationPart1 = commandsToOp 10007 commands

  -- Part 1, direct operation
  let (a,b) = operationPart1
  print $ (2019*a + b) `mod` 10007

  -- Part 2
  -- Raise the operation to the power of 101741582076661
  let deckSize = 119315717514047
      exponent = 101741582076661
      operationPart2 = commandsToOp deckSize commands
      raisedOperation = opExp deckSize operationPart2 exponent
  print raisedOperation
  -- Apply the operation "backwards"
  let (a,b) = raisedOperation
  let x = (2020 - b) `mod` deckSize
      aInv = invMod a deckSize
      y = (x * aInv) `mod` deckSize
  print y
