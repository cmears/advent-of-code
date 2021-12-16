import Data.Char
import Data.List
import Numeric
import Control.Monad.State
import Text.Printf

-- Convert a single hex digit into a list of bits.
hex2bits :: Char -> [Bit]
hex2bits c | isHexDigit c = map digitToInt . printf "%04b" . (fst :: (Int,a) -> Int) . head . readHex . (:[]) $ c
           | otherwise = []

main = do
  hex <- readFile "input16"
  case runStateT packet (concatMap hex2bits hex) of
    Left e -> print e
    Right (p,bs) -> do
      print (versionSum p)
      print (evaluate p)

-- Represent bits with Ints.
type Bit = Int

-- Our parser consumes a list of Bits.
type Parser = StateT [Bit] (Either String)

-- Definitions of packets and their contents.
data Packet = Packet Integer Body

data Op = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo

data Body = Literal Integer | Operator Op [Packet]

-- Compute a packet's "version sum" -- the sum of all the versions
-- of the packet and all packets inside.
versionSum :: Packet -> Integer
versionSum (Packet v body) =
    v + case body of
          Literal _ -> 0
          Operator _ packets -> sum (map versionSum packets)

-- Evaluate the operation a packet represents.
evaluate :: Packet -> Integer
evaluate (Packet _ (Literal x)) = x
evaluate (Packet _ (Operator op packets)) = evalOp op (map evaluate packets)

-- Evaluate a single operation.
evalOp :: Op -> [Integer] -> Integer
evalOp Sum = sum
evalOp Product = product
evalOp Minimum = minimum
evalOp Maximum = maximum
evalOp GreaterThan = \[a,b] -> b2i (a > b)
evalOp LessThan = \[a,b] -> b2i (a < b)
evalOp EqualTo = \[a,b] -> b2i (a == b)

b2i :: Bool -> Integer
b2i b = if b then 1 else 0

-- Parse a stream of bits into a packet.
parse :: [Bit] -> Either String Packet
parse bits = evalStateT packet bits

-- Parse a packet.
packet :: Parser Packet
packet = do
  v <- version
  t <- typeId
  if t == 4 then Packet v . Literal <$> literal else Packet v . Operator (opType t) <$> operator

-- Convert an integer to its op-type.
opType i = [Sum, Product, Minimum, Maximum, undefined, GreaterThan, LessThan, EqualTo] `genericIndex` i

-- Parse a literal packet body.
literal :: Parser Integer
literal = do
  quartets <- loop
  pure (foldl' (\acc x -> acc*16+x) 0 quartets)
  where loop = do
            ~(b:bs) <- bits 5
            let x = integer bs
            if b == 0 then pure [x] else (x:) <$> loop

-- Parse an operation packet body.
operator :: Parser [Packet]
operator = do
  lengthTypeId <- bit
  if lengthTypeId == 0
  then do
    nbits <- integer <$> bits 15
    bits <- bits nbits
    let Right (packets, []) = runStateT (many packet) bits
    pure packets
  else do
    npackets <- fromInteger . integer <$> bits 11
    replicateM npackets packet

-- Generic parser combinator; execute the parser repeatedly until it fails.
-- The final failing parse doesn't consume any input.
many :: Parser a -> Parser [a]
many p = do
  bits <- get
  case runStateT p bits of
    Left e -> pure []
    Right (a, bs) -> put bs >> (a:) <$> many p

-- Parse the version number.
version :: Parser Integer
version = integer <$> bits 3

-- Parse the type id.
typeId :: Parser Integer
typeId = integer <$> bits 3

-- Read a list of bits as an integer, most significant bit first.
integer :: [Bit] -> Integer
integer = fst . head . readBin . concatMap show

-- Parse a fixed number of bits.
bits :: Integer -> Parser [Bit]
bits n = replicateM (fromInteger n) bit

-- Parse a single bit.
bit :: Parser Bit
bit = do
  bs0 <- get
  case bs0 of
    [] -> lift $ Left "unexpected end of input"
    (b:bs) -> put bs >> pure b
