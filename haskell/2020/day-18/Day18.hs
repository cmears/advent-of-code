import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

space :: Parser ()
space = L.space space1 empty empty
symbol = L.symbol space
lexeme = L.lexeme space
number = L.signed space (lexeme L.decimal)

data E = N Integer | Op E O E
  deriving (Show)
data O = Add | Mult
  deriving (Show)

eval (N x) = x
eval (Op e1 Add e2) = eval e1 + eval e2
eval (Op e1 Mult e2) = eval e1 * eval e2

parseExpr1 = do
  u <- unit parseExpr1
  ts <- many (exprTail parseExpr1)
  pure $ foldl (\acc (o,u2) -> Op acc o u2) u ts

parseExpr2 = do
  u <- unit parseExpr2
  ts <- many (exprTail parseExpr2)
  let loop h [] = [h]
      loop h ((Mult, u):ts) = h : loop u (ts)
      loop h ((Add, u):ts) = loop (Op h Add u) (ts)
  pure $ foldl1 (\acc t -> Op acc Mult t) (loop u ts)
    
exprTail pe = (,) <$> operator <*> unit pe
unit pe = parens pe <|> (N <$> number)
operator = (symbol "+" >> pure Add) <|> (symbol "*" >> pure Mult)
parens pe = between (symbol "(") (symbol ")") pe

parseLine pe s =
  case parse pe "" s of
    Left e -> error (errorBundlePretty e)
    Right e -> e

main = do
  ls <- lines <$> readFile "input.txt"
  print . sum . map (eval . parseLine parseExpr1) $ ls
  print . sum . map (eval . parseLine parseExpr2) $ ls
