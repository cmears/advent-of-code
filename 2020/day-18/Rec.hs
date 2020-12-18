import Data.Char

data S = N Integer | S Char
  deriving (Show)
lexer :: String -> [S]
lexer [] = []
lexer (c:cs) | isDigit c = let (a,b) = span isDigit (c:cs) in N (read a) : lexer b
             | isSpace c = lexer (dropWhile isSpace cs)
             | isPunctuation c = S c : lexer cs
             | isSymbol c = S c : lexer cs

data E = C Integer | Op E O E
  deriving (Show)
data O = Add | Mult
  deriving (Show)

parseExpr :: [S] -> (E, [S])
parseExpr ss =
    let (t, ss2) = parseTerm ss
    in case parseExprTail ss2 of
         Nothing -> (t, ss2)
         Just (e,ss3) -> (Op t Mult e, ss3)

parseExprTail (S '*' : ss) = let (t,ss2) = parseExpr ss in Just (t,ss2)
parseExprTail _ = Nothing

parseTerm ss =
    let (u, ss2) = parseUnit ss
    in case parseTermTail ss2 of
         Nothing -> (u, ss2)
         Just (e,ss3) -> (Op u Add e, ss3)

parseTermTail (S '+' : ss) = let (t,ss2) = parseTerm ss in Just (t,ss2)
parseTermTail _ = Nothing

parseUnit (S '(':ss) = let (e, S ')':ss2) = parseExpr ss in (e,ss2)
parseUnit (N x:ss) = (C x, ss)

eval (C x) = x
eval (Op e1 Add e2) = eval e1 + eval e2
eval (Op e1 Mult e2) = eval e1 * eval e2
