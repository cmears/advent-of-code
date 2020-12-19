import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import qualified Text.Megaparsec.Char.Lexer as L


-- Parsing
----------
type Parser = Parsec Void String

space :: Parser ()
space = L.space space1 empty empty
symbol = L.symbol space
lexeme = L.lexeme space
number = L.signed space (lexeme L.decimal)

data Rule = Sequence [[Integer]] | Single Char
  deriving (Show)

ruleLine = (,) <$> (number <* symbol ":") <*> rule
rule = singleChar <|> Sequence <$> many number `sepBy` symbol "|"
singleChar = Single <$> between (symbol "\"") (symbol "\"") anySingle

parseOrFail p s = let Right a = parse p "" s in a

-- Message validity
-------------------
valid ruleMap = loop [ruleMap M.! 0]
  where
    -- "loop rules message" returns True iff the message matches the
    -- list of rules.  The rules must be matched in order; i.e. the
    -- first rule matches the first part of the message.

    -- All rules satisfied and the message completely consumed.
    loop [] [] = True

    -- The next rule to satisfy is a "Single"; this succeeds only if
    -- the first character of the message matches, and consumes that
    -- character.
    loop (Single c:rs) (x:xs) | c == x = loop rs xs

    -- The next rule to satisfy is a choice between several sequences
    -- of rule numbers.
    loop (Sequence seqs:rs) xs =
        -- The "or" means "match any of the choices".
        or $ do
          -- Choose one sequence out of many.  A failure might
          -- backtrack to here and try the next sequence.
          seq <- seqs
          -- Convert the rule numbers into rules (by retrieving them
          -- from the rule map).
          let rs' = map (ruleMap M.!) seq
          -- Loop again without consuming any of the message, putting
          -- the new sequence of rules at the top of the list.
          pure (loop (rs'++rs) xs)

    -- If nothing else matched, we've failed.
    loop _ _ = False


main = do
  ls <- lines <$> readFile "input.txt"
  let [ruleText,messages] = splitOn [""] ls
  let ruleMap = M.fromList (map (parseOrFail ruleLine) ruleText)
  print . length . filter (valid ruleMap) $ messages

  let newRules = M.fromList (map (parseOrFail ruleLine) ["8: 42 | 42 8", "11: 42 31 | 42 11 31"])
  let ruleMap2 = M.union newRules ruleMap
  print . length . filter (valid ruleMap2) $ messages
  
