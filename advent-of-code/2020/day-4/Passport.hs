import Control.Monad
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main = do
  c <- readFile "input.txt"
  case parse parser "" c of
    Left e -> putStrLn (errorBundlePretty e)
    Right passports -> do
         print (length (filter valid passports))
         print (length (filter valid2 passports))

type Parser = Parsec Void String
type Passport = [(String, String)]

-- Many passports, separated by double-newlines.
parser :: Parser [Passport]
parser = passport `sepBy` (try (newline >> newline))

-- A passport is many "items", separated by single spaces.
passport :: Parser Passport
passport = item `sepEndBy` singleSpace

-- A single space is a newline or a space, but *not* a double-newline.
singleSpace :: Parser Char
singleSpace = do
  notFollowedBy (newline >> newline)
  newline <|> char ' '

-- An item is a three letter key and a string separated by a colon.
item :: Parser (String, String)
item = do
  key <- replicateM 3 alphaNumChar
  char ':'
  value <- many (alphaNumChar <|> char '#')
  guard (not (null value))
  pure (key, value)


-- The fields a passport must have.
requiredFields = S.fromList ["ecl", "pid", "eyr", "hcl", "byr", "iyr", "hgt"]

-- A passport is valid if it has all the required fields, after you take the (optional) "cid" away.
valid passport = S.delete "cid" (S.fromList (map fst passport)) == requiredFields

-- A passport is valid if it has all the fields and they're all valid.
valid2 passport = valid passport && all validField passport

-- Run a parser against a string and tell us if it succeeded.
checkParse :: Parser a -> String -> Bool
checkParse p s = case parse (p >> eof) "" s of
                   Left _ -> False
                   Right _ -> True

-- Is x ∈ [l,u] ?
within l u x = l <= x && x <= u

-- Parse a 4-digit year within the given bounds.
year :: Integer -> Integer -> Parser Integer
year l u = do
  y <- read <$> replicateM 4 digitChar
  guard (within l u y)
  pure y

-- Parse strings like "100cm" or "80in", respecting the height limits.
height = try heightCM <|> heightIN
  where heightCM = (decimal <* string "cm") >>= (guard . within 150 193)
        heightIN = (decimal <* string "in") >>= (guard . within 59 76)

-- Parse colour strings like "#12ab4d".
hairColor = char '#' >> replicateM 6 hexDigitChar

-- Parse eye colours.
eyeColor = foldl1 (<|>) (map string (words "amb blu brn gry grn hzl oth"))

-- Parse passport IDs (9 digits).
passportID = replicateM 9 digitChar

-- A field is valid if the parser succeeds.
validField :: (String, String) -> Bool
validField ("byr", s) = checkParse (year 1920 2002) s
validField ("iyr", s) = checkParse (year 2010 2020) s
validField ("eyr", s) = checkParse (year 2020 2030) s
validField ("hgt", s) = checkParse height s
validField ("hcl", s) = checkParse hairColor s
validField ("ecl", s) = checkParse eyeColor s
validField ("pid", s) = checkParse passportID s
validField ("cid", _) = True
validField _ = False                   
