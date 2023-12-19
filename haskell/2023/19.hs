import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Trifecta
import qualified Data.Map as Map

data Workflow = Workflow [Rule] deriving Show
data Rule = Rule Condition Consequence deriving Show
data Condition = Always | Less Category Integer | Greater Category Integer deriving Show
data Category = X | M | A | S deriving Show
data Consequence = Accept | Reject | Redirect String deriving Show

data Part = Part Integer Integer Integer Integer deriving Show

getCategory :: Category -> Part -> Integer
getCategory X (Part x m a s) = x
getCategory M (Part x m a s) = m
getCategory A (Part x m a s) = a
getCategory S (Part x m a s) = s

parseWorkflow :: Parser Workflow
parseWorkflow = Workflow <$> braces (sepBy1 parseRule comma)

parseRule :: Parser Rule
parseRule = Rule <$> option Always (try (parseCondition <* char ':')) <*> parseConsequence

parseCategory :: Parser Category
parseCategory = do
  c <- satisfy (`elem` "xmas")
  pure $ [X,M,A,S] !! (fromJust (c `elemIndex` "xmas"))

parseCondition :: Parser Condition
parseCondition = do
  cat <- parseCategory
  op <- satisfy (`elem` "<>")
  n <- decimal
  pure $ (if op == '<' then Less else Greater) cat n

parseConsequence :: Parser Consequence
parseConsequence = do
  s <- many letter
  pure $ case s of
           "A" -> Accept
           "R" -> Reject
           _ -> Redirect s

parseNamedWorkflow :: Parser (String, Workflow)
parseNamedWorkflow = (,) <$> many letter <*> parseWorkflow

parsePart :: Parser Part
parsePart = do
  pairs <- braces (sepBy parseProperty comma)
  pure $ foldl f (Part undefined undefined undefined undefined) pairs
  where f (Part x m a s) (X,n) = Part n m a s
        f (Part x m a s) (M,n) = Part x n a s
        f (Part x m a s) (A,n) = Part x m n s
        f (Part x m a s) (S,n) = Part x m a n

parseProperty :: Parser (Category, Integer)
parseProperty = (,) <$> parseCategory <*> (char '=' *> decimal)

ex = "xy{a<2006:qkq,m>2090:A,rfg}"
ex2 = "{x=2036,m=264,a=79,s=2244}"

parseInput :: String -> (Map.Map String Workflow, [Part])
parseInput s = 
    let ls = lines s
        [workflows, parts] = splitOn [""] ls
    in (Map.fromList (map (fromSuccess . parseString parseNamedWorkflow mempty) workflows),
        map (fromSuccess . parseString parsePart mempty) parts)

fromSuccess (Success a) = a

data Evaluation = Accepted | Rejected deriving (Show, Eq)

evaluate :: Map.Map String Workflow -> Part -> Evaluation
evaluate workflows part = evaluateWorkflow (workflows Map.! "in") part
  where evaluateWorkflow (Workflow rules) part =
            case head $ mapMaybe (\r -> evaluateRule r part) rules of
              Accept -> Accepted
              Reject -> Rejected
              Redirect s -> evaluateWorkflow (workflows Map.! s) part
        evaluateRule (Rule Always consequence) part = Just consequence
        evaluateRule (Rule (Less category n) consequence) part | getCategory category part < n = Just consequence
        evaluateRule (Rule (Greater category n) consequence) part | getCategory category part > n = Just consequence
        evaluateRule _ _ = Nothing

--simulate :: Map.Map String Workflow -> 

main = do
  (workflows, parts) <- parseInput <$> readFile "19.txt"
  let accepted = filter ((==Accepted) . evaluate workflows) parts
  print $ sum $ map (\p -> getCategory X p + getCategory M p + getCategory A p + getCategory S p) accepted
