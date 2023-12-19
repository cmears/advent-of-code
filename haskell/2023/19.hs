import Data.List
import Data.List.Split (splitOn)
import Data.Maybe
import Text.Trifecta
import qualified Data.Map as M

data Workflow = Workflow [Rule] deriving Show
data Rule = Rule Condition Consequence deriving Show
data Condition = Always | Comparison Op String Integer deriving Show
data Op = Less | Greater deriving Show
data Consequence = Accept | Reject | Redirect String deriving Show

data Part = Part (M.Map String Integer) deriving Show

parseWorkflow :: Parser Workflow
parseWorkflow = Workflow <$> braces (sepBy1 parseRule comma)

parseRule :: Parser Rule
parseRule = Rule <$> option Always (try (parseCondition <* char ':')) <*> parseConsequence

parseCategory :: Parser String
parseCategory = do
  c <- satisfy (`elem` "xmas")
  pure [c]

parseCondition :: Parser Condition
parseCondition = do
  cat <- parseCategory
  op <- satisfy (`elem` "<>")
  n <- decimal
  pure $ Comparison (if op == '<' then Less else Greater) cat n

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
  pure $ Part $ foldl f M.empty pairs
  where f m (c,n) = M.insert c n m

parseProperty :: Parser (String, Integer)
parseProperty = (,) <$> parseCategory <*> (char '=' *> decimal)

ex = "xy{a<2006:qkq,m>2090:A,rfg}"
ex2 = "{x=2036,m=264,a=79,s=2244}"

parse p s = case parseString p mempty s of
              Success a -> a
              Failure e -> error (show e)

parseInput :: String -> (M.Map String Workflow, [Part])
parseInput s = 
    let ls = lines s
        [workflows, parts] = splitOn [""] ls
    in (M.fromList (map (fromSuccess . parseString parseNamedWorkflow mempty) workflows),
        map (fromSuccess . parseString parsePart mempty) parts)

fromSuccess (Success a) = a

data Evaluation = Accepted | Rejected deriving (Show, Eq)

evaluate :: M.Map String Workflow -> Part -> Evaluation
evaluate workflows part = evaluateWorkflow (workflows M.! "in") part
  where evaluateWorkflow (Workflow rules) part =
            case head $ mapMaybe (\r -> evaluateRule r part) rules of
              Accept -> Accepted
              Reject -> Rejected
              Redirect s -> evaluateWorkflow (workflows M.! s) part
        evaluateRule (Rule Always consequence) part = Just consequence
        evaluateRule (Rule (Comparison op cat n) cons) (Part ratings) | (f op) (ratings M.! cat) n = Just cons
        evaluateRule _ _ = Nothing
        f Less = (<)
        f Greater = (>)

--simulate :: Map.Map String Workflow -> 

main = do
  (workflows, parts) <- parseInput <$> readFile "19.txt"
  let accepted = filter ((==Accepted) . evaluate workflows) parts
  print $ sum $ map (\(Part ratings) -> sum (M.elems ratings)) accepted
