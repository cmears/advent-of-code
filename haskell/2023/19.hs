import Data.List.Split (splitOn)
import Data.Maybe
import Text.Trifecta
import qualified Data.Map as M
import qualified Data.Set as S

type Workflow = [Rule]
data Rule = Rule Condition Consequence deriving Show
data Condition = Always | Comparison Op Char Integer deriving Show
data Op = Less | Greater deriving Show
data Consequence = Accept | Reject | Redirect String deriving Show

type Part = M.Map Char Integer

parseWorkflow :: Parser Workflow
parseWorkflow = braces (sepBy1 parseRule comma)

parseRule :: Parser Rule
parseRule = Rule <$> option Always (try (parseCondition <* char ':')) <*> parseConsequence

parseCategory :: Parser Char
parseCategory = satisfy (`elem` "xmas")

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
parsePart = foldl f M.empty <$> braces (sepBy parseProperty comma)
  where f m (c,n) = M.insert c n m

parseProperty :: Parser (Char, Integer)
parseProperty = (,) <$> parseCategory <*> (char '=' *> decimal)

parse p s = case parseString p mempty s of
              Success a -> a
              Failure e -> error (show e)

parseInput :: String -> (M.Map String Workflow, [Part])
parseInput s = 
    let [workflows, parts] = splitOn [""] (lines s)
    in (M.fromList (map (parse parseNamedWorkflow) workflows), map (parse parsePart) parts)

data Evaluation = Accepted | Rejected deriving (Show, Eq)

evaluate :: M.Map String Workflow -> Part -> Evaluation
evaluate workflows part = evaluateWorkflow (workflows M.! "in") part
  where evaluateWorkflow rules part =
            case head $ mapMaybe (\r -> evaluateRule r part) rules of
              Accept -> Accepted
              Reject -> Rejected
              Redirect s -> evaluateWorkflow (workflows M.! s) part
        evaluateRule (Rule Always consequence) part = Just consequence
        evaluateRule (Rule (Comparison op cat n) cons) part | (f op) (part M.! cat) n = Just cons
        evaluateRule _ _ = Nothing
        f Less = (<)
        f Greater = (>)

simulate :: M.Map String Workflow -> Integer
simulate workflows =
  let rules = (workflows M.! "in")
  in simulate' (M.fromList [(c,S.fromList [1..4000]) | c <- "xmas"]) rules
  where
    simulate' :: M.Map Char (S.Set Integer) -> [Rule] -> Integer
    simulate' v _ | any S.null (M.elems v) = 0
    simulate' v ((Rule condition consequence):rs) =
        let (v1,v2) = applyCondition condition v
        in case consequence of
             Accept -> sz v1 + simulate' v2 rs
             Reject -> simulate' v2 rs
             Redirect s -> simulate' v1 (workflows M.! s) + simulate' v2 rs

applyCondition :: Condition -> M.Map Char (S.Set Integer) -> (M.Map Char (S.Set Integer), M.Map Char (S.Set Integer))
applyCondition Always v = (v, M.fromList [(c,S.empty) | c <- "xmas"])
applyCondition (Comparison op c n) v =
    let (smaller, equal, bigger) = S.splitMember n (v M.! c)
        (s1,s2') = case op of
                     Less -> (smaller, bigger)
                     Greater -> (bigger, smaller)
        s2 = if equal then S.insert n s2' else s2'
    in (M.insert c s1 v, M.insert c s2 v)

sz = product . map (fromIntegral . S.size) . M.elems

main = do
  (workflows, parts) <- parseInput <$> readFile "19.txt"
  let accepted = filter ((==Accepted) . evaluate workflows) parts
  print $ sum $ map (sum . M.elems) accepted
  print $ simulate workflows
