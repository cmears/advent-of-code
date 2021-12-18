import Data.Either
import Data.Foldable
import Text.Parsec

data T = N Int | P T T
  deriving (Show, Eq)

type Parser = Parsec String ()

item :: Parser T
item = pair <|> number
pair :: Parser T
pair = P <$> (char '[' *> item <* char ',') <*> (item <* char ']')
number :: Parser T
number = N . read <$> many digit

parseSnail :: String -> T
parseSnail s = fromRight (error "?") (parse item "" s)

showT (N x) = show x
showT (P x y) = "[" ++ showT x ++ "," ++ showT y ++ "]"

explode' :: Int -> T -> Maybe (T,(Int,Int))
explode' 4 (P (N x) (N y)) = Just (N 0, (x,y))
explode' d (P x y) =
    case (explode' (d+1) x, explode' (d+1) y) of
      (Just (x', (xr, yr)), _) -> let (y',yr') = distributeR y yr in Just (P x' y', (xr, yr'))
      (Nothing, Just (y', (xr, yr))) -> let (x',xr') = distributeL x xr in Just (P x' y', (xr', yr))
      (Nothing, Nothing) -> Nothing
explode' _ (N x) = Nothing
      
distributeL :: T -> Int -> (T, Int)
distributeL (N x) r = (N (x+r), 0)
distributeL (P x y) r = let (y',r') = distributeL y r in (P x y', r')

distributeR :: T -> Int -> (T, Int)
distributeR (N x) r = (N (x+r), 0)
distributeR (P x y) r = let (x',r') = distributeR x r in (P x' y, r')

explode :: T -> Maybe T
explode t = fst <$> explode' 0 t

split :: T -> Maybe T
split (N x) | x >= 10 = let y = x `div` 2 in Just (P (N y) (N (x-y)))
            | otherwise = Nothing
split (P x y) = case (split x, split y) of
                  (Just x', _) -> Just (P x' y)
                  (Nothing, Just y') -> Just (P x y')
                  _ -> Nothing

reduce t = maybe t reduce (msum [explode t, split t])

mag (N x) = x
mag (P x y) = 3 * mag x + 2 * mag y

main = do
  snails <- map (reduce . parseSnail) . lines <$> readFile "input18"
  print . mag . foldl1 (\a t -> reduce (P a t)) $ snails
  print . maximum $ [ mag (reduce (P x y)) | x <- snails, y <- snails, x /= y ]
