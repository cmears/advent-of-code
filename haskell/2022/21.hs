{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Map as M
import Data.Maybe
import Util

type Monkey = (String, Job)
data Job = Literal Int
         | Op String Char String
  deriving (Show)

parseLine :: String -> Monkey
parseLine (submatches "(.*): ([0-9]+)" -> Just [m,n]) = (m, Literal (read n))
parseLine (submatches "(.*): ([a-z]*) ([-+*/]) ([a-z]*)" -> Just [m,s1,op,s2]) = (m, Op s1 (head op) s2)

apply op = fromJust (lookup op (zip "+-/*" [(+),(-),div,(*)]))
invert1 op = fromJust (lookup op (zip "-+*/" [(+),(-),div,(*)]))
invert2 op x y | op `elem` "+*" = invert1 op x y
               | otherwise = apply op y x

main = do
  m <- M.fromList . map parseLine . lines <$> readFile "21.txt"
  print . fromJust $ eval m "root"

  let Op l _ r = m M.! "root"
      m' = M.delete "humn" m
  print $ case (eval m' l, eval m' r) of
            (Nothing, Just y) -> force m' l y
            (Just x, Nothing) -> force m' r x

eval :: M.Map String Job -> String -> Maybe Int
eval m monkey = do
   M.lookup monkey m >>= \case
     Literal x -> pure x
     Op s1 op s2 -> apply op <$> eval m s1 <*> eval m s2

force :: M.Map String Job -> String -> Int -> Int
force m "humn" target = target
force m name target =
    case m M.! name of
      Literal _ -> error "x"
      Op s1 op s2 ->
          case (eval m s1, eval m s2) of
            (Nothing, Just y) -> force m s1 (invert1 op target y)
            (Just x, Nothing) -> force m s2 (invert2 op target x)
