import Data.List
import Data.List.Split
import qualified Data.Set as S

data Item = Item { itemIngredients :: [String], itemAllergens :: [String] }
  deriving (Show)

readLine :: String -> Item
readLine s =
  let [ingredients, allergens] = splitOn ["(contains"] (words s)
  in Item ingredients (map (delete ')' . delete ',') allergens)

main = do
  items <- map readLine . lines <$> readFile "input.txt"
  let allergens = S.fromList (concatMap itemAllergens items)
      possibles = do
         a <- S.toList allergens
         let relevantItems = filter (\i -> a `elem` itemAllergens i) items
         let possible = foldl1 S.intersection (map (S.fromList . itemIngredients) relevantItems)
         pure (a,possible)
  mapM_ print possibles
  let allPossible = S.unions (map snd possibles)
      safeIngredients = map (\i -> filter (\ing -> not (S.member ing allPossible)) (itemIngredients i)) items
  print allPossible
  -- print safeIngredients
  print (length (concat safeIngredients))
  
-- eggs jxx
-- fish zzt
-- dairy kqv
-- nuts dklgl
-- peanuts pmvfzk
-- sesame tsnkknk
-- shellfish qdlpbt
-- wheat tlgrhdh

-- kqv,jxx,zzt,dklgl,pmvfzk,tsnkknk,qdlpbt,tlgrhdh
