import qualified Data.Map as M
import Data.Map ((!))
import Data.List
import Data.Char
import Data.Maybe

neighbours (r,c) = [(r+1,c),(r-1,c),(r,c+1),(r,c-1)]

main = do
  ls <- lines <$> readFile "input9"
  let m = M.fromList $ [ ((r,c),digitToInt d) | (r,l) <- zip [0..] ls, (c,d) <- zip [0..] l ]
  let risks = map (+1) . M.elems . M.filterWithKey (\rc v -> v < minimum (mapMaybe (flip M.lookup m) (neighbours rc))) $ m
  print (sum risks)
  print . product . take 3 . map fst . reverse . sort . map ((,) <$> length <*> head) . group . sort . filter isJust . M.elems . foldl (f m) M.empty . M.toList $ m

f m basinMap (rc,v) | M.member rc basinMap = basinMap
                    | v == 9 = M.insert rc Nothing basinMap
                    | otherwise =
                    case find (maybe False (<v) . flip M.lookup m) (neighbours rc) of
                      Nothing -> M.insert rc (Just rc) basinMap
                      Just rcl -> let bm' = f m basinMap (rcl,m!rcl)
                                  in M.insert rc (bm' ! rcl) bm'
