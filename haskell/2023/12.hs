import Control.Monad
import Data.List
import Data.List.Split

main = do
  ls <- lines <$> readFile "12.txt"
  let pairs = map parseLine ls
  let ws = map (ways3 . expand) pairs
--  let bs = map (\p -> ways2 p == ways3 p) pairs
  mapM_ print (zip pairs ws)
  print $ sum ws
  pure ()

expand (s,xs) = (intercalate "?" (replicate 5 s), concat (replicate 5 xs))

fills :: String -> [String]
fills s = sequence $ map (\c -> if c == '?' then "#." else [c]) s

ways (s,xs) = length [ f | f <- fills s, interpret f == xs ]

parseLine :: String -> (String, [Int])
parseLine l =
  let [x,y] = words l
  in (x, read ("[" ++ y ++ "]"))

interpret :: String -> [Int]
interpret = map length . wordsBy (=='.')

ways2 (s,xs) = ways2' 0 (s++".",xs)
ways2' :: Int -> (String, [Int]) -> Int
ways2' 0 ([], []) = 1
ways2' 0 ('.':cs, xs) = ways2' 0 (cs,xs)
ways2' 0 ('#':cs, (x:xs)) = ways2' x (cs,xs)
ways2' 0 ('?':cs, xs) =
    let m = sum xs + length xs - 1
    in if (length ('?':cs) < m) then 0
       else let nhash = length (filter (=='#') cs)
                nq = 1 + length (filter (=='?') cs)
            in if sum xs < nhash || sum xs > nhash + nq then 0
               else ways2' 0 ('.':cs,xs) + ways2' 0 ('#':cs,xs)
ways2' 0 ('#':cs, []) = 0
ways2' 0 ([], xs) = 0
ways2' 0 p = error (show (0,p))
ways2' 1 ('.':cs, xs) = ways2' 0 (cs,xs)
ways2' 1 ('?':cs, xs) = ways2' 0 (cs,xs)
ways2' 1 ('#':cs, xs) = 0
ways2' 1 _ = error "?"
ways2' n ('#':cs, xs) = ways2' (n-1) (cs,xs)
ways2' n ('?':cs, xs) = ways2' (n-1) (cs,xs)
ways2' n ('.':cs, xs) = 0

nonhash c = c `elem` ".?"
nondot c = c `elem` "#?"

knife xs =
    let n = length xs
        (before,(middle:after)) = splitAt (n`div`2) xs
    in (before,middle,after)

ways3 (s,[]) = if all nonhash s then 1 else 0
ways3 (s,xs) =
    let (before,middle,after) = knife xs
        n = length s
        minbefore = sum before + length before
        minafter = sum after + length after
        slack = n - (minbefore + minafter + middle)
        range = [minbefore .. minbefore+slack]
    in sum $ do
         j <- range
         case divide j middle s of
           Nothing -> pure 0
           Just (pre,intra,post) -> pure $ ways3 (pre,before) * ways3 (post,after)

divide j middle s =
    let (pre, rest) = splitAt j s
        (intra, post) = splitAt middle rest
    in if all nondot intra && (null pre || nonhash (last pre)) && (null post || nonhash (head post))
       then Just (if null pre then pre else init pre, intra, if null post then post else tail post)
       else Nothing
