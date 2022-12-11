import Data.Foldable
import qualified Data.Sequence as S

data M = M { i :: S.Seq Int, f :: Int -> Int, d :: Int, y :: Int, n :: Int }

input = [ M (s[74,64,74,63,53]) (*7) 5 1 6,M (s[69,99,95,62]) (^2) 17 2 5
        , M (s[59,81]) (+8) 7 4 3,M (s[50,67,63,57,63,83,97]) (+4) 13 0 7
        , M (s[61,94,85,52,81,90,94,70]) (+3) 19 7 3,M (s[69]) (+5) 3 4 2
        , M (s[54,55,58]) (+7) 11 1 5,M (s[79,51,83,88,93,76]) (*3) 2 0 6
        ] where s = S.fromList

step relief (monkeys,counts) idx =
  let m = monkeys `S.index` idx
  in case S.viewl (i m) of
       S.EmptyL -> (monkeys, counts)
       item S.:< rest ->
           let item' = relief (f m item)
               recipient = ([n,y] !! (fromEnum (item' `mod` d m == 0))) m
               m' = S.adjust' (\m -> m { i = i m S.|> item' }) recipient .
                    S.update idx (m { i = rest}) $ monkeys
           in step relief (m',S.adjust' (+1) idx counts) idx

main = do 
  let { monkeys = S.fromList input ; nm = S.length monkeys }
  let go (relief, n) =
        let states = iterate (\mc -> foldl (step relief) mc [0..nm-1]) (monkeys, S.replicate nm 0)
        in product . take 2 . reverse . toList . S.sort . snd $ states !! n
  print (go ((`div` 3), 20), go ((`mod` (product (map d (toList monkeys)))), 10000))

