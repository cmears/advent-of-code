import qualified Data.Map as M
import Data.List

type Monkey = ([Integer], Integer->Integer, Integer, Int, Int)
type M = M.Map Int Monkey
type S = (M, M.Map Int Integer)

real :: M
real    = M.fromList $ zip [0..]
            [ ([74,64,74,63,53], (*7), 5, 1, 6)
            , ([69,99,95,62], (^2), 17, 2, 5)
            , ([59,81], (+8), 7, 4, 3)
            , ([50, 67, 63, 57, 63, 83, 97], (+4), 13, 0, 7)
            , ([61, 94, 85, 52, 81, 90, 94, 70], (+3), 19, 7, 3)
            , ([69], (+5), 3, 4, 2)
            , ([54, 55, 58], (+7), 11, 1, 5)
            , ([79, 51, 83, 88, 93, 76], (*3), 2, 0, 6)
            ]

ex :: M
ex = M.fromList $ zip [0..]
     [ ([79,98], (*19), 23, 2, 3)
     , ([54,65,75,74], (+6), 19, 2, 0)
     , ([79,60,97], (^2), 13, 1, 3)
     , ([74], (+3), 17, 0, 1)
     ]


step :: S -> Int -> S
step (monkeys,counts) i =
  let (items, f, d, yes, no) = monkeys M.! i
  in case items of
       [] -> (monkeys, counts)
       (item:rest) ->
           let item' = f item `mod` (5*17*7*13*19*3*11*2)
--           let item' = f item `mod` (23*19*13*17)
               recip = if item' `mod` d == 0
                       then yes
                       else no
               m' = M.update (\(ri,rf,rd,ry,rn) -> Just (ri++[item'],rf,rd,ry,rn)) recip .
                    M.insert i (rest,f,d,yes,no) $
                    monkeys
               c' = M.update (Just . (+1)) i counts
           in step (m',c') i

play (monkeys, counts) = foldl step (monkeys, counts) [0..((M.size monkeys) - 1)]

showItems monkeys = do
  mapM_ (\(j,(i,f,d,y,n)) -> putStrLn $ show j ++ ": " ++ show i) (M.toList monkeys)

main = do
  let monkeys = real
      states = iterate play $ (monkeys, M.fromList (zip [0..(M.size monkeys)-1] (repeat 0)))
      items = map (\(m,c) -> map (\(i,_,_,_,_) -> i) (M.elems m)) states
--  showItems (fst (states !! 15))
--  print (snd (states !! 20))
  let counts = M.elems $ snd (states !! 10000)
  let (a:b:_) = reverse $ sort $ counts
  print counts
  print $ a*b
