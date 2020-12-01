import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Debug.Trace

--input = [1,2,3,4,5,6,7,8]

n = genericLength input

--input = s2s "80871224585914546619083218645595"
input = concat . replicate 10000 $ s2s "59768092839927758565191298625215106371890118051426250855924764194411528004718709886402903435569627982485301921649240820059827161024631612290005106304724846680415690183371469037418126383450370741078684974598662642956794012825271487329243583117537873565332166744128845006806878717955946534158837370451935919790469815143341599820016469368684893122766857261426799636559525003877090579845725676481276977781270627558901433501565337409716858949203430181103278194428546385063911239478804717744977998841434061688000383456176494210691861957243370245170223862304663932874454624234226361642678259020094801774825694423060700312504286475305674864442250709029812379"

s2s :: String -> [Integer]
s2s = map (read . (:[]))

type S = M.Map (Integer,Integer) Integer

-- Digit at jth position after i FFTs
f :: Integer -> Integer -> Integer
f 0 j = input `genericIndex` (j-1)
f i j = lastDigit (sum (zipWith (\x p -> if p == 0 then 0 else x*p) [f (i-1) k | k <- [1..n]] (pattern j)))

f2 :: Integer -> Integer -> State S Integer
f2 i j | trace (show (i,j)) False = undefined
f2 0 j = pure (input `genericIndex` (j-1))
f2 i j = do
  ns <- forM (zip [1..n] (pattern j)) $ \(k,p) -> do
          if p == 0 then pure 0 else (p*) <$> g (i-1) k
  pure (lastDigit (sum ns))

g :: Integer -> Integer -> State S Integer
g i j = do
  m <- get
  case M.lookup (i,j) m of
    Nothing -> do
          x <- f2 i j
          modify (M.insert (i,j) x)
          pure x
    Just x -> pure x

lastDigit :: Integer -> Integer
lastDigit x = abs x `mod` 10

pattern :: Integer -> [Integer]
pattern j = tail $ cycle (concatMap (replicate (fromIntegral j)) [0, 1, 0, -1])

main = do
  let offset = 5976809
  print $ flip evalState M.empty $ mapM (f2 100) (take 8 [offset..])
