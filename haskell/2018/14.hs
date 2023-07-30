import qualified Data.Sequence as Seq
import Data.List

type M = Seq.Seq Int
type S = (M, Int, Int)

main = do
  let m = Seq.fromList [3,7]
--      training = 2018
      training = 293801
      enough = training + 10
  let s = (m, 0, 1)
      steps = iterate step s
      Just (finalm,_,_) = find (\(m,_,_) -> Seq.length m >= enough) steps

      targets = take 10 ([training, training+1..])
  putStrLn $ concatMap show $ map (\i -> finalm `Seq.index` i) targets

  let target = "293801"
      targetLen = length target
      Just found = find (\(m,_,_) -> extract targetLen m == target
                                || extract targetLen (chop m) == target) steps
      (foundm,_,_) = found
  if extract targetLen foundm == target
    then print (Seq.length foundm - targetLen)
    else print (Seq.length foundm - targetLen - 1)

chop seq = case Seq.viewr seq of
             seq' Seq.:> _ -> seq'

extract n seq | Seq.length seq < n = "?????"
              | otherwise = concatMap show $ do
                  i <- [0..n-1]
                  pure $ seq `Seq.index` (Seq.length seq - n + i)



step :: S -> S
step (m, i, j) =
    let xi = m `Seq.index` i
        xj = m `Seq.index` j
        total = xi + xj
        digits = if total >= 10 then [1, total `mod` 10] else [total `mod` 10]
        m' = m Seq.>< (Seq.fromList digits)
        n' = Seq.length m'
        i' = (i + xi + 1) `mod` n'
        j' = (j + xj + 1) `mod` n'
    in (m',i',j')
