import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Map as M

shapes = [
    [(0,0),(0,1),(0,2),(0,3)]
  , [(0,1),(1,0),(1,1),(1,2),(2,1)]
  , [(0,0),(0,1),(0,2),(1,2),(2,2)]
  , [(0,0),(1,0),(2,0),(3,0)]
  , [(0,0),(0,1),(1,0),(1,1)]
  ]

gas direction shape fallen =
    let shape' = map (plus (if direction == '<' then (0,-1) else (0,1))) shape
    in if any (\c@(y,x) -> x < 0 || x > 6 || c `S.member` fallen) shape'
       then shape else shape'

fall shape fallen =
     let shape' = map (plus (-1,0)) shape
     in if any (\c@(y,x) -> y <= 0 || c `S.member` fallen) shape'
        then Nothing else Just shape'

plus (a,b) (c,d) = (a+c,b+d)

execute gases (fallen, gasIndex, shapeIndex, sn) =
    let highestRock = if S.null fallen then 0 else fst (S.findMax fallen)
        offset = (highestRock+4,2)
        placedShape = map (plus offset) (shapes !! shapeIndex)
        loop s gi =
            let s' = gas (gases `Seq.index` gi) s fallen
                gi' = (gi+1)`mod`(Seq.length gases)
            in case fall s' fallen of
                 Nothing -> (s', gi')
                 Just s'' -> loop s'' gi'
        (restingPlace, gi) = loop placedShape gasIndex
    in (S.union (S.fromList restingPlace) fallen, gi, (shapeIndex+1)`mod`5, sn+1)

main = do
  input <- head . lines <$> readFile "17.txt"
  let states = iterate (execute (Seq.fromList input)) (S.empty, 0, 0, 0)

  -- Part 1
  print . fst . S.findMax . (\(f,_,_,_) -> f) $ states !! 2022

  -- Part 2
  let loop ((f,gi,si,sn):ss) m =
          let height = fst (S.findMax f)
              key = (gi,si)
              m' = M.insertWith (++) key [(sn,height)] m
          in case m' M.! key of
               ((n4,h4):(n3,h3):(n2,h2):(n1,h1):_) | take 4 [n1,n2..] == [n1,n2,n3,n4] &&
                                                     take 4 [h1,h2..] == [h1,h2,h3,h4] -> (n2-n1,h2-h1)
               _ -> loop ss m'
      (dn,hn) = loop (tail states) M.empty

  let target = 10^12
  let (f,_,_,_) = states !! (target `mod` dn)
  print $ fst (S.findMax f) + (hn * (target `div` dn))
