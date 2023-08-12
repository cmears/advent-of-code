import Control.Monad.State
import qualified Data.Set as S
import Data.Set (Set)
import Data.List.Split

import Debug.Trace

-- (X,Y)
-- Y=0 is the surface
type Coord = (Int,Int)

spring :: Coord
spring = (500, 0)

parseLine :: String -> [Coord]
parseLine s = let [x,y] = splitOn ", " s
              in case head s of
                   'x' -> (,) <$> parsePart x <*> parsePart y
                   'y' -> (,) <$> parsePart y <*> parsePart x

parsePart :: String -> [Int]
parsePart s = case splitOn ".." (drop 2 s) of
                [m] -> [read m]
                [m,n] -> [ (read m) .. (read n) ]

main = do
  coords <- concatMap parseLine . lines <$> readFile "17.txt"
  let clay = S.fromList coords
  let s = fill clay
  let (S w r) = s
  print $ S.size w
  print $ S.size r

data S = S { wet :: Set Coord, rest :: Set Coord }
  deriving (Show)

data Result = IntoTheVoid | Pooled
data Probe = Overflow | Wall Int

fill :: Set Coord -> S
fill clay = execState (loop spring) (S S.empty S.empty)
  where
    minY = minimum (snd <$> S.toList clay)
    maxY = maximum (snd <$> S.toList clay)
    loop coord = do
      (S w r) <- get
      if coord `S.member` clay || coord `S.member` r then pure Pooled
      else if snd coord > maxY then pure IntoTheVoid
      else do
         markWet coord
         dres <- probeDown coord
         case dres of
           IntoTheVoid -> pure IntoTheVoid
           Pooled -> do
                     lres <- probeLeft coord
                     rres <- probeRight coord
                     case (lres, rres) of
                       (Wall a, Wall b) -> do
                                         let coords = [ (x,snd coord) | x <- [a+1..b-1] ]
                                         mapM_ markRest coords
                                         pure Pooled
                       _ -> pure IntoTheVoid
    probeLR :: Int -> Coord -> State S Probe
    probeLR offset (x,y) = do
        let next = (x+offset, y)
        (S w r) <- get
        if next `S.member` clay then pure (Wall (x+offset))
        else if next `S.member` r then error "?!?!?!?!?"
             else do
               markWet next
               dres <- probeDown next
               case dres of
                 IntoTheVoid -> pure Overflow
                 Pooled -> probeLR offset next
    probeLeft = probeLR (-1)
    probeRight = probeLR 1

    probeDown (x,y) = loop (x,y+1)

    markWet :: Coord -> State S ()
    markWet coord | snd coord < minY = pure ()
                  | otherwise = modify (\(S w r) -> (S (S.insert coord w) r))

    markRest :: Coord -> State S ()
    markRest coord = modify (\(S w r) -> (S w (S.insert coord r)))
