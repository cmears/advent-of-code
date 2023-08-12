{-# LANGUAGE RankNTypes #-}
import Control.Monad.State
import qualified Data.Set as S
import Data.Set (Set)
import Data.List.Split

import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed

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
  let (w,r) = fill clay
  let nwet = length $ filter id $ elems w
      nrest = length $ filter id $ elems r
  print nwet
  print nrest
  print $ S.size clay
  putStrLn ""
  print $ nwet - S.size clay
  print $ nrest - S.size clay
  pure ()

data Result = IntoTheVoid | Pooled
data Probe = Overflow | Wall Int

type A s = STUArray s Coord Bool

fill :: Set Coord -> (UArray Coord Bool, UArray Coord Bool)
fill clay = runST $ do
              let minCoord = (minX-1, minY)
                  maxCoord = (maxX+1, maxY)
              wa <- trace (show (minCoord, maxCoord)) $
                  newArray (minCoord, maxCoord) False
              ra <- newArray (minCoord, maxCoord) False
              forM_ (S.toList clay) $ \c -> do
                writeArray wa c True
                writeArray ra c True
              loop spring wa ra
              wa' <- freeze wa
              ra' <- freeze ra
              pure (wa', ra')
  where
    minY = minimum (snd <$> S.toList clay)
    maxY = maximum (snd <$> S.toList clay)
    minX = minimum (fst <$> S.toList clay)
    maxX = maximum (fst <$> S.toList clay)
    loop :: forall s. Coord -> A s -> A s -> ST s Result
    loop coord wa ra | snd coord > maxY = pure IntoTheVoid
    loop coord wa ra | snd coord < minY = probeDown coord wa ra
    loop coord wa ra = do
      isRest <- readArray ra coord
      if isRest then pure Pooled
      else do
         markWet wa coord
         dres <- probeDown coord wa ra
         case dres of
           IntoTheVoid -> pure IntoTheVoid
           Pooled -> do
                     lres <- probeLeft coord wa ra
                     rres <- probeRight coord wa ra
                     case (lres, rres) of
                       (Wall a, Wall b) -> do
                                         let coords = [ (x,snd coord) | x <- [a+1..b-1] ]
                                         mapM_ (markRest ra) coords
                                         pure Pooled
                       _ -> pure IntoTheVoid
    probeLR :: Int -> Coord -> A s -> A s -> ST s Probe
    probeLR offset (x,y) wa ra = do
        let next = (x+offset, y)
        isRest <- readArray ra next
        if isRest then pure (Wall (x+offset))
        else do
          markWet wa next
          dres <- probeDown next wa ra
          case dres of
            IntoTheVoid -> pure Overflow
            Pooled -> probeLR offset next wa ra
    probeLeft = probeLR (-1)
    probeRight = probeLR 1

    probeDown :: Coord -> A s -> A s -> ST s Result
    probeDown (x,y) = loop (x,y+1)

    markWet :: A s -> Coord -> ST s ()
    markWet wa coord | snd coord < minY = pure ()
                     | otherwise = writeArray wa coord True

    markRest :: A s -> Coord -> ST s ()
    markRest ra coord = writeArray ra coord True
