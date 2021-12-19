import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Text.Parsec
import qualified Text.Parsec.Token as T
import Text.Parsec.Language

-- Parsing
type Parser = Parsec String ()
lexer = T.makeTokenParser emptyDef
integer :: Parser Integer
integer = T.integer lexer
header :: Parser ()
header = try $ void $ string "--- scanner " >> (integer <?> "scan number") >> string "---\n"
type Coord = (Integer,Integer,Integer)
coord :: Parser Coord
coord = try $ (,,) <$> (integer <?> "x ord") <*> (char ',' >> integer) <*> (char ',' >> integer)
scanner :: Parser [Coord]
scanner = header >> many1 coord
report :: Parser [[Coord]]
report = many scanner


main = do
  input <- readFile "input19"
  let scans = fromRight (error "???") $ parse report "" input
  let loop exhausted aligned [] = exhausted ++ aligned
      loop exhausted ((a,v):aligned) unaligned =
          let (stillUnaligned, newlyAligned) = loop2 a unaligned
          in loop ((a,v):exhausted) (newlyAligned ++ aligned) stillUnaligned
      loop2 basis unaligned = partitionEithers (map f unaligned)
        where f u = case overlap basis u of
                      Nothing -> Left u
                      Just (o,v) -> Right (map (transform (o,v)) u, v)
  let aligned = loop [] [(head scans, (0,0,0))] (tail scans)
  let beacons = nub $ concatMap fst aligned
  print (length beacons)
  let scanners = map snd aligned
  let distances = [ manhattan (x `minus` y) | x <- scanners, y <- scanners ]
  print (maximum distances)


-- We are looking down z; x is to our right; y is up.
turnRight (x,y,z) = (-z,y,x)
turnLeft = turnRight . turnRight . turnRight
turnUp (x,y,z) = (x,-z,y)
turnDown = turnUp . turnUp . turnUp
spinRight (x,y,z) = (y,-x,z)
spinLeft = spinRight . spinRight . spinRight

orientations = do
  facing <- [id, turnRight, turnRight . turnRight, turnLeft, turnUp, turnDown]
  spin <- [id, spinRight, spinRight . spinRight, spinLeft]
  pure (spin . facing)

-- A rotation followed by a translation.
type Transformation = (Coord -> Coord, Coord)

transform (o,v) = (`add` v) . o

-- Do two scans overlap?
overlap :: [Coord] -> [Coord] -> Maybe Transformation
overlap cs1 cs2 =
    let n1 = length cs1
        n2 = length cs2
        cs1s = S.fromList cs1
        f o (i,j) =
            -- We want cs1[i] to match cs2[j]
            -- Therefore we translate all cs2 by cs1[i] - cs2[j]
            let cs2' = map o cs2
                v = (cs1!!i) `minus` (cs2'!!j)
                cs2'' = map (`add` v) cs2'
            in if S.size (cs1s `S.intersection` S.fromList cs2'') >= 12 then Just v else Nothing
    in listToMaybe $ do
      -- We have two scans, not necessarily oriented.
      -- We assume scan 1 is in the "absolute" coordinate system.
      -- We try all 24 orientations of scan 2 and try to match with scan 1.
      -- If we get any match, return the orientation and translation required to match scan 2 to scan 1.
      -- The vector part of the transformation is also the position of scanner 2 from the perspective
      --   of scanner 1.
      o <- orientations
      let vs = mapMaybe (f o) [ (i,j) | i <- [0..n1-12], j <- [0..n2-1] ]
      guard (not (null vs))
      pure (o,head vs)

(x,y,z) `add` (a,b,c) = (x+a,y+b,z+c)
(x,y,z) `minus` (a,b,c) = (x-a,y-b,z-c)
manhattan (x,y,z) = abs x + abs y + abs z
