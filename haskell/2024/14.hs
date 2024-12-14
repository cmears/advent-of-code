import Data.List
import Control.Monad
import qualified Data.Map as M

(gridWidth,gridHeight) = (101,103)

main = do
  s <- readFile "14.txt"
  let robots :: [Robot]
      robots = do
         l <- lines s
         let [a,b] = words l
             ps = "(" ++ drop 2 a ++ ")"
             vs = "(" ++ drop 2 b ++ ")"
         pure (read ps, read vs)

  let robots100 = map (\r -> normalise (gridWidth,gridHeight) (iterate step r !! 100)) robots
      quads = map (mapToQuad (gridWidth,gridHeight)) robots100
      groups = map length $ group (sort (filter (>0) quads))
  print $ product groups

  -- let loop (i,rs) = do
  --        if i `mod` 101 == 4 then draw (i,rs) else pure ()
  --        let rs' = map (normalise (gridWidth,gridHeight) . step) rs
  --        loop (i+1,rs')
  -- loop (0,robots)
  draw (6771, map (\r -> normalise (gridWidth, gridHeight) (iterate step r !! 6771)) robots)

type Robot = ((Int,Int),(Int,Int))

draw :: (Int,[Robot]) -> IO ()
draw (i,robots) = do
  let m = M.fromList $ do
                    ((px,py),_) <- robots
                    pure ((px,py),True)
  print i
  forM_ [0..gridHeight-1] $ \y -> do
    forM_ [0..gridWidth-1] $ \x -> do
      putChar (if M.member (x,y) m then '*' else '.')
    putChar '\n'
             
step :: Robot -> Robot
step ((px,py),(vx,vy)) = ((px+vx,py+vy),(vx,vy))

normalise :: (Int,Int) -> Robot -> Robot
normalise (gw,gh) ((px,py),(vx,vy)) = ((px `mod` gw,py `mod` gh),(vx,vy))

mapToQuad :: (Int,Int) -> Robot -> Int
mapToQuad (gw,gh) ((px,py),(vx,vy)) =
    let (w,h) = (gw `div` 2, gh `div` 2)
    in if px < w && py < h then 1 else if px < w && py > h then 2
       else if px > w && py < h then 3 else if px > w && py > h then 4
       else 0
         
