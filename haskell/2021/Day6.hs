import Data.List
import qualified Data.Map as M

main = do
  xs <- read . (\s -> "["++s++"]") <$> readFile "input6" :: IO [Int]
  let ys = iterate step . M.fromList . map ((,)<$>head<*>length) . group . sort $ xs
  mapM_ (print . sum . M.elems . (ys!!)) [80,256]

step m = M.fromList [(k,f k)|k<-[0..8]]
  where l = flip (M.findWithDefault 0) m
        f 6 = l 7 + l 0
        f n = l ((n+1)`mod`9)
