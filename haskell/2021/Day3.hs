import Data.Char
m = fromEnum . uncurry (<=) . foldl (\(z,o) -> ([(z+1,o),(z,o+1)] !!)) (0,0)
b = foldl ((+).(2*)) 0
g f _ [x] = b x
g f i xs = let t = f(m(map(!!i)xs)) in g f (i+1) [x|x<-xs,x!!i==t]
main = do
  xs <- ((digitToInt<$>)<$>) . lines <$> readFile "input3"
  let x = (\i -> m (map (!!i) xs)) <$> [0..11] in print (b x*(4095-b x))
  print (g id 0 xs * g (1-) 0 xs)

