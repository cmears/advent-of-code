import Data.List
go x p f = print . f . foldl (\a l -> let { [c,n] = words l; Just i = elemIndex c ["forward","down","up"] } in p (read n) a !! i) x . lines =<< readFile "input2"
main = go (0,0) (\n (h,d) -> [(h+n,d),(h,d+n),(h,d-n)]) (uncurry (*)) >> go (0,0,0) (\n (h,d,a) -> [(h+n,d+a*n,a),(h,d,a+n),(h,d,a-n)]) (\(h,d,_) -> h*d)
