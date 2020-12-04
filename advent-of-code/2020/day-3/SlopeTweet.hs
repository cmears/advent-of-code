main=do
ls<-lines<$>readFile "input.txt"
let m = [((i,j),t)|(i,l)<-zip [0..] ls,(j,t)<-zip [0..] l]
let g x y = length(filter(==Just '#')(map(flip lookup m)(takeWhile (\(i,_)->i<length ls)[(y*i,x*i`mod`length(head ls))|i<-[0..]])))
print(g 3 1,g 1 1*g 3 1*g 5 1*g 7 1*g 1 2)
