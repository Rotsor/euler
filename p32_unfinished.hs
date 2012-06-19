import Data.List
isPandi a b = sort (show a ++ show b ++ show (a * b)) == ['1'..'9']
pandis = filter (\(a,b) -> isPandi a b) [(a,b)|a<-[1..9999],b<-[1..9999]]
main = print pandis