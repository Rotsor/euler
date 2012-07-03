import Search
dgts = length . show . (+0)
main = print $ length $ [(x,n,x^n) |n<-[1..100], x<-[1..9],length(show $ x ^ n) == n]
-- x>=10 are impossible because 10^n always has n+1 digits
-- n>=100 are imppossible because 9^n will always have less digits
