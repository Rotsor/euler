only [x] = x
main = print $ only $ [a*b*c|a<-[1..1000],b<-[a..1000-a],let c = 1000 - a - b, c^2==a^2+b^2]
