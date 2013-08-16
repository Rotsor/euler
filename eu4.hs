isPali n = show n == reverse (show n)
main = print $ maximum $ filter isPali $ [a*b|a<-[100..999],b<-[100..999]]
