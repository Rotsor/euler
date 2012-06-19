import Data.List
answer = length . group . sort $ [a^b|a<-[2..100],b<-[2..100]]