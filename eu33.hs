import Data.List
import Data.Ratio
import Control.Monad
n a b=a*10+b
answer = product [(num%den)|[a,b,c]<-replicateM 3 [1..9],let [(num,nr),(den,dr)]=sort$[(n a b,a),(n b c,c)],num<den,nr*den==dr*num]
-- answer