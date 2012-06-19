import Data.List
import Data.Ord

intSqrt x = take 1 $ filter (\r -> r * r == x) [floor rd, ceiling rd] where
 rd = sqrt $ fromIntegral x

answers = [((k1, k2), p)|k1<-[1..340],k2<-[k1..500],let h2=k1*k1+k2*k2,h<-intSqrt h2,let p = k1+k2+h, p<=1000]

answer = maximumBy (comparing snd) $ map (\l@(x:_) -> (x, length l)). group . sort $ map snd answers