import Data.List
fact x = product [1..x]
digits = unfoldr (\x -> if x == 0 then Nothing else Just $ (x `mod` 10, x `div` 10))
isCur x = x == sum (map fact $ digits x)
curious = filter isCur [10..4000000]
answer = sum curious
