import Data.List
toBase b = unfoldr (\n -> if n==0 then Nothing else Just (n `mod` b, n `div` b))
isPalindrome x = x == reverse x
curious x = isPalindrome (toBase 2 x) && isPalindrome (toBase 10 x)
curiouses = filter curious [1..1000000]
answer = sum curiouses
