import Data.List
import Data.Ord
import Data.Array

collatzArr = listArray (1, 1000000) [collatz' x | x <- [1..1000000]]
collatz'' n | n > 1000000 = collatz' n
 | otherwise = collatzArr ! n
collatz' 1 = 1
collatz' n
   | even n = 1 + (collatz'' $ n `div` 2)
   | otherwise = 1 + (collatz'' $ 3 * n + 1)

main = print $ maximumBy (comparing collatz') [1..1000000-1]