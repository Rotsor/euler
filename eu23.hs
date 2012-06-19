
import Data.Array
import Control.Applicative

d x = sum $ filter (\y -> x `mod` y == 0) [1..x-1]

isAbundant' x = d x > x

abundancyArr = listArray (1,28123) [isAbundant' x|x<-[1..28123]]


isAbundant = (abundancyArr !)

abundants = filter isAbundant [1..28123]

sums = filter (<=28123) ((+) <$> abundants <*> abundants)

sumsArr = accumArray (\_ x -> True) False (1, 28123) (map (\s -> (s,())) sums)

result = sum $ filter (not.(sumsArr !)) [1..28123]

main = print result