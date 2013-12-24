{-# LANGUAGE NoMonomorphismRestriction #-}
import Primes
import Control.Arrow
import Data.List

factor :: (Integral a) => [a] -> a -> [a]
factor _ 1 = []
factor (p : ps) x 
       | p * p > x = [x]
       | x `mod` p == 0 = p : factor (p : ps) (x `div` p)
       | otherwise = factor ps x

factor' :: Integer -> [(Integer, Int)]
factor' = map (head &&& length) . group . factor primes

divisors n = product $ map (\(_, c) -> c+1) (factor' n)

main = print $ head $ dropWhile ((<=500) . divisors) (scanl (+) 1 [2..])
