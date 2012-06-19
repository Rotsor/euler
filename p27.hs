import Data.Array
primes = 2 : filter (\cand -> all (\p -> cand `mod` p /= 0) (takeWhile (\p -> p * p <= cand) primes)) [3..]
primeArr = accumArray (\_ _ -> True) False (0,20000) $ map (\x -> (x,())) $ takeWhile (<=20000) primes
isPrime n = n >= 0 && (primeArr ! n)
q a b = length $ takeWhile isPrime [n^2+a*n+b|n<-[0..]]
answer = maximum [(q a b, a * b)|a<-[-1000..1000],b<-[-1000..1000]]
main = print answer