import Primes
main = putStrLn . reverse . take 10 . reverse . show $ ifold (+) 0 $ map (\i -> i^i) [1..1000]