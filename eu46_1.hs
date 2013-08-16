import Primes

isTwiceSquare n = floor m == ceiling m
    where m = sqrt $ fromIntegral n / 2

isGoldbach n = or $ map (\m -> isTwiceSquare (n - m)) $ takeWhile (<n) primes

isOddComposite n = mod n 2 == 1 && not (isPrime n)

main = print $ head $ filter (\n -> isOddComposite n && not (isGoldbach n)) [2..]
