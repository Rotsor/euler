
getDigit sz i j = show (10^sz+i) !! j

find i sz | i > (1+sz)*9*10^sz = find (i - (1+sz)*9*10^sz) (sz + 1)
 | otherwise = getDigit sz (i `div` (1 + sz)) (i `mod` (1 + sz))

d i = read . (:[]) $ find i 0

answer = d 0 * d 9 * d 99 * d 999 * d 9999 * d 99999 * d 999999
