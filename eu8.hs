import Data.List
import Data.Char
main = readFile "eu8.inp" >>= print . maximum . map product . filter ((==5) . length) . map (take 5) . tails . map digitToInt . concat . words
