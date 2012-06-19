import Data.Char
import Data.List
import Network.HTTP
import Data.List.Split

isTriangle :: Integer -> Bool
isTriangle n = (>0) . length . filter (==n) $ takeWhile (<=n) [i*(i+1)`div`2|i<-[1..]] 

wordValue = sum . map (fromIntegral.(+1).(subtract (ord 'A')).ord)

isTriangleWord = isTriangle . wordValue

parseWords = map (init . tail) . splitOn ","

solve = genericLength . filter isTriangleWord . parseWords

main = simpleHTTP (getRequest "http://projecteuler.net/project/words.txt") >>= getResponseBody >>= print . solve