import Network.HTTP
import Data.List.Split


sample :: [[Integer]]
sample = map (map read.words) [
 "131 673 234 103 18"
 ,"201 96 342 965 150"
 ,"630 803 746 422 111"
 ,"537 699 497 121 956"
 ,"805 732 524 37 331"]

minsum l = head $ foldr (\xs ss -> scanr (\(s,x) a -> min a s + x) 99999999999 $ zip ss xs) (replicate (length (head l) - 1) 999999999999999 ++ [0]) l

url = "http://projecteuler.net/project/matrix.txt"

myRead :: (Show a, Read a) => String -> a
myRead x = case reads x of
  [(x, [])] -> x
  [] -> error $ "Can't parse: " ++ x
  other -> error $ "wtf: " ++ show other ++ " while parsing " ++ x

main = simpleHTTP (getRequest url) >>= getResponseBody >>= print . minsum . map (map myRead.wordsBy(`elem` ",")). wordsBy (`elem` "\r\n")