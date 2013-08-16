import Network.HTTP
import Data.Char
url = "http://projecteuler.net/project/roman.txt"

parse ('I':'X':t) = 9 + parse t
parse ('I':'V':t) = 4 + parse t
parse ('X':'L':t) = 40 + parse t
parse ('X':'C':t) = 90 + parse t
parse ('C':'D':t) = 400 + parse t
parse ('C':'M':t) = 900 + parse t
parse ('I':t) = 1 + parse t
parse ('V':t) = 5 + parse t
parse ('X':t) = 10 + parse t
parse ('L':t) = 50 + parse t
parse ('C':t) = 100 + parse t
parse ('D':t) = 500 + parse t
parse ('M':t) = 1000 + parse t
parse [] = 0

showRoman x
  | x >= 1000 = 'M' : showRoman (x-1000)
  | x >= 900 = "CM" ++ showRoman (x-900)
  | x >= 500 = "D" ++ showRoman (x-500)
  | x >= 400 = "CD" ++ showRoman (x-400)

  | x >= 100 = 'C' : showRoman (x-100)
  | x >= 90 = "XC" ++ showRoman (x-90)
  | x >= 50 = "L" ++ showRoman (x-50)
  | x >= 40 = "XL" ++ showRoman (x-40)

  | x >= 10 = 'X' : showRoman (x-10)
  | x >= 9 = "IX" ++ showRoman (x-9)
  | x >= 5 = "V" ++ showRoman (x-5)
  | x >= 4 = "IV" ++ showRoman (x-4)

  | x >= 1 = "I" ++ showRoman (x-1)
  | otherwise = ""

minimize x 
  | parse res == parse x = res 
  | otherwise = error $ "At " ++ show x
    where res = showRoman $ parse x
saving n = length n - length (minimize n)
main = Network.HTTP.simpleHTTP (getRequest url) >>= getResponseBody >>= print . sum . (\x -> if length x == 1000 then x else undefined) . map (saving . filter (not . isSpace)) . lines
