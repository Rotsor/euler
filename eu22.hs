
import Network.HTTP
import Data.List
import Data.List.Split

ascore = sum . map (\c -> fromIntegral $ fromEnum c - fromEnum 'A' + 1)

solve = sum . zipWith (*) [1..] . map ascore . sort . map (tail.init) . splitOn ","

main = do
      txt <- Network.HTTP.simpleHTTP (getRequest "http://projecteuler.net/project/names.txt") >>= getResponseBody
      print $ solve txt