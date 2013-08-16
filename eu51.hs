import Data.Numbers.Primes
import Control.Monad
import Data.Maybe
import System.IO

type Family = [Maybe Int]

members :: Family -> [Int]
members fam = map (foldr (\d a -> a*10+d) 0) $ filter ((>0).last)[map (maybe w id) fam|w<-[0..9]]

families n = filter (any isNothing) $ replicateM n (Nothing : map Just [0..9])

cntp = length . filter isPrime . members

main = do
  hSetBuffering stdout NoBuffering
  print $ minimum $ filter isPrime $ (>>=members) $ head $ dropWhile null [filter ((>=8) . cntp) $ families n|n<-[1..]]
