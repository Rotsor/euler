import Network.HTTP
import qualified Data.Map as Map
import Data.Bits
import Data.List
import Data.List.Split
import Data.Ord
import Data.Char

chop3s = unfoldr (\l -> if null l then Nothing else Just $ splitAt 3 l)

dec l = map (xor key) l where
  key = fst (maximumBy (comparing snd) (Map.toList (Map.fromListWith (+) (map (\x -> (x, 1)) l)))) `xor` 32
decipher = concat . transpose . map dec . transpose . chop3s

main = simpleHTTP (getRequest "http://projecteuler.net/project/cipher1.txt") >>= getResponseBody >>= \txt -> do
     print $ (sum $ decipher $ map read (wordsBy (==',') txt) :: Int)