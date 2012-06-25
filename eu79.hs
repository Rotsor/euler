import Network.HTTP
import Data.List
import Data.List.Split
import Control.Monad
import Data.Function
import System.IO

url = "http://projecteuler.net/project/keylog.txt"

data Nat = Zero | Succ Nat

shortest' [] _ = ([], False)
shortest' _ [] = ([], True)
shortest' (a : as) (b : bs) = (r : rs, bChosen) where
  (rs, bChosen) = shortest' as bs
  r | bChosen = b
    | otherwise = a

shortest as bs = fst $ shortest' as bs

consumeChar :: Char -> [String] -> [String]
consumeChar x = (>>= consume)
     where
        consume (c : t) 
                | c == x = if null t then [] else [t] 
                | otherwise = [c:t]

sim :: [String] -> String -> [[String]]
sim attempts chars = scanl (flip consumeChar) attempts chars

hack :: [String] -> String
hack attempts = go attempts where
  go :: [String] -> String
  go [] = ""
  go attempts = foldr shortest (repeat $ error "should not be used!") $ do
    let 
        heads = nub (map head attempts)
        forcings = [x|x<-heads,all(not.elem x.tail)attempts]
    x <- case forcings of
       [] -> heads
       (h : _) -> return h
    guard $ any ((==x).head) attempts
    return $ {- (if null forcings then "b" else "f") ++ -} ( x : go (consumeChar x attempts))

showSpine :: [a] -> String
showSpine [] = "[]"
showSpine (h:t) = "_:" ++ showSpine t

showSpineFirst :: Show a => [a] -> String
showSpineFirst l = showSpine l ++ ". " ++ show l

getInput = fmap (wordsBy (`elem` "\r\n")) $ simpleHTTP (getRequest url) >>= getResponseBody
initt = hSetBuffering stdout NoBuffering >> hSetBuffering stdin NoBuffering

main = initt >> getInput >>= putStrLn . showSpineFirst . hack
main' = initt >> getInput >>= \inp -> getContents >>= \userInput -> mapM_ print $ sim inp userInput 

-- The input is disappointing. All the digits were forced (the first digit is 7 because 7 only occurs in the first position etc).
