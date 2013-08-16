module Main where
import Data.List

splits [] = []
splits (h:t) = ([],h,t) : map (\(a,b,c) -> (h:a,b,c)) (splits t)

lexiperms [] = [[]]
lexiperms l = splits l >>= (\(a,b,c) -> map (b:) $ lexiperms (a ++ c))

result = (lexiperms [0..9] !! 999999)

-- another version, really fast:

toFactory :: Integer -> [Integer]
toFactory = go [1..] where
  go :: [Integer] -> Integer -> [Integer]
  go (b : bs) n | n == 0 = []
    | otherwise = n `mod` b : go bs (n `div` b)

applyFactory fact str = case genericSplitAt (genericLength fact) (reverse str) of
    (s, rest) -> reverse rest ++ go (reverse fact) (reverse s)
 where
  go [] s = []
  go (n : ns) s = case splits s !! fromIntegral n of
    (a, b, c) -> b : go ns (a ++ c)

main1 = print $ (!!999999) $ sort $ permutations [0..9]
main2 = print $ result
main3 = print $ applyFactory (toFactory 999999) ['0'..'9']
main = main3
