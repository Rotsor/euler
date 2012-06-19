import Data.List

splits [] = []
splits (h:t) = ([],h,t) : map (\(a,b,c) -> (h:a,b,c)) (splits t)

lexiperms [] = [[]]
lexiperms l = splits l >>= (\(a,b,c) -> map (b:) $ lexiperms (a ++ c))

result = (lexiperms [0..9] !! 999999)