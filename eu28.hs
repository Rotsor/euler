seeds = [7,9,3,5]
lengths = [500,500,500,500]
-- lengths = [1,1,2,1]
answer = succ $ sum $ zipWith (\s l -> sum $ take l $ tail $ scanl (+) 1 $ scanl (+) (s-1) $ repeat 8) seeds lengths