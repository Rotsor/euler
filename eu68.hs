main = print $ 6531031914842725 

-- solved on paper:
--  * guessed that the outer circle should be 6 .. 10 because we need to maximize the min of them
--  * noticed that the sum should be 14 so there must be (10,1,3) or (10,3,1)
--  * noticed that there are exactly two layouts satisfying those constraints (CW and CCW)
--  * chose the best of the two layouts (653... instead of 635...)