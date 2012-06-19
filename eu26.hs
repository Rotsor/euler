l d = head $ filter (\i -> read (replicate i '9') `mod` d == 0) [1..]
