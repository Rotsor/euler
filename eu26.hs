
cl (x:t) = 1 + length (takeWhile (/= x) t)
cyc b = cl $ drop b $ iterate ((`mod` b) . (*10)) 1
answer = snd $ maximum [(cyc b, b)|b<-[1..999]]
main = print answer