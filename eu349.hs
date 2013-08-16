import Data.Array

type Coord = (Int, Int)
type Field = Array Coord Bool
type Dir = (Bool, Bool) -- isHor, isPos
type State = (Field, Coord, Dir)

rot90 (True, p) = (False, p)
rot90 (False, p) = (True, not p)

stepC (x, y) (True, dir) = (x + fromEnum dir * 2 - 1, y)
stepC (x, y) (False, dir) = (x, y + fromEnum dir * 2 - 1)

step :: State -> State
step (fld, coord, dir) = (fld // [(coord, not color)], stepC coord newDir, newDir) where
  color = fld ! coord
  newDir 
    | color = rot90 dir
    | otherwise = rot90 $ rot90 $ rot90 $ dir

dirChar (True, True) = '>'
dirChar (True, False) = '<'
dirChar (False, True) = 'v'
dirChar (False, False) = '^'

draw (field, coord, dir) = mapM_ putStrLn $ let arr = fmap (\x -> if x then 'x' else ' ') field // [(coord, dirChar dir)] in [[arr ! (x, y)|x<-[-10..10]]|y<-[-10..10]]

go :: State -> Int -> IO ()
go state n = print n >> draw state >> getLine >> go (step state) (n+1)

main = go (accumArray undefined False ((-30, -30), (30, 30)) [], (0, 0), (True, True)) 0