import Data.Maybe
import Data.List

data Dir = R | D | L | U deriving (Show, Eq, Enum)
type Spiral = ([[Int]], Int, Dir) -- (rows, current row, next direction)

rows :: Spiral -> [[Int]]
rows (rs, i, d) = rs
currentrow :: Spiral -> Int
currentrow (rs, i, d) = i
nextdir :: Spiral -> Dir
nextdir (rs, i, d) = d

getrow :: Int -> [[Int]] -> Maybe [Int]
getrow i sp = if i < 0 || i >= length sp then Nothing else Just (sp!!i)

ndir :: Dir -> Dir
ndir d = if d == U then R else succ d

newsp :: Spiral
newsp = ([[1]], 0, R)

makeSpiral :: Int -> Spiral
makeSpiral i = makeSpiral' 2 newsp
    where makeSpiral' j sp = if j > i
                             then sp
                             else makeSpiral' (j+1) (update j sp)

update :: Int -> Spiral -> Spiral
update i (sp, cr, d) = (sp', cr', d')
    where oldrow  = if (d == U && cr' == cr && cr == 0) ||
                    (d == D && cr' == length sp)
                    then []
                    else fromJust $ getrow cr' sp
          cr'    | d == L || d == R = cr
                 | d == U = if cr == 0 then 0 else cr-1
                 | otherwise = cr+1
          cr''   = if d == U && cr == 0 then -1 else cr'
          sp'    = insertrow cr'' newrow sp
          newrow = case d of
                          R -> oldrow++[i]
                          D -> oldrow++[i]
                          L -> i:oldrow
                          U -> i:oldrow
          d' | d == R || d == L = if length oldrow == maximum (map length sp)
                                  then ndir d
                                  else d
             | d == U = if cr'' == -1 then ndir d else d
             | otherwise = if cr' == length sp then ndir d else d



insertrow :: Int -> [Int] -> [[Int]] -> [[Int]]
insertrow i r rs = if i == -1 then r:rs else front++[r]++back
    where (front, rest) = splitAt i rs
          back = if null rest then [] else tail rest

printSpiral :: Spiral -> IO ()
printSpiral (sp, i, d) = putStrLn (concat $ intersperse "\n" (map show sp))

sumdiags :: Spiral -> Int
sumdiags (sp, i, d) = (sumdiags' 0 0 (+1)) + (sumdiags' 0 end (subtract 1)) - centre
    where row1 = sp!!0
          end = length row1 - 1
          halfx = (length row1 `div` 2)
          halfy = (length sp `div` 2)
          centre = (sp!!halfy)!!halfx
          sumdiags' row col f = if row == length sp
                                then 0
                                else (sp!!row)!!col + sumdiags' (row+1) (f col) f

main = print (makeSpiral 10000)
