data Point = Point { x,y :: Int } deriving Show

f (Point {x,y}) = Point {x, y=y+1}

main = print (f (Point {x=0,y=0}))
