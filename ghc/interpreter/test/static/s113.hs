--!!! Repeated field name ... in field list
data T = T {x,y   :: Int}

f a b = T{x=a,x=b}

