--!!! No constructor has all of the fields specified
data T = T {x,y::Int}
data U = U {z::Int}

f a b c = T{x=a,y=b,z=c}

