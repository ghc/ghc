--!!! Constructor ... does not have selected fields in ...
data T = T1 {x,y   :: Int}
       | T2 {  y,z :: Int}

f a b c = T1{y=b,z=c}

