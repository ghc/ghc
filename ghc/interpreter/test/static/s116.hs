--!!! Construction does not define strict field

data T = T {x :: Int, y :: !Int}

f a = T{x=a}

