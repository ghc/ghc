--!!! Known bug: can't have strict fieldnames (I think this is trivial to fix)

data T = T {x :: Int, y :: !Int} deriving Show



