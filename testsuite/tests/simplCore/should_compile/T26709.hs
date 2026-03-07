module T26709 where

data T = A | B | C

f x = case x of
        A -> True
        _ -> let {-# NOINLINE j #-}
                 j y = y && not (f x)
             in case x of
                   B -> j True
                   C -> j False
