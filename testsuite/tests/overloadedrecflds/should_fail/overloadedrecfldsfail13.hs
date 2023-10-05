-- Test that giving a stupid type annotation to an ambiguous field
-- yields a sensible error message

{-# LANGUAGE DuplicateRecordFields #-}

data S = MkS { x :: Int }
data T = MkT { x :: Bool }
data U = MkU

a = x (MkU :: U)

b = x (MkU :: a)

c :: U -> Int
c = x

d :: a -> Int
d = x

main = return ()
