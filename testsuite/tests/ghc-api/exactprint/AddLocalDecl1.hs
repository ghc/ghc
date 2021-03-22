module AddLocalDecl1 where

-- |This is a function
foo = x -- comment1
-- trailing 1

-- |Another fun
x = a -- comment2
  where
    a = 3
-- trailing 2

y = 3
