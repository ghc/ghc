module AddLocalDecl2 where

-- |This is a function
foo = x -- comment 0
  where p = 2 -- comment 1

-- |Another fun
bar = a -- comment 2
  where nn = 2
        p = 2 -- comment 3
