module AddLocalDecl2 where

-- |This is a function
foo = x -- comment 0
  where p = 2 -- comment 1
 -- comment f

-- |Another fun
bar = a -- comment 2
  where p = 2 -- comment 3
        nn = 2
 -- comment b
