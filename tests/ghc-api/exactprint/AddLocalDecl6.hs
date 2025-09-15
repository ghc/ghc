module AddLocalDecl6 where

foo [] = 1 -- comment 0
foo xs = 2 -- comment 1

bar [] = 1 -- comment 2
  where
    x = 3
bar xs = 2 -- comment 3

