module AddLocalDecl5 where

toplevel :: Integer -> Integer
toplevel x = c * x
  where
    -- c,d :: Integer
    c = 7

d = 9
