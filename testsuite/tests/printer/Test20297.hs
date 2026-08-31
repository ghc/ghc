{-# OPTIONS -ddump-parsed-ast #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
module Test20297 where

-- ValBinds
bar = x
  -- comment0
  where -- comment1

foo = x
  where -- comment2
        doStuff = do stuff

-- IPBinds
ipb = ?y
  where -- comment3
        ?y = 1

-- ClsInstDecl
instance C Int where -- comment4
  meth = x

-- ClassDecl
class C a where -- comment5

class D a where -- comment6
  meth :: a

-- Layout in multi-if
f = if | x -> y
       | z -> w

-- eof
