{-# OPTIONS -ddump-parsed-ast #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
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

-- FamilyDecl/ClosedTypeFamily
type family F a where -- comment7
  F Int = Bool

-- exact_condecls in GADT syntax
data G a where -- comment8
  { -- comment9
  MkG :: G Int
  ; -- comment10
  } -- comment11

-- HsMultiIf
mwi = if -- comment12
         | x -> 1
         | otherwise -> 2

-- HsUntypedBracket/DecBrL
declBracket = [d| -- comment10
                  dee = 1
                  -- comment11
                |]

-- Do statement
dodo = do -- comment12
          stmt1
          stmt2

dodo2 = do
    -- comment13
    stmt1


-- RecStmt
recdo = do
  rec -- comment14
      a <- f b
      b <- g a
  return a

-- HsCmdDo
cmd = proc x -> do -- comment15
                   returnA -< x
