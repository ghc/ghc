{-# LANGUAGE Unsafe #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}


module GHC.Internal.Magic.TagToEnum
  ( TagToEnum(..)
  ) where

import GHC.Internal.Types (RuntimeRep(BoxedRep), Levity, TYPE, Constraint)
import GHC.Internal.Prim (Int#)

{- | @'tagToEnum#'@ can produce a value of any "enumeration-like" data
type given the index of the desired constructor.  For example, given
@data Ordering = LT | EQ | GT@,

>>> tagToEnum# 0# :: Ordering
LT
>>> tagToEnum# 1# :: Ordering
EQ
>>> tagToEnum# 2# :: Ordering
GT
>>> tagToEnum# 3# :: Ordering
<...undefined behavior...>

As seen in the last example, the expression @tagToEnum# x :: ty@
exhibits /undefined behavior/ if @x@ does not correspond to the index
of a constructor for the enumeration-like type @ty@:  A program that
evaluates such an expression may behave arbitrarily at run-time.

A constraint @TagToEnum ty@ for an enumeration-like type @ty@ will be
automatically solved by GHC.  A type is considered enumeration-like
if it matches a @data@ or @data instance@ declaration where

 1. There is at least one data constructor,
 2. Every data constructor has no value-level fields/arguments, and
 3. Every data constructor has no constraints from @ExistentialQuantification@.

GADT constructors are allowed. @DatatypeContexts@ are ignored. For example,

@
data X a where
  XInt  :: X Int
  XBool :: X Bool

x1 :: X Int
x1 = tagToEnum# 0#  -- equivalent to "x1 = XInt"

x2 :: X Bool
x2 = tagToEnum# 1#  -- equivalent to "x2 = XBool"

x3 :: forall a. X a
x3 = tagToEnum# 0#  -- equivalent to "x3 = unsafeCoerce XInt"
                    -- (allowed, but very dangerous)


data Y a where
  YInt :: a ~ Int => Y a
    -- This is similar to a GADT constructor of type
    -- "Y Int", but is written using a constraint.
    -- That means this declaration does not satisfy condition 3.

y1 :: Y Int
y1 = tagToEnum# 0# -- type error: no TagToEnum instance

y2 :: TagToEnum (Y Int) => Y Int
y2 = tagToEnum# 0#
  -- allowed, but use-sites will probably not be able
  -- to solve the "TagToEnum (Y Int)" constraint

data Eq a => Z a = ZCon1 | ZCon2 | ZCon3

z1 :: Z a
z1 = tagToEnum# 1# -- equivalent to "z1 = ZCon2"

data VoidTy where {}

v1 :: VoidTy
v1 = tagToEnum# 0# -- type error: no TagToEnum instance
@
-}
type TagToEnum :: forall {lev :: Levity}. TYPE (BoxedRep lev) -> Constraint
class TagToEnum a where
  tagToEnum# :: Int# -> a
