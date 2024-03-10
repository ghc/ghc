{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK not-home #-}

{-|
This module exports:

  - The 'TypeError' type family, which is used to provide custom type
    errors. This is a type-level analogue to the term level error function.
  - The 'ErrorMessage' kind, used to define custom error messages.
  - The 'Unsatisfiable' constraint, a more principled variant of 'TypeError'
    which gives a more predictable way of reporting custom type errors.

@since base-4.17.0.0
-}

module GHC.Internal.TypeError
  ( ErrorMessage (..)
  , TypeError
  , Assert
  , Unsatisfiable, unsatisfiable
  ) where

import GHC.Internal.Data.Bool
import GHC.Types (TYPE, Constraint, Symbol)

{- Note [Custom type errors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TypeError is used to provide custom type errors, similar to the term-level
error function. TypeError is somewhat magical: when the constraint solver
encounters a constraint where the RHS is TypeError, it reports the error to
GHC. Later, GHC renders this error for display to the user (see the function
GHC.Tc.Errors.mkUserTypeErrorReporter).

See also the wiki page on custom type errors:
https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/custom-type-errors
-}

-- | A description of a custom type error.
data {-kind-} ErrorMessage = Text Symbol
                             -- ^ Show the text as is.

                           | forall t. ShowType t
                             -- ^ Pretty print the type.
                             -- @ShowType :: k -> ErrorMessage@

                           | ErrorMessage :<>: ErrorMessage
                             -- ^ Put two pieces of error message next
                             -- to each other.

                           | ErrorMessage :$$: ErrorMessage
                             -- ^ Stack two pieces of error message on top
                             -- of each other.

infixl 5 :$$:
infixl 6 :<>:

-- | The type-level equivalent of 'Prelude.error'.
--
-- The polymorphic kind of this type allows it to be used in several settings.
-- For instance, it can be used as a constraint, e.g. to provide a better error
-- message for a non-existent instance,
--
-- @
-- -- in a context
-- instance TypeError (Text "Cannot 'Show' functions." :$$:
--                     Text "Perhaps there is a missing argument?")
--       => Show (a -> b) where
--     showsPrec = error "unreachable"
-- @
--
-- It can also be placed on the right-hand side of a type-level function
-- to provide an error for an invalid case,
--
-- @
-- type family ByteSize x where
--    ByteSize Word16   = 2
--    ByteSize Word8    = 1
--    ByteSize a        = TypeError (Text "The type " :<>: ShowType a :<>:
--                                   Text " is not exportable.")
-- @
--
-- @since base-4.9.0.0
type family TypeError (a :: ErrorMessage) :: b where

{- Note [Getting good error messages from boolean comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to write types like

  f :: forall (x :: Int) (y :: Int). (x <= y) => T x -> T y

so we need (<=) :: Int -> Int -> Constraint. We already have
(<=?) :: Int -> Int -> Bool, defined in Data.Type.Ord. One obvious way to get
(<=) is to say

  type (<=) x y = (x <=? y) ~ True

But suppose we call (f @4 @2); then we get the constraint (4 <=? 2) ~ True
which simplifies to False ~ True, which gives a very poor error message.

So we adopt a different idiom:

  type (<=) x y = Assert (x <=? y) (LeErrMsg x y)

The Assert function is defined so that

  Assert True msg ===> ()

so as soon as (x <=? y) evaluates to True, the Assert disappears. But as soon
as (x <=? y) is apart from True (i.e. cannot evaluate to True) the second
equation of Assert kicks in, and

  Assert non-true msg ==> msg
-}

-- | A type-level assert function.
--
-- If the first argument evaluates to true, then the empty constraint is
-- returned, otherwise the second argument (which is intended to be something
-- which reduces to 'TypeError' is used).
--
-- For example, given some type level predicate @P' :: Type -> Bool@, it is
-- possible to write the type synonym
--
-- @
-- type P a = Assert (P' a) (NotPError a)
-- @
--
-- where @NotPError@ reduces to a @TypeError@ which is reported if the
-- assertion fails.
--
-- @since base-4.17.0.0
--
type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True _      = ()
  Assert _     errMsg = errMsg
  -- See Note [Getting good error messages from boolean comparisons]

{- Note [The Unsatisfiable constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The class `Unsatisfiable :: ErrorMessage -> Constraint` provides a mechanism
for custom type errors that reports the errors in a more predictable behaviour
than `TypeError`, as these constraints are handled purely during constraint solving.

The details are laid out in GHC Proposal #433 (https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0433-unsatisfiable.rst).

See Note [Implementation of Unsatisfiable constraints] in GHC.Tc.Errors for
details of the implementation in GHC.

Note [The Unsatisfiable representation-polymorphism trick]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The class method `unsatisfiableLifted :: forall (a::Type). Unsatisfiable msg => a`
works only for lifted types `a`.  What if we want an unsatisfiable value of type
`Int#`, say?  The function `unsatisfiable` has a representation-polymoprhic type
   unsatisfiable :: forall {rep} (msg :: ErrorMessage) (b :: TYPE rep).
                    Unsatisfiable msg => b
and yet is defined in terms of `unsatisfiableLifted`.  How? By instantiating
`unsatisfiableLifted` at type `(##) -> b`, and applying the result to `(##)`.
Very cunning!
-}

-- | An unsatisfiable constraint. Similar to 'TypeError' when used at the
-- 'Constraint' kind, but reports errors in a more predictable manner.
--
-- See also the 'unsatisfiable' function.
--
-- @since base-4.19.0.0@.
type Unsatisfiable :: ErrorMessage -> Constraint
class Unsatisfiable msg where
  unsatisfiableLifted :: a

-- | Prove anything within a context with an 'Unsatisfiable' constraint.
--
-- This is useful for filling in instance methods when there is an 'Unsatisfiable'
-- constraint in the instance head, e.g.:
--
-- > instance Unsatisfiable (Text "No Eq instance for functions") => Eq (a -> b) where
--     (==) = unsatisfiable
--
-- @since base-4.19.0.0@.
unsatisfiable :: forall {rep} (msg :: ErrorMessage) (a :: TYPE rep). Unsatisfiable msg => a
unsatisfiable = unsatisfiableLifted @msg @((##) -> a) (##)
  -- See Note [The Unsatisfiable representation-polymorphism trick]

