{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1997-1998

-}

module GHC.Types.Arity
   ( Arity
   , FullArgCount
   , JoinArity
   , RepArity
   , VisArity
   ) where

import GHC.Prelude

{-
************************************************************************
*                                                                      *
\subsection[Arity]{Arity}
*                                                                      *
************************************************************************
-}

-- | The number of value arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100     has arity 0
--  \x -> fib x has arity 1
-- See also Note [Definition of arity] in "GHC.Core.Opt.Arity"
type Arity = Int

-- | FullArgCount is the number of type or value arguments in an application,
-- or the number of type or value binders in a lambda.  Note: it includes
-- both type and value arguments!
type FullArgCount = Int

-- | Representation Arity
--
-- The number of represented arguments that can be applied to a value before it does
-- "real work". So:
--  fib 100                    has representation arity 0
--  \x -> fib x                has representation arity 1
--  \(# x, y #) -> fib (x + y) has representation arity 2
type RepArity = Int

-- | The number of arguments that a join point takes. Unlike the arity of a
-- function, this is a purely syntactic property and is fixed when the join
-- point is created (or converted from a value). Both type and value arguments
-- are counted.
type JoinArity = Int

-- | Syntactic (visibility) arity, i.e. the number of visible arguments.
-- See Note [Visibility and arity]
type VisArity = Int

{- Note [Visibility and arity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Arity is the number of arguments that a function expects. In a curried language
like Haskell, there is more than one way to count those arguments.

* `Arity` is the classic notion of arity, concerned with evalution, so it counts
  the number of /value/ arguments that need to be supplied before evaluation can
  take place, as described in notes
    Note [Definition of arity]      in GHC.Core.Opt.Arity
    Note [Arity and function types] in GHC.Types.Id.Info

  Examples:
    Int                       has arity == 0
    Int -> Int                has arity <= 1
    Int -> Bool -> Int        has arity <= 2
  We write (<=) rather than (==) as sometimes evaluation can occur before all
  value arguments are supplied, depending on the actual function definition.

  This evaluation-focused notion of arity ignores type arguments, so:
    forall a.   a             has arity == 0
    forall a.   a -> a        has arity <= 1
    forall a b. a -> b -> a   has arity <= 2
  This is true regardless of ForAllTyFlag, so the arity is also unaffected by
  (forall {a}. ty) or (forall a -> ty).

  Class dictionaries count towards the arity, as they are passed at runtime
    forall a.   (Num a)        => a            has arity <= 1
    forall a.   (Num a)        => a -> a       has arity <= 2
    forall a b. (Num a, Ord b) => a -> b -> a  has arity <= 4

* `VisArity` is the syntactic notion of arity. It is the number of /visible/
  arguments, i.e. arguments that occur visibly in the source code.

  In a function call `f x y z`, we can confidently say that f's vis-arity >= 3,
  simply because we see three arguments [x,y,z]. We write (>=) rather than (==)
  as this could be a partial application.

  At definition sites, we can acquire an underapproximation of vis-arity by
  counting the patterns on the LHS, e.g. `f a b = rhs` has vis-arity >= 2.
  The actual vis-arity can be higher if there is a lambda on the RHS,
  e.g. `f a b = \c -> rhs`.

  If we look at the types, we can observe the following
    * function arrows   (a -> b)        add to the vis-arity
    * visible foralls   (forall a -> b) add to the vis-arity
    * constraint arrows (a => b)        do not affect the vis-arity
    * invisible foralls (forall a. b)   do not affect the vis-arity

  This means that ForAllTyFlag matters for VisArity (in contrast to Arity),
  while the type/value distinction is unimportant (again in contrast to Arity).

  Examples:
    Int                         -- vis-arity == 0   (no args)
    Int -> Int                  -- vis-arity == 1   (1 funarg)
    forall a. a -> a            -- vis-arity == 1   (1 funarg)
    forall a. Num a => a -> a   -- vis-arity == 1   (1 funarg)
    forall a -> Num a => a      -- vis-arity == 1   (1 req tyarg, 0 funargs)
    forall a -> a -> a          -- vis-arity == 2   (1 req tyarg, 1 funarg)
    Int -> forall a -> Int      -- vis-arity == 2   (1 funarg, 1 req tyarg)

  Wrinkle: with TypeApplications and TypeAbstractions, it is possible to visibly
  bind and pass invisible arguments, e.g. `f @a x = ...` or `f @Int 42`. Those
  @-prefixed arguments are ignored for the purposes of vis-arity.
-}
