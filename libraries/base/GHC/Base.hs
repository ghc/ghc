{-

The overall structure of the GHC Prelude is a bit tricky.

  a) We want to avoid "orphan modules", i.e. ones with instance
        decls that don't belong either to a tycon or a class
        defined in the same module

  b) We want to avoid giant modules

So the rough structure is as follows, in (linearised) dependency order


GHC.Prim        Has no implementation.  It defines built-in things, and
                by importing it you bring them into scope.
                The source file is GHC.Prim.hi-boot, which is just
                copied to make GHC.Prim.hi

GHC.Base        Classes: Eq, Ord, Functor, Monad
                Types:   list, (), Int, Bool, Ordering, Char, String

Data.Tuple      Types: tuples, plus instances for GHC.Base classes

GHC.Show        Class: Show, plus instances for GHC.Base/GHC.Tup types

GHC.Enum        Class: Enum,  plus instances for GHC.Base/GHC.Tup types

Data.Maybe      Type: Maybe, plus instances for GHC.Base classes

GHC.List        List functions

GHC.Num         Class: Num, plus instances for Int
                Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

                Integer is needed here because it is mentioned in the signature
                of 'fromInteger' in class Num

GHC.Real        Classes: Real, Integral, Fractional, RealFrac
                         plus instances for Int, Integer
                Types:  Ratio, Rational
                        plus instances for classes so far

                Rational is needed here because it is mentioned in the signature
                of 'toRational' in class Real

GHC.ST  The ST monad, instances and a few helper functions

Ix              Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

GHC.Arr         Types: Array, MutableArray, MutableVar

                Arrays are used by a function in GHC.Float

GHC.Float       Classes: Floating, RealFloat
                Types:   Float, Double, plus instances of all classes so far

                This module contains everything to do with floating point.
                It is a big module (900 lines)
                With a bit of luck, many modules can be compiled without ever reading GHC.Float.hi


Other Prelude modules are much easier with fewer complex dependencies.
-}

{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP
           , NoImplicitPrelude
           , BangPatterns
           , ExplicitForAll
           , MagicHash
           , UnboxedTuples
           , ExistentialQuantification
           , RankNTypes
           , KindSignatures
           , PolyKinds
           , DataKinds
  #-}
-- -Wno-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Base
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic data types and classes.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Base
        (
        module GHC.Base,
        module GHC.Classes,
        module GHC.CString,
        module GHC.Magic,
        module GHC.Types,
        module GHC.Prim,        -- Re-export GHC.Prim and [boot] GHC.Err,
        module GHC.Prim.Ext,    -- to avoid lots of people having to
        module GHC.Err,         -- import it explicitly
        module GHC.Maybe
  )
        where

import GHC.Types
import GHC.Classes
import GHC.CString
import GHC.Magic
import GHC.Prim
import GHC.Prim.Ext
import GHC.Err
import GHC.Maybe
import {-# SOURCE #-} GHC.IO (mkUserError, mplusIO)

import GHC.Tuple ()              -- Note [Depend on GHC.Tuple]
import GHC.Integer ()            -- Note [Depend on GHC.Integer]
import GHC.Natural ()            -- Note [Depend on GHC.Natural]

-- for 'class Semigroup'
import {-# SOURCE #-} GHC.Real (Integral)
import {-# SOURCE #-} Data.Semigroup.Internal ( stimesDefault
                                              , stimesMaybe
                                              , stimesList
                                              , stimesIdempotentMonoid
                                              )

infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!

infixl 4 <*>, <*, *>, <**>

default ()              -- Double isn't available yet

{-
Note [Depend on GHC.Integer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Integer type is special because GHC.Iface.Tidy uses
GHC.Integer.Type.mkInteger to construct Integer literal values
Currently it reads the interface file whether or not the current
module *has* any Integer literals, so it's important that
GHC.Integer.Type (in package integer-gmp or integer-simple) is
compiled before any other module.  (There's a hack in GHC to disable
this for packages ghc-prim, integer-gmp, integer-simple, which aren't
allowed to contain any Integer literals.)

Likewise we implicitly need Integer when deriving things like Eq
instances.

The danger is that if the build system doesn't know about the dependency
on Integer, it'll compile some base module before GHC.Integer.Type,
resulting in:
  Failed to load interface for ‘GHC.Integer.Type’
    There are files missing in the ‘integer-gmp’ package,

Bottom line: we make GHC.Base depend on GHC.Integer; and everything
else either depends on GHC.Base, or does not have NoImplicitPrelude
(and hence depends on Prelude).

Note [Depend on GHC.Tuple]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Similarly, tuple syntax (or ()) creates an implicit dependency on
GHC.Tuple, so we use the same rule as for Integer --- see Note [Depend on
GHC.Integer] --- to explain this to the build system.  We make GHC.Base
depend on GHC.Tuple, and everything else depends on GHC.Base or Prelude.

Note [Depend on GHC.Natural]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Similar to GHC.Integer.
-}

#if 0
-- for use when compiling GHC.Base itself doesn't work
data  Bool  =  False | True
data Ordering = LT | EQ | GT
data Char = C# Char#
type  String = [Char]
data Int = I# Int#
data  ()  =  ()
data [] a = MkNil

not True = False
(&&) True True = True
otherwise = True

build = errorWithoutStackTrace "urk"
foldr = errorWithoutStackTrace "urk"
#endif

infixr 6 <>

-- | The class of semigroups (types with an associative binary operation).
--
-- Instances should satisfy the following:
--
-- [Associativity] @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
--
-- @since 4.9.0.0
class Semigroup a where
        -- | An associative operation.
        --
        -- >>> [1,2,3] <> [4,5,6]
        -- [1,2,3,4,5,6]
        (<>) :: a -> a -> a

        -- | Reduce a non-empty list with '<>'
        --
        -- The default definition should be sufficient, but this can be
        -- overridden for efficiency.
        --
        -- >>> import Data.List.NonEmpty
        -- >>> sconcat $ "Hello" :| [" ", "Haskell", "!"]
        -- "Hello Haskell!"
        sconcat :: NonEmpty a -> a
        sconcat (a :| as) = go a as where
          go b (c:cs) = b <> go c cs
          go b []     = b

        -- | Repeat a value @n@ times.
        --
        -- Given that this works on a 'Semigroup' it is allowed to fail if
        -- you request 0 or fewer repetitions, and the default definition
        -- will do so.
        --
        -- By making this a member of the class, idempotent semigroups
        -- and monoids can upgrade this to execute in \(\mathcal{O}(1)\) by
        -- picking @stimes = 'Data.Semigroup.stimesIdempotent'@ or @stimes =
        -- 'stimesIdempotentMonoid'@ respectively.
        --
        -- >>> stimes 4 [1]
        -- [1,1,1,1]
        stimes :: Integral b => b -> a -> a
        stimes = stimesDefault


-- | The class of monoids (types with an associative binary operation that
-- has an identity).  Instances should satisfy the following:
--
-- [Right identity] @x '<>' 'mempty' = x@
-- [Left identity]  @'mempty' '<>' x = x@
-- [Associativity]  @x '<>' (y '<>' z) = (x '<>' y) '<>' z@ ('Semigroup' law)
-- [Concatenation]  @'mconcat' = 'foldr' ('<>') 'mempty'@
--
-- The method names refer to the monoid of lists under concatenation,
-- but there are many other instances.
--
-- Some types can be viewed as a monoid in more than one way,
-- e.g. both addition and multiplication on numbers.
-- In such cases we often define @newtype@s and make those instances
-- of 'Monoid', e.g. 'Data.Semigroup.Sum' and 'Data.Semigroup.Product'.
--
-- __NOTE__: 'Semigroup' is a superclass of 'Monoid' since /base-4.11.0.0/.
class Semigroup a => Monoid a where
        -- | Identity of 'mappend'
        --
        -- >>> "Hello world" <> mempty
        -- "Hello world"
        mempty  :: a

        -- | An associative operation
        --
        -- __NOTE__: This method is redundant and has the default
        -- implementation @'mappend' = ('<>')@ since /base-4.11.0.0/.
        -- Should it be implemented manually, since 'mappend' is a synonym for
        -- ('<>'), it is expected that the two functions are defined the same
        -- way. In a future GHC release 'mappend' will be removed from 'Monoid'.
        mappend :: a -> a -> a
        mappend = (<>)
        {-# INLINE mappend #-}

        -- | Fold a list using the monoid.
        --
        -- For most types, the default definition for 'mconcat' will be
        -- used, but the function is included in the class definition so
        -- that an optimized version can be provided for specific types.
        --
        -- >>> mconcat ["Hello", " ", "Haskell", "!"]
        -- "Hello Haskell!"
        mconcat :: [a] -> a
        mconcat = foldr mappend mempty

-- | @since 4.9.0.0
instance Semigroup [a] where
        (<>) = (++)
        {-# INLINE (<>) #-}

        stimes = stimesList

-- | @since 2.01
instance Monoid [a] where
        {-# INLINE mempty #-}
        mempty  = []
        {-# INLINE mconcat #-}
        mconcat xss = [x | xs <- xss, x <- xs]
-- See Note: [List comprehensions and inlining]

{-
Note: [List comprehensions and inlining]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The list monad operations are traditionally described in terms of concatMap:

xs >>= f = concatMap f xs

Similarly, mconcat for lists is just concat. Here in Base, however, we don't
have concatMap, and we'll refrain from adding it here so it won't have to be
hidden in imports. Instead, we use GHC's list comprehension desugaring
mechanism to define mconcat and the Applicative and Monad instances for lists.
We mark them INLINE because the inliner is not generally too keen to inline
build forms such as the ones these desugar to without our insistence.  Defining
these using list comprehensions instead of foldr has an additional potential
benefit, as described in compiler/deSugar/DsListComp.hs: if optimizations
needed to make foldr/build forms efficient are turned off, we'll get reasonably
efficient translations anyway.
-}

-- | @since 4.9.0.0
instance Semigroup (NonEmpty a) where
        (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

-- | @since 4.9.0.0
instance Semigroup b => Semigroup (a -> b) where
        f <> g = \x -> f x <> g x
        stimes n f e = stimes n (f e)

-- | @since 2.01
instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty

-- | @since 4.9.0.0
instance Semigroup () where
        _ <> _      = ()
        sconcat _   = ()
        stimes  _ _ = ()

-- | @since 2.01
instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        mconcat _     = ()

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
        (a,b) <> (a',b') = (a<>a',b<>b')
        stimes n (a,b) = (stimes n a, stimes n b)

-- | @since 2.01
instance (Monoid a, Monoid b) => Monoid (a,b) where
        mempty = (mempty, mempty)

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
        (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
        stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

-- | @since 2.01
instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
        mempty = (mempty, mempty, mempty)

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
        (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
        stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

-- | @since 2.01
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
        mempty = (mempty, mempty, mempty, mempty)

-- | @since 4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
        (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
        stimes n (a,b,c,d,e) =
            (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

-- | @since 2.01
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                Monoid (a,b,c,d,e) where
        mempty = (mempty, mempty, mempty, mempty, mempty)


-- | @since 4.9.0.0
instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

    stimes = stimesIdempotentMonoid

-- lexicographical ordering
-- | @since 2.01
instance Monoid Ordering where
    mempty             = EQ

-- | @since 4.9.0.0
instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

    stimes = stimesMaybe

-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\"
--
-- /Since 4.11.0/: constraint on inner @a@ value generalised from
-- 'Monoid' to 'Semigroup'.
--
-- @since 2.01
instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

-- | For tuples, the 'Monoid' constraint on @a@ determines
-- how the first values merge.
-- For example, 'String's concatenate:
--
-- > ("hello ", (+15)) <*> ("world!", 2002)
-- > ("hello world!",2017)
--
-- @since 2.01
instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)
    liftA2 f (u, x) (v, y) = (u <> v, f x y)

-- | @since 4.9.0.0
instance Monoid a => Monad ((,) a) where
    (u, a) >>= k = case k a of (v, b) -> (u <> v, b)

-- | @since 4.14.0.0
instance Functor ((,,) a b) where
    fmap f (a, b, c) = (a, b, f c)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b) => Applicative ((,,) a b) where
    pure x = (mempty, mempty, x)
    (a, b, f) <*> (a', b', x) = (a <> a', b <> b', f x)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b) => Monad ((,,) a b) where
    (u, v, a) >>= k = case k a of (u', v', b) -> (u <> u', v <> v', b)

-- | @since 4.14.0.0
instance Functor ((,,,) a b c) where
    fmap f (a, b, c, d) = (a, b, c, f d)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b, Monoid c) => Applicative ((,,,) a b c) where
    pure x = (mempty, mempty, mempty, x)
    (a, b, c, f) <*> (a', b', c', x) = (a <> a', b <> b', c <> c', f x)

-- | @since 4.14.0.0
instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c) where
    (u, v, w, a) >>= k = case k a of (u', v', w', b) -> (u <> u', v <> v', w <> w', b)

-- | @since 4.10.0.0
instance Semigroup a => Semigroup (IO a) where
    (<>) = liftA2 (<>)

-- | @since 4.9.0.0
instance Monoid a => Monoid (IO a) where
    mempty = pure mempty

{- | A type @f@ is a Functor if it provides a function @fmap@ which, given any types @a@ and @b@
lets you apply any function from @(a -> b)@ to turn an @f a@ into an @f b@, preserving the
structure of @f@. Furthermore @f@ needs to adhere to the following:

[Identity]    @'fmap' 'id' == 'id'@
[Composition] @'fmap' (f . g) == 'fmap' f . 'fmap' g@

Note, that the second law follows from the free theorem of the type 'fmap' and
the first law, so you need only check that the former condition holds.
-}

class  Functor f  where
    -- | Using @ApplicativeDo@: \'@'fmap' f as@\' can be understood as
    -- the @do@ expression
    --
    -- @
    -- do a <- as
    --    pure (f a)
    -- @
    --
    -- with an inferred @Functor@ constraint.
    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    --
    -- Using @ApplicativeDo@: \'@a '<$' bs@\' can be understood as the
    -- @do@ expression
    --
    -- @
    -- do bs
    --    pure a
    -- @
    --
    -- with an inferred @Functor@ constraint.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const

-- | A functor with application, providing operations to
--
-- * embed pure expressions ('pure'), and
--
-- * sequence computations and combine their results ('<*>' and 'liftA2').
--
-- A minimal complete definition must include implementations of 'pure'
-- and of either '<*>' or 'liftA2'. If it defines both, then they must behave
-- the same as their default definitions:
--
--      @('<*>') = 'liftA2' 'id'@
--
--      @'liftA2' f x y = f 'Prelude.<$>' x '<*>' y@
--
-- Further, any definition must satisfy the following:
--
-- [Identity]
--
--      @'pure' 'id' '<*>' v = v@
--
-- [Composition]
--
--      @'pure' (.) '<*>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)@
--
-- [Homomorphism]
--
--      @'pure' f '<*>' 'pure' x = 'pure' (f x)@
--
-- [Interchange]
--
--      @u '<*>' 'pure' y = 'pure' ('$' y) '<*>' u@
--
--
-- The other methods have the following default definitions, which may
-- be overridden with equivalent specialized implementations:
--
--   * @u '*>' v = ('id' '<$' u) '<*>' v@
--
--   * @u '<*' v = 'liftA2' 'const' u v@
--
-- As a consequence of these laws, the 'Functor' instance for @f@ will satisfy
--
--   * @'fmap' f x = 'pure' f '<*>' x@
--
--
-- It may be useful to note that supposing
--
--      @forall x y. p (q x y) = f x . g y@
--
-- it follows from the above that
--
--      @'liftA2' p ('liftA2' q u v) = 'liftA2' f u . 'liftA2' g v@
--
--
-- If @f@ is also a 'Monad', it should satisfy
--
--   * @'pure' = 'return'@
--
--   * @m1 '<*>' m2 = m1 '>>=' (\x1 -> m2 '>>=' (\x2 -> 'return' (x1 x2)))@
--
--   * @('*>') = ('>>')@
--
-- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).

class Functor f => Applicative f where
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
    -- | Lift a value.
    pure :: a -> f a

    -- | Sequential application.
    --
    -- A few functors support an implementation of '<*>' that is more
    -- efficient than the default one.
    --
    -- Using @ApplicativeDo@: \'@fs '<*>' as@\' can be understood as
    -- the @do@ expression
    --
    -- @
    -- do f <- fs
    --    a <- as
    --    pure (f a)
    -- @
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    -- | Lift a binary function to actions.
    --
    -- Some functors support an implementation of 'liftA2' that is more
    -- efficient than the default one. In particular, if 'fmap' is an
    -- expensive operation, it is likely better to use 'liftA2' than to
    -- 'fmap' over the structure and then use '<*>'.
    --
    -- This became a typeclass method in 4.10.0.0. Prior to that, it was
    -- a function defined in terms of '<*>' and 'fmap'.
    --
    -- Using @ApplicativeDo@: \'@'liftA2' f as bs@\' can be understood
    -- as the @do@ expression
    --
    -- @
    -- do a <- as
    --    b <- bs
    --    pure (f a b)
    -- @

    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    -- | Sequence actions, discarding the value of the first argument.
    --
    -- \'@as '*>' bs@\' can be understood as the @do@ expression
    --
    -- @
    -- do as
    --    bs
    -- @
    --
    -- This is a tad complicated for our @ApplicativeDo@ extension
    -- which will give it a @Monad@ constraint. For an @Applicative@
    -- constraint we write it of the form
    --
    -- @
    -- do _ <- as
    --    b <- bs
    --    pure b
    -- @
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2
    -- This is essentially the same as liftA2 (flip const), but if the
    -- Functor instance has an optimized (<$), it may be better to use
    -- that instead. Before liftA2 became a method, this definition
    -- was strictly better, but now it depends on the functor. For a
    -- functor supporting a sharing-enhancing (<$), this definition
    -- may reduce allocation by preventing a1 from ever being fully
    -- realized. In an implementation with a boring (<$) but an optimizing
    -- liftA2, it would likely be better to define (*>) using liftA2.

    -- | Sequence actions, discarding the value of the second argument.
    --
    -- Using @ApplicativeDo@: \'@as '<*' bs@\' can be understood as
    -- the @do@ expression
    --
    -- @
    -- do a <- as
    --    bs
    --    pure a
    -- @
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const

-- | A variant of '<*>' with the arguments reversed.
--
-- Using @ApplicativeDo@: \'@as '<**>' fs@\' can be understood as the
-- @do@ expression
--
-- @
-- do a <- as
--    f <- fs
--    pure (f a)
-- @
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)
-- Don't use $ here, see the note at the top of the page

-- | Lift a function to actions.
-- This function may be used as a value for `fmap` in a `Functor` instance.
--
-- Using @ApplicativeDo@: \'@'liftA' f as@\' can be understood as the
-- @do@ expression
--
--
-- @
-- do a <- as
--    pure (f a)
-- @
--
-- with an inferred @Functor@ constraint, weaker than @Applicative@.
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a
-- Caution: since this may be used for `fmap`, we can't use the obvious
-- definition of liftA = fmap.

-- | Lift a ternary function to actions.
--
-- Using @ApplicativeDo@: \'@'liftA3' f as bs cs@\' can be understood
-- as the @do@ expression
--
-- @
-- do a <- as
--    b <- bs
--    c <- cs
--    pure (f a b c)
-- @
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c


{-# INLINABLE liftA #-}
{-# SPECIALISE liftA :: (a1->r) -> IO a1 -> IO r #-}
{-# SPECIALISE liftA :: (a1->r) -> Maybe a1 -> Maybe r #-}
{-# INLINABLE liftA3 #-}
{-# SPECIALISE liftA3 :: (a1->a2->a3->r) -> IO a1 -> IO a2 -> IO a3 -> IO r #-}
{-# SPECIALISE liftA3 :: (a1->a2->a3->r) ->
                                Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe r #-}

-- | The 'join' function is the conventional monad join operator. It
-- is used to remove one level of monadic structure, projecting its
-- bound argument into the outer level.
--
--
-- \'@'join' bss@\' can be understood as the @do@ expression
--
-- @
-- do bs <- bss
--    bs
-- @
--
-- ==== __Examples__
--
-- A common use of 'join' is to run an 'IO' computation returned from
-- an 'GHC.Conc.STM' transaction, since 'GHC.Conc.STM' transactions
-- can't perform 'IO' directly. Recall that
--
-- @
-- 'GHC.Conc.atomically' :: STM a -> IO a
-- @
--
-- is used to run 'GHC.Conc.STM' transactions atomically. So, by
-- specializing the types of 'GHC.Conc.atomically' and 'join' to
--
-- @
-- 'GHC.Conc.atomically' :: STM (IO b) -> IO (IO b)
-- 'join'       :: IO (IO b)  -> IO b
-- @
--
-- we can compose them as
--
-- @
-- 'join' . 'GHC.Conc.atomically' :: STM (IO b) -> IO b
-- @
--
-- to run an 'GHC.Conc.STM' transaction and the 'IO' action it
-- returns.
join              :: (Monad m) => m (m a) -> m a
join x            =  x >>= id

{- | The 'Monad' class defines the basic operations over a /monad/,
a concept from a branch of mathematics known as /category theory/.
From the perspective of a Haskell programmer, however, it is best to
think of a monad as an /abstract datatype/ of actions.
Haskell's @do@ expressions provide a convenient syntax for writing
monadic expressions.

Instances of 'Monad' should satisfy the following:

[Left identity]  @'return' a '>>=' k  =  k a@
[Right identity] @m '>>=' 'return'  =  m@
[Associativity]  @m '>>=' (\\x -> k x '>>=' h)  =  (m '>>=' k) '>>=' h@

Furthermore, the 'Monad' and 'Applicative' operations should relate as follows:

* @'pure' = 'return'@
* @m1 '<*>' m2 = m1 '>>=' (\x1 -> m2 '>>=' (\x2 -> 'return' (x1 x2)))@

The above laws imply:

* @'fmap' f xs  =  xs '>>=' 'return' . f@
* @('>>') = ('*>')@

and that 'pure' and ('<*>') satisfy the applicative functor laws.

The instances of 'Monad' for lists, 'Data.Maybe.Maybe' and 'System.IO.IO'
defined in the "Prelude" satisfy these laws.
-}
class Applicative m => Monad m where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    --
    -- \'@as '>>=' bs@\' can be understood as the @do@ expression
    --
    -- @
    -- do a <- as
    --    bs a
    -- @
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    --
    -- \'@as '>>' bs@\' can be understood as the @do@ expression
    --
    -- @
    -- do as
    --    bs
    -- @
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
    {-# INLINE (>>) #-}

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    return      = pure

{- Note [Recursive bindings for Applicative/Monad]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The original Applicative/Monad proposal stated that after
implementation, the designated implementation of (>>) would become

  (>>) :: forall a b. m a -> m b -> m b
  (>>) = (*>)

by default. You might be inclined to change this to reflect the stated
proposal, but you really shouldn't! Why? Because people tend to define
such instances the /other/ way around: in particular, it is perfectly
legitimate to define an instance of Applicative (*>) in terms of (>>),
which would lead to an infinite loop for the default implementation of
Monad! And people do this in the wild.

This turned into a nasty bug that was tricky to track down, and rather
than eliminate it everywhere upstream, it's easier to just retain the
original default.

-}

-- | Same as '>>=', but with the arguments interchanged.
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f

-- | Conditional execution of 'Applicative' expressions. For example,
--
-- > when debug (putStrLn "Debugging")
--
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
when      :: (Applicative f) => Bool -> f () -> f ()
{-# INLINABLE when #-}
{-# SPECIALISE when :: Bool -> IO () -> IO () #-}
{-# SPECIALISE when :: Bool -> Maybe () -> Maybe () #-}
when p s  = if p then s else pure ()

-- | Evaluate each action in the sequence from left to right,
-- and collect the results.
sequence :: Monad m => [m a] -> m [a]
{-# INLINE sequence #-}
sequence = mapM id
-- Note: [sequence and mapM]

-- | @'mapM' f@ is equivalent to @'sequence' . 'map' f@.
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
{-# INLINE mapM #-}
mapM f as = foldr k (return []) as
            where
              k a r = do { x <- f a; xs <- r; return (x:xs) }

{-
Note: [sequence and mapM]
~~~~~~~~~~~~~~~~~~~~~~~~~
Originally, we defined

mapM f = sequence . map f

This relied on list fusion to produce efficient code for mapM, and led to
excessive allocation in cryptarithm2. Defining

sequence = mapM id

relies only on inlining a tiny function (id) and beta reduction, which tends to
be a more reliable aspect of simplification. Indeed, this does not lead to
similar problems in nofib.
-}

-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.  For example,
--
-- > liftM2 (+) [0,1] [0,2] = [0,2,1,3]
-- > liftM2 (+) (Just 1) Nothing = Nothing
--
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
-- Caution: since this may be used for `liftA2`, we can't use the obvious
-- definition of liftM2 = liftA2.

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM3  :: (Monad m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3       = do { x1 <- m1; x2 <- m2; x3 <- m3; return (f x1 x2 x3) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM4  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m r
liftM4 f m1 m2 m3 m4    = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; return (f x1 x2 x3 x4) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right (cf. 'liftM2').
liftM5  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m r
liftM5 f m1 m2 m3 m4 m5 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; return (f x1 x2 x3 x4 x5) }

{-# INLINABLE liftM #-}
{-# SPECIALISE liftM :: (a1->r) -> IO a1 -> IO r #-}
{-# SPECIALISE liftM :: (a1->r) -> Maybe a1 -> Maybe r #-}
{-# INLINABLE liftM2 #-}
{-# SPECIALISE liftM2 :: (a1->a2->r) -> IO a1 -> IO a2 -> IO r #-}
{-# SPECIALISE liftM2 :: (a1->a2->r) -> Maybe a1 -> Maybe a2 -> Maybe r #-}
{-# INLINABLE liftM3 #-}
{-# SPECIALISE liftM3 :: (a1->a2->a3->r) -> IO a1 -> IO a2 -> IO a3 -> IO r #-}
{-# SPECIALISE liftM3 :: (a1->a2->a3->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe r #-}
{-# INLINABLE liftM4 #-}
{-# SPECIALISE liftM4 :: (a1->a2->a3->a4->r) -> IO a1 -> IO a2 -> IO a3 -> IO a4 -> IO r #-}
{-# SPECIALISE liftM4 :: (a1->a2->a3->a4->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe r #-}
{-# INLINABLE liftM5 #-}
{-# SPECIALISE liftM5 :: (a1->a2->a3->a4->a5->r) -> IO a1 -> IO a2 -> IO a3 -> IO a4 -> IO a5 -> IO r #-}
{-# SPECIALISE liftM5 :: (a1->a2->a3->a4->a5->r) -> Maybe a1 -> Maybe a2 -> Maybe a3 -> Maybe a4 -> Maybe a5 -> Maybe r #-}

{- | In many situations, the 'liftM' operations can be replaced by uses of
'ap', which promotes function application.

> return f `ap` x1 `ap` ... `ap` xn

is equivalent to

> liftMn f x1 x2 ... xn

-}

ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- Since many Applicative instances define (<*>) = ap, we
-- cannot define ap = (<*>)
{-# INLINABLE ap #-}
{-# SPECIALISE ap :: IO (a -> b) -> IO a -> IO b #-}
{-# SPECIALISE ap :: Maybe (a -> b) -> Maybe a -> Maybe b #-}

-- instances for Prelude types

-- | @since 2.01
instance Functor ((->) r) where
    fmap = (.)

-- | @since 2.01
instance Applicative ((->) r) where
    pure = const
    (<*>) f g x = f x (g x)
    liftA2 q f g x = q (f x) (g x)

-- | @since 2.01
instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r

-- | @since 2.01
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

-- | @since 2.01
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

-- | @since 2.01
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing

-- | @since 2.01
instance  Monad Maybe  where
    (Just x) >>= k      = k x
    Nothing  >>= _      = Nothing

    (>>) = (*>)

-- -----------------------------------------------------------------------------
-- The Alternative class definition

infixl 3 <|>

-- | A monoid on applicative functors.
--
-- If defined, 'some' and 'many' should be the least solutions
-- of the equations:
--
-- * @'some' v = (:) 'Prelude.<$>' v '<*>' 'many' v@
--
-- * @'many' v = 'some' v '<|>' 'pure' []@
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    -- | Zero or more.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v


-- | @since 2.01
instance Alternative Maybe where
    empty = Nothing
    Nothing <|> r = r
    l       <|> _ = l

-- -----------------------------------------------------------------------------
-- The MonadPlus class definition

-- | Monads that also support choice and failure.
class (Alternative m, Monad m) => MonadPlus m where
   -- | The identity of 'mplus'.  It should also satisfy the equations
   --
   -- > mzero >>= f  =  mzero
   -- > v >> mzero   =  mzero
   --
   -- The default definition is
   --
   -- @
   -- mzero = 'empty'
   -- @
   mzero :: m a
   mzero = empty

   -- | An associative operation. The default definition is
   --
   -- @
   -- mplus = ('<|>')
   -- @
   mplus :: m a -> m a -> m a
   mplus = (<|>)

-- | @since 2.01
instance MonadPlus Maybe

---------------------------------------------
-- The non-empty list type

infixr 5 :|

-- | Non-empty (and non-strict) list type.
--
-- @since 4.9.0.0
data NonEmpty a = a :| [a]
  deriving ( Eq  -- ^ @since 4.9.0.0
           , Ord -- ^ @since 4.9.0.0
           )

-- | @since 4.9.0.0
instance Functor NonEmpty where
  fmap f ~(a :| as) = f a :| fmap f as
  b <$ ~(_ :| as)   = b   :| (b <$ as)

-- | @since 4.9.0.0
instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) = ap
  liftA2 = liftM2

-- | @since 4.9.0.0
instance Monad NonEmpty where
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f
          toList ~(c :| cs) = c : cs

----------------------------------------------
-- The list type

-- | @since 2.01
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map

-- See Note: [List comprehensions and inlining]
-- | @since 2.01
instance Applicative [] where
    {-# INLINE pure #-}
    pure x    = [x]
    {-# INLINE (<*>) #-}
    fs <*> xs = [f x | f <- fs, x <- xs]
    {-# INLINE liftA2 #-}
    liftA2 f xs ys = [f x y | x <- xs, y <- ys]
    {-# INLINE (*>) #-}
    xs *> ys  = [y | _ <- xs, y <- ys]

-- See Note: [List comprehensions and inlining]
-- | @since 2.01
instance Monad []  where
    {-# INLINE (>>=) #-}
    xs >>= f             = [y | x <- xs, y <- f x]
    {-# INLINE (>>) #-}
    (>>) = (*>)

-- | @since 2.01
instance Alternative [] where
    empty = []
    (<|>) = (++)

-- | @since 2.01
instance MonadPlus []

{-
A few list functions that appear here because they are used here.
The rest of the prelude list functions are in GHC.List.
-}

----------------------------------------------
--      foldr/build/augment
----------------------------------------------

-- | 'foldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a list, reduces the list
-- using the binary operator, from right to left:
--
-- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

foldr            :: (a -> b -> b) -> b -> [a] -> b
-- foldr _ z []     =  z
-- foldr f z (x:xs) =  f x (foldr f z xs)
{-# INLINE [0] foldr #-}
-- Inline only in the final stage, after the foldr/cons rule has had a chance
-- Also note that we inline it when it has *two* parameters, which are the
-- ones we are keen about specialising!
foldr k z = go
          where
            go []     = z
            go (y:ys) = y `k` go ys

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >    build g = g (:) []
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('build' g)@, which may arise after inlining, to @g k z@,
-- which avoids producing an intermediate list.

build   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
{-# INLINE [1] build #-}
        -- The INLINE is important, even though build is tiny,
        -- because it prevents [] getting inlined in the version that
        -- appears in the interface file.  If [] *is* inlined, it
        -- won't match with [] appearing in rules in an importing module.
        --
        -- The "1" says to inline in phase 1

build g = g (:) []

-- | A list producer that can be fused with 'foldr'.
-- This function is merely
--
-- >    augment g xs = g (:) xs
--
-- but GHC's simplifier will transform an expression of the form
-- @'foldr' k z ('augment' g xs)@, which may arise after inlining, to
-- @g k ('foldr' k z xs)@, which avoids producing an intermediate list.

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) .
                foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"                        foldr (:) [] = \x  -> x
"foldr/app"     [1] forall ys. foldr (:) ys = \xs -> xs ++ ys
        -- Only activate this from phase 1, because that's
        -- when we disable the rule that expands (++) into foldr

-- The foldr/cons rule looks nice, but it can give disastrously
-- bloated code when commpiling
--      array (a,b) [(1,2), (2,2), (3,2), ...very long list... ]
-- i.e. when there are very very long literal lists
-- So I've disabled it for now. We could have special cases
-- for short lists, I suppose.
-- "foldr/cons" forall k z x xs. foldr k z (x:xs) = k x (foldr k z xs)

"foldr/single"  forall k z x. foldr k z [x] = k x z
"foldr/nil"     forall k z.   foldr k z []  = z

"foldr/cons/build" forall k z x (g::forall b. (a->b->b) -> b -> b) .
                           foldr k z (x:build g) = k x (g k z)

"augment/build" forall (g::forall b. (a->b->b) -> b -> b)
                       (h::forall b. (a->b->b) -> b -> b) .
                       augment g (build h) = build (\c n -> g c (h c n))
"augment/nil"   forall (g::forall b. (a->b->b) -> b -> b) .
                        augment g [] = build g
 #-}

-- This rule is true, but not (I think) useful:
--      augment g (augment h t) = augment (\cn -> g c (h c n)) t

----------------------------------------------
--              map
----------------------------------------------

-- | \(\mathcal{O}(n)\). 'map' @f xs@ is the list obtained by applying @f@ to
-- each element of @xs@, i.e.,
--
-- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- > map f [x1, x2, ...] == [f x1, f x2, ...]
--
-- >>> map (+1) [1, 2, 3]
-- [2,3,4]
map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
  -- We want the RULEs "map" and "map/coerce" to fire first.
  -- map is recursive, so won't inline anyway,
  -- but saying so is more explicit, and silences warnings
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-} -- See Note [Inline FB functions] in GHC.List
mapFB c f = \x ys -> c (f x) ys

{- Note [The rules for map]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
The rules for map work like this.

* Up to (but not including) phase 1, we use the "map" rule to
  rewrite all saturated applications of map with its build/fold
  form, hoping for fusion to happen.

  In phase 1 and 0, we switch off that rule, inline build, and
  switch on the "mapList" rule, which rewrites the foldr/mapFB
  thing back into plain map.

  It's important that these two rules aren't both active at once
  (along with build's unfolding) else we'd get an infinite loop
  in the rules.  Hence the activation control below.

* This same pattern is followed by many other functions:
  e.g. append, filter, iterate, repeat, etc. in GHC.List

  See also Note [Inline FB functions] in GHC.List

* The "mapFB" rule optimises compositions of map

* The "mapFB/id" rule gets rid of 'map id' calls.
  You might think that (mapFB c id) will turn into c simply
  when mapFB is inlined; but before that happens the "mapList"
  rule turns
     (foldr (mapFB (:) id) [] a
  back into
     map id
  Which is not very clever.

* Any similarity to the Functor laws for [] is expected.
-}

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
"mapFB/id"  forall c.           mapFB c (\x -> x)       = c
  #-}

-- See Breitner, Eisenberg, Peyton Jones, and Weirich, "Safe Zero-cost
-- Coercions for Haskell", section 6.5:
--   http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf

{-# RULES "map/coerce" [1] map coerce = coerce #-}
-- See Note [Getting the map/coerce RULE to work] in CoreOpt

----------------------------------------------
--              append
----------------------------------------------

-- | Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
--
-- If the first list is not finite, the result is the first list.

(++) :: [a] -> [a] -> [a]
{-# NOINLINE [1] (++) #-}    -- We want the RULE to fire first.
                             -- It's recursive, so won't inline anyway,
                             -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++"    [~1] forall xs ys. xs ++ ys = augment (\c n -> foldr c n xs) ys
  #-}


-- |'otherwise' is defined as the value 'True'.  It helps to make
-- guards more readable.  eg.
--
-- >  f x | x < 0     = ...
-- >      | otherwise = ...
otherwise               :: Bool
otherwise               =  True

----------------------------------------------
-- Type Char and String
----------------------------------------------

-- | A 'String' is a list of characters.  String constants in Haskell are values
-- of type 'String'.
--
-- See "Data.List" for operations on lists.
type String = [Char]

unsafeChr :: Int -> Char
unsafeChr (I# i#) = C# (chr# i#)

-- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

-- | This 'String' equality predicate is used when desugaring
-- pattern-matches against strings.
eqString :: String -> String -> Bool
eqString []       []       = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _        _        = False

{-# RULES "eqString" (==) = eqString #-}
-- eqString also has a BuiltInRule in GHC.Core.Op.ConstantFold:
--      eqString (unpackCString# (Lit s1)) (unpackCString# (Lit s2)) = s1==s2


----------------------------------------------
-- 'Int' related definitions
----------------------------------------------

maxInt, minInt :: Int

{- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}
#if WORD_SIZE_IN_BITS == 31
minInt  = I# (-0x40000000#)
maxInt  = I# 0x3FFFFFFF#
#elif WORD_SIZE_IN_BITS == 32
minInt  = I# (-0x80000000#)
maxInt  = I# 0x7FFFFFFF#
#else
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#
#endif

----------------------------------------------
-- The function type
----------------------------------------------

-- | Identity function.
--
-- > id x = x
id                      :: a -> a
id x                    =  x

-- Assertion function.  This simply ignores its boolean argument.
-- The compiler may rewrite it to @('assertError' line)@.

-- | If the first argument evaluates to 'True', then the result is the
-- second argument.  Otherwise an 'Control.Exception.AssertionFailed' exception
-- is raised, containing a 'String' with the source file and line number of the
-- call to 'assert'.
--
-- Assertions can normally be turned on or off with a compiler flag
-- (for GHC, assertions are normally on unless optimisation is turned on
-- with @-O@ or the @-fignore-asserts@
-- option is given).  When assertions are turned off, the first
-- argument to 'assert' is ignored, and the second argument is
-- returned as the result.

--      SLPJ: in 5.04 etc 'assert' is in GHC.Prim,
--      but from Template Haskell onwards it's simply
--      defined here in Base.hs
assert :: Bool -> a -> a
assert _pred r = r

breakpoint :: a -> a
breakpoint r = r

breakpointCond :: Bool -> a -> a
breakpointCond _ r = r

data Opaque = forall a. O a
-- | @const x@ is a unary function which evaluates to @x@ for all inputs.
--
-- >>> const 42 "hello"
-- 42
--
-- >>> map (const 42) [0..3]
-- [42,42,42,42]
const                   :: a -> b -> a
const x _               =  x

-- | Function composition.
{-# INLINE (.) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
--
-- >>> flip (++) "hello" "world"
-- "worldhello"
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

-- | Application operator.  This operator is redundant, since ordinary
-- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- low, right-associative binding precedence, so it sometimes allows
-- parentheses to be omitted; for example:
--
-- > f $ g $ h x  =  f (g (h x))
--
-- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- or @'Data.List.zipWith' ('$') fs xs@.
--
-- Note that @('$')@ is levity-polymorphic in its result type, so that
-- @foo '$' True@ where @foo :: Bool -> Int#@ is well-typed.
{-# INLINE ($) #-}
($) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $ x =  f x

-- | Strict (call-by-value) application operator. It takes a function and an
-- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- the function with that value.

($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $! x = let !vx = x in f vx  -- see #2273

-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
until                   :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go
  where
    go x | p x          = x
         | otherwise    = go (f x)

-- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- used as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf                :: a -> a -> a
asTypeOf                =  const

----------------------------------------------
-- Functor/Applicative/Monad instances for IO
----------------------------------------------

-- | @since 2.01
instance  Functor IO where
   fmap f x = x >>= (pure . f)

-- | @since 2.01
instance Applicative IO where
    {-# INLINE pure #-}
    {-# INLINE (*>) #-}
    {-# INLINE liftA2 #-}
    pure  = returnIO
    (*>)  = thenIO
    (<*>) = ap
    liftA2 = liftM2

-- | @since 2.01
instance  Monad IO  where
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    (>>)      = (*>)
    (>>=)     = bindIO

-- | @since 4.9.0.0
instance Alternative IO where
    empty = failIO "mzero"
    (<|>) = mplusIO

-- | @since 4.9.0.0
instance MonadPlus IO

returnIO :: a -> IO a
returnIO x = IO (\ s -> (# s, x #))

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

thenIO :: IO a -> IO b -> IO b
thenIO (IO m) k = IO (\ s -> case m s of (# new_s, _ #) -> unIO k new_s)

-- Note that it is import that we do not SOURCE import this as
-- its demand signature encodes knowledge of its bottoming
-- behavior, which can expose useful simplifications. See
-- #16588.
failIO :: String -> IO a
failIO s = IO (raiseIO# (mkUserError s))

unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
unIO (IO a) = a

{- |
Returns the tag of a constructor application; this function is used
by the deriving code for Eq, Ord and Enum.
-}
{-# INLINE getTag #-}
getTag :: a -> Int#
getTag x = dataToTag# x

----------------------------------------------
-- Numeric primops
----------------------------------------------

-- Definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

{-# INLINE quotInt #-}
{-# INLINE remInt #-}

quotInt, remInt, divInt, modInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)

quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)

divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)

divModInt# :: Int# -> Int# -> (# Int#, Int# #)
x# `divModInt#` y#
 | isTrue# (x# ># 0#) && isTrue# (y# <# 0#) =
                                    case (x# -# 1#) `quotRemInt#` y# of
                                      (# q, r #) -> (# q -# 1#, r +# y# +# 1# #)
 | isTrue# (x# <# 0#) && isTrue# (y# ># 0#) =
                                    case (x# +# 1#) `quotRemInt#` y# of
                                      (# q, r #) -> (# q -# 1#, r +# y# -# 1# #)
 | otherwise                                =
                                    x# `quotRemInt#` y#

-- Wrappers for the shift operations.  The uncheckedShift# family are
-- undefined when the amount being shifted by is greater than the size
-- in bits of Int#, so these wrappers perform a check and return
-- either zero or -1 appropriately.
--
-- Note that these wrappers still produce undefined results when the
-- second argument (the shift amount) is negative.

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
shiftL# :: Word# -> Int# -> Word#
a `shiftL#` b   | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0##
                | otherwise                          = a `uncheckedShiftL#` b

-- | Shift the argument right by the specified number of bits
-- (which must be non-negative).
-- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- (although an arithmetic right shift wouldn't make sense for Word#)
shiftRL# :: Word# -> Int# -> Word#
a `shiftRL#` b  | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0##
                | otherwise                          = a `uncheckedShiftRL#` b

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
iShiftL# :: Int# -> Int# -> Int#
a `iShiftL#` b  | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0#
                | otherwise                          = a `uncheckedIShiftL#` b

-- | Shift the argument right (signed) by the specified number of bits
-- (which must be non-negative).
-- The "RA" means "right, arithmetic" (as opposed to RL for logical)
iShiftRA# :: Int# -> Int# -> Int#
a `iShiftRA#` b | isTrue# (b >=# WORD_SIZE_IN_BITS#) = if isTrue# (a <# 0#)
                                                          then (-1#)
                                                          else 0#
                | otherwise                          = a `uncheckedIShiftRA#` b

-- | Shift the argument right (unsigned) by the specified number of bits
-- (which must be non-negative).
-- The "RL" means "right, logical" (as opposed to RA for arithmetic)
iShiftRL# :: Int# -> Int# -> Int#
a `iShiftRL#` b | isTrue# (b >=# WORD_SIZE_IN_BITS#) = 0#
                | otherwise                          = a `uncheckedIShiftRL#` b

------------------------------------------------------------------------
-- Rules for C strings (the functions themselves are now in GHC.CString)
------------------------------------------------------------------------

-- Ascii variants
{-# RULES
"unpack"       [~1] forall a   . unpackCString# a             = build (unpackFoldrCString# a)
"unpack-list"  [1]  forall a   . unpackFoldrCString# a (:) [] = unpackCString# a
"unpack-append"     forall a n . unpackFoldrCString# a (:) n  = unpackAppendCString# a n

-- There's a built-in rule (in GHC.Core.Op.ConstantFold) for
--      unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n

  #-}

-- Utf8 variants
{-# RULES
"unpackUtf8"       [~1] forall a   . unpackCStringUtf8# a             = build (unpackFoldrCStringUtf8# a)
"unpack-listUtf8"  [1]  forall a   . unpackFoldrCStringUtf8# a (:) [] = unpackCStringUtf8# a
"unpack-appendUtf8"     forall a n . unpackFoldrCStringUtf8# a (:) n  = unpackAppendCStringUtf8# a n

-- There's a built-in rule (in PrelRules.hs) for
--      unpackFoldrUtf8 "föö" c (unpackFoldrUtf8 "bäz" c n)  =  unpackFoldrUtf8 "fööbäz" c n

  #-}
