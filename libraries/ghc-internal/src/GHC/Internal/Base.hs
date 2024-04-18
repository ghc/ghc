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

GHC.Internal.Base        Classes: Eq, Ord, Functor, Monad
                Types:   List, (), Int, Bool, Ordering, Char, String

GHC.Internal.Data.Tuple      Types: tuples, plus instances for GHC.Internal.Base classes

GHC.Internal.Show        Class: Show, plus instances for GHC.Base/GHC.Tup types

GHC.Internal.Enum        Class: Enum,  plus instances for GHC.Base/GHC.Tup types

GHC.Internal.Data.Maybe      Type: Maybe, plus instances for GHC.Internal.Base classes

GHC.Internal.List        List functions

GHC.Internal.Num         Class: Num, plus instances for Int
                Type:  Integer, plus instances for all classes so far (Eq, Ord, Num, Show)

                Integer is needed here because it is mentioned in the signature
                of 'fromInteger' in class Num

GHC.Internal.Real        Classes: Real, Integral, Fractional, RealFrac
                         plus instances for Int, Integer
                Types:  Ratio, Rational
                        plus instances for classes so far

                Rational is needed here because it is mentioned in the signature
                of 'toRational' in class Real

GHC.Internal.ST  The ST monad, instances and a few helper functions

Ix              Classes: Ix, plus instances for Int, Bool, Char, Integer, Ordering, tuples

GHC.Internal.Arr         Types: Array, MutableArray, MutableVar

                Arrays are used by a function in GHC.Internal.Float

GHC.Internal.Float       Classes: Floating, RealFloat
                Types:   Float, Double, plus instances of all classes so far

                This module contains everything to do with floating point.
                It is a big module (900 lines)
                With a bit of luck, many modules can be compiled without ever reading GHC.Internal.Float.hi


Other Prelude modules are much easier with fewer complex dependencies.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE Unsafe #-}

-- -Wno-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Base
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic data types and classes.
--
-----------------------------------------------------------------------------

#include "MachDeps.h"

module GHC.Internal.Base
        (
        module GHC.Internal.Base,
        module GHC.Classes,
        module GHC.CString,
        module GHC.Magic,
        module GHC.Magic.Dict,
        module GHC.Types,
        module GHC.Prim,         -- Re-export GHC.Prim, GHC.Prim.Ext,
        module GHC.Prim.Ext,     -- GHC.Prim.PtrEq and [boot] GHC.Internal.Err
        module GHC.Prim.PtrEq,   -- to avoid lots of people having to
        module GHC.Internal.Err, -- import these modules explicitly
        module GHC.Internal.Maybe
  )
        where

import GHC.Types hiding (
  Unit#,
  Solo#,
  Tuple0#,
  Tuple1#,
  Tuple2#,
  Tuple3#,
  Tuple4#,
  Tuple5#,
  Tuple6#,
  Tuple7#,
  Tuple8#,
  Tuple9#,
  Tuple10#,
  Tuple11#,
  Tuple12#,
  Tuple13#,
  Tuple14#,
  Tuple15#,
  Tuple16#,
  Tuple17#,
  Tuple18#,
  Tuple19#,
  Tuple20#,
  Tuple21#,
  Tuple22#,
  Tuple23#,
  Tuple24#,
  Tuple25#,
  Tuple26#,
  Tuple27#,
  Tuple28#,
  Tuple29#,
  Tuple30#,
  Tuple31#,
  Tuple32#,
  Tuple33#,
  Tuple34#,
  Tuple35#,
  Tuple36#,
  Tuple37#,
  Tuple38#,
  Tuple39#,
  Tuple40#,
  Tuple41#,
  Tuple42#,
  Tuple43#,
  Tuple44#,
  Tuple45#,
  Tuple46#,
  Tuple47#,
  Tuple48#,
  Tuple49#,
  Tuple50#,
  Tuple51#,
  Tuple52#,
  Tuple53#,
  Tuple54#,
  Tuple55#,
  Tuple56#,
  Tuple57#,
  Tuple58#,
  Tuple59#,
  Tuple60#,
  Tuple61#,
  Tuple62#,
  Tuple63#,
  Tuple64#,
  Sum2#,
  Sum3#,
  Sum4#,
  Sum5#,
  Sum6#,
  Sum7#,
  Sum8#,
  Sum9#,
  Sum10#,
  Sum11#,
  Sum12#,
  Sum13#,
  Sum14#,
  Sum15#,
  Sum16#,
  Sum17#,
  Sum18#,
  Sum19#,
  Sum20#,
  Sum21#,
  Sum22#,
  Sum23#,
  Sum24#,
  Sum25#,
  Sum26#,
  Sum27#,
  Sum28#,
  Sum29#,
  Sum30#,
  Sum31#,
  Sum32#,
  Sum33#,
  Sum34#,
  Sum35#,
  Sum36#,
  Sum37#,
  Sum38#,
  Sum39#,
  Sum40#,
  Sum41#,
  Sum42#,
  Sum43#,
  Sum44#,
  Sum45#,
  Sum46#,
  Sum47#,
  Sum48#,
  Sum49#,
  Sum50#,
  Sum51#,
  Sum52#,
  Sum53#,
  Sum54#,
  Sum55#,
  Sum56#,
  Sum57#,
  Sum58#,
  Sum59#,
  Sum60#,
  Sum61#,
  Sum62#,
  Sum63#,
  )
import GHC.Classes hiding (
  CUnit,
  CSolo,
  CTuple0,
  CTuple1,
  CTuple2,
  CTuple3,
  CTuple4,
  CTuple5,
  CTuple6,
  CTuple7,
  CTuple8,
  CTuple9,
  CTuple10,
  CTuple11,
  CTuple12,
  CTuple13,
  CTuple14,
  CTuple15,
  CTuple16,
  CTuple17,
  CTuple18,
  CTuple19,
  CTuple20,
  CTuple21,
  CTuple22,
  CTuple23,
  CTuple24,
  CTuple25,
  CTuple26,
  CTuple27,
  CTuple28,
  CTuple29,
  CTuple30,
  CTuple31,
  CTuple32,
  CTuple33,
  CTuple34,
  CTuple35,
  CTuple36,
  CTuple37,
  CTuple38,
  CTuple39,
  CTuple40,
  CTuple41,
  CTuple42,
  CTuple43,
  CTuple44,
  CTuple45,
  CTuple46,
  CTuple47,
  CTuple48,
  CTuple49,
  CTuple50,
  CTuple51,
  CTuple52,
  CTuple53,
  CTuple54,
  CTuple55,
  CTuple56,
  CTuple57,
  CTuple58,
  CTuple59,
  CTuple60,
  CTuple61,
  CTuple62,
  CTuple63,
  CTuple64,
  )
import GHC.CString
import GHC.Magic
import GHC.Magic.Dict
import GHC.Prim hiding (dataToTagSmall#, dataToTagLarge#, whereFrom#)
  -- Hide dataToTag# ops because they are expected to break for
  -- GHC-internal reasons in the near future, and shouldn't
  -- be exposed from base (not even GHC.Exts)

import GHC.Prim.Ext
import GHC.Prim.PtrEq
import GHC.Internal.Err
import GHC.Internal.Maybe
import {-# SOURCE #-} GHC.Internal.IO (mkUserError, mplusIO)

import GHC.Tuple (Solo (MkSolo))

-- See Note [Semigroup stimes cycle]
import {-# SOURCE #-} GHC.Internal.Num (Num (..))
import {-# SOURCE #-} GHC.Internal.Real (Integral (..))

-- $setup
-- >>> import GHC.Internal.Num

infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 1  =<<
infixr 0  $, $!

infixl 4 <*>, <*, *>, <**>

default ()              -- Double isn't available yet

{-
Note [Tracking dependencies on primitives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When desugaring certain program constructs, GHC will insert references
to the primitives used to implement those constructs even if those
primitives have not actually been imported.  For example, the function
``fst (x,_) = x`` will be desugared using references to the 2-tuple
data constructor, which lives in GHC.Tuple.

When bootstrapping GHC, it is important that we do not attempt to
compile any such reference to GHC.Tuple before GHC.Tuple itself has
been built, otherwise compilation will fail with an error like this one:
    Failed to load interface for ‘GHC.Tuple’.
    There are files missing in the ‘ghc-prim-0.10.0’ package,
    try running 'ghc-pkg check'.
    Use -v to see a list of the files searched for.

To prevent such errors, we insist that if any boot library module X
implicitly depends on primitives in module Y, then the transitive
imports of X must include Y.

Such implicit dependencies can be introduced in at least the following ways:

W1:
  Common awkward dependencies:
   * TypeRep metadata introduces references to GHC.Types in EVERY module.
   * A String literal introduces a reference to GHC.CString, for either
     unpackCString# or unpackCStringUtf8# depending on its contents.
   * Tuple-notation introduces references to GHC.Tuple.
   * Constraint tuples introduce references to GHC.Classes.
   * Short lists like [3,8,2] produce references to GHC.Internal.Base.build

  A module can transitively depend on all of these by importing any of
  GHC.Internal.Base, GHC.Base, or Prelude.  The latter in particular
  means that an explicit import for this reason is only necessary when
  ImplicitPrelude is disabled, so this primarily comes up in the
  dependencies of base and in the compiler itself.

   * Most modules in ghc-internal import GHC.Internal.Base.
   * Most modules in compiler/ import GHC.Prelude, which imports Prelude.
   * Most hs-boot files that would otherwise have no imports can get
     away with just importing GHC.Types.

  Unfortunately, the requirement to transitively import these modules
  when they are implicitly used is obscure and causes only /intermittent/
  build failures, so enforcement of that requirement has historically
  been pretty spotty, causing issues like #23942.

  Improving this situation is discussed at #24520.

W2:
  Non-exhaustive pattern matches, incomplete record selectors,
  missing record fields, and missing class instance methods all
  introduce references to GHC.Internal.Control.Exception.Base.

  These constructs are therefore not allowed in ghc-prim or ghc-bignum.
  But since they generally have bad code smell and are avoided by
  developers anyway, this restriction has not been very burdensome.

W3:
  Various "overloaded" bits of syntax:
   * Overloaded integer literals introduce references to GHC.Internal.Num.
     * Likewise overloaded fractional literals to GHC.Internal.Real
     * Likewise overloaded string literals to GHC.Internal.Data.String
     * Likewise overloaded list literals to GHC.Internal.IsList
   * Overloaded labels introduce references to GHC.Internal.OverloadedLabels
   * Uses of OverloadedRecordDot introduce references to GHC.Internal.Records.
   * Do-notation introduces references to GHC.Internal.Base for Monad stuff.
     * Likewise arrow-notation to GHC.Internal.Control.Arrow
     * Likewise RecursiveDo stuff to GHC.Internal.Control.Monad.Fix
   * (Does TemplateHaskellQuotes fall into this category as well?)

  These are not problematic in practice.  For example, a program
  that uses arrow-notation but does not otherwise import the Arrow
  type class will almost certainly fail to type-check anyway.
  (The "Arrow m" constraint will be very hard to solve!)

W4:
  Stock derived instances introduce references to various things.
  Derived Eq instances can reference GHC.Magic.dataToTag#, for example.
  But since any module containing a derived Eq instance must import Eq,
  as long as the module which defines Eq imports GHC.Magic this cannot
  cause trouble.

  Things are a bit more complex for the Lift class (see #22229).
  * Derived Lift instances refer to machinery in
  Language.Haskell.TH.Lib.Internal, which is not imported by the module
  Language.Haskell.TH.Lib.Syntax that defines the Lift class.
  * Language.Haskell.TH.Lib.Internal imports Language.Haskell.TH.Lib.Syntax, so
  we can't add the reverse dependency without using a .hs-boot file
  * What we do instead is that we expose a module Language.Haskell.TH.Syntax
  importing both Language.Haskell.TH.Lib.{Syntax,Internal). Users are expected
  to import this module.

W5:
  If no explicit "default" declaration is present, the assumed
  "default (Integer, Double)" creates a dependency on GHC.Num.Integer
  for the Integer type if defaulting is ever attempted during
  type-checking.  (This doesn't apply to hs-boot files, which can't
  be given "default" declarations anyway.)

W6:
  In the wasm backend, JSFFI imports and exports pull in a bunch of stuff;
  see Note [Desugaring JSFFI static export] and Note [Desugaring JSFFI import]
  in GHC.HsToCore.Foreign.Wasm.

A complete list could probably be made by going through the known-key
names in GHC.Builtin.Names and GHC.Builtin.Names.TH.  To test whether
the transitive imports are sufficient for any single module, instruct
the build system to build /only/ that module in stage 2.  For example,
a command to check whether the transitive imports for GHC.Internal.Maybe
are sufficient is:

  hadrian/build -o_checkDeps _checkDeps/stage1/libraries/ghc-internal/build/GHC/Internal/Maybe.o

Use the ".o-boot" suffix instead of ".o" to check an hs-boot file's
transitive imports.


Note [Semigroup stimes cycle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Semigroup is defined in this module, GHC.Base, with the method
stimes :: (Semigroup a, Integral b) => b -> a -> a

This presents a problem.
* We use Integral methods (quot, rem) and Num methods (-) in stimes definitions
  in this module. Num is a superclass of Integral.
* Num is defined in GHC.Internal.Num, which imports GHC.Internal.Base.
* Enum is defined in GHC.Internal.Enum, which imports
  GHC.Internal.Base and GHC.Internal.Num. Enum is a superclass of
  Integral. We don't use any Enum methods here, but it is relevant
  (read on).
* Integral is defined in GHC.Internal.Real, which imports
  GHC.Internal.Base, GHC.Internal.Num, and GHC.Internal.Enum.

We resolve this web of dependencies with hs-boot files. The rules
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/separate_compilation.html#how-to-compile-mutually-recursive-modules
require us to put either the full declarations or only the instance head for
classes in a hs-boot file.
So we put the full class decls for Num and Integral in Num.hs-boot and
Real.hs-boot respectively. This also forces us to have an Enum.hs-boot.

An obvious alternative is to move the class decls for Num, Enum, Real, and
Integral here. We don't do that because we would then need to move all the
instances (for Int, Word, Integer, etc.) here as well, or leave those instances
as orphans, which is generally bad.

We previously resolved this problem in a different way, with an hs-boot for
Semigroup.Internal that provided stimes implementations. This made them
impossible to inline or specialize when used in this module. We no longer have
that problem because we only import classes and not implementations.
-}

#if 0
-- for use when compiling GHC.Internal.Base itself doesn't work
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

-- | Uninhabited data type
--
-- @since base-4.8.0.0
data Void deriving
  ( Eq      -- ^ @since base-4.8.0.0
  , Ord     -- ^ @since base-4.8.0.0
  )

-- | Since 'Void' values logically don't exist, this witnesses the
-- logical reasoning tool of \"ex falso quodlibet\".
--
-- >>> let x :: Either Void Int; x = Right 5
-- >>> :{
-- case x of
--     Right r -> r
--     Left l  -> absurd l
-- :}
-- 5
--
-- @since base-4.8.0.0
absurd :: Void -> a
absurd a = case a of {}

-- | If 'Void' is uninhabited then any 'Functor' that holds only
-- values of type 'Void' is holding no values.
-- It is implemented in terms of @fmap absurd@.
--
-- @since base-4.8.0.0
vacuous :: Functor f => f Void -> f a
vacuous = fmap absurd

infixr 6 <>

-- | The class of semigroups (types with an associative binary operation).
--
-- Instances should satisfy the following:
--
-- [Associativity] @x '<>' (y '<>' z) = (x '<>' y) '<>' z@
--
-- You can alternatively define `sconcat` instead of (`<>`), in which case the
-- laws are:
--
-- [Unit]: @'sconcat' ('pure' x) = x@
-- [Multiplication]: @'sconcat' ('join' xss) = 'sconcat' ('fmap' 'sconcat' xss)@
--
-- @since base-4.9.0.0
class Semigroup a where
        -- | An associative operation.
        --
        -- ==== __Examples__
        --
        -- >>> [1,2,3] <> [4,5,6]
        -- [1,2,3,4,5,6]
        --
        -- >>> Just [1, 2, 3] <> Just [4, 5, 6]
        -- Just [1,2,3,4,5,6]
        --
        -- >>> putStr "Hello, " <> putStrLn "World!"
        -- Hello, World!
        (<>) :: a -> a -> a
        a <> b = sconcat (a :| [ b ])

        -- | Reduce a non-empty list with '<>'
        --
        -- The default definition should be sufficient, but this can be
        -- overridden for efficiency.
        --
        -- ==== __Examples__
        --
        -- For the following examples, we will assume that we have:
        --
        -- >>> import Data.List.NonEmpty (NonEmpty (..))
        --
        -- >>> sconcat $ "Hello" :| [" ", "Haskell", "!"]
        -- "Hello Haskell!"
        --
        -- >>> sconcat $ Just [1, 2, 3] :| [Nothing, Just [4, 5, 6]]
        -- Just [1,2,3,4,5,6]
        --
        -- >>> sconcat $ Left 1 :| [Right 2, Left 3, Right 4]
        -- Right 2
        sconcat :: NonEmpty a -> a
        sconcat (a :| as) = go a as where
          go b (c:cs) = b <> go c cs
          go b []     = b

        -- | Repeat a value @n@ times.
        --
        -- The default definition will raise an exception for a multiplier that is @<= 0@.
        -- This may be overridden with an implementation that is total. For monoids
        -- it is preferred to use 'stimesMonoid'.
        --
        -- By making this a member of the class, idempotent semigroups
        -- and monoids can upgrade this to execute in \(\mathcal{O}(1)\) by
        -- picking @stimes = 'Data.Semigroup.stimesIdempotent'@ or @stimes =
        -- 'Data.Semigroup.stimesIdempotentMonoid'@ respectively.
        --
        -- ==== __Examples__
        --
        -- >>> stimes 4 [1]
        -- [1,1,1,1]
        --
        -- >>> stimes 5 (putStr "hi!")
        -- hi!hi!hi!hi!hi!
        --
        -- >>> stimes 3 (Right ":)")
        -- Right ":)"
        stimes :: Integral b => b -> a -> a
        stimes y0 x0
          | y0 <= 0   = errorWithoutStackTrace "stimes: positive multiplier expected"
          | otherwise = f x0 y0
          where
            f x y
              | y `rem` 2 == 0 = f (x <> x) (y `quot` 2)
              | y == 1 = x
              | otherwise = g (x <> x) (y `quot` 2) x        -- See Note [Half of y - 1]
            g x y z
              | y `rem` 2 == 0 = g (x <> x) (y `quot` 2) z
              | y == 1 = x <> z
              | otherwise = g (x <> x) (y `quot` 2) (x <> z) -- See Note [Half of y - 1]

        {-# MINIMAL (<>) | sconcat #-}

{- Note [Half of y - 1]
   ~~~~~~~~~~~~~~~~~~~~~
   Since y is guaranteed to be odd and positive here,
   half of y - 1 can be computed as y `quot` 2, optimising subtraction away.
-}

-- | The class of monoids (types with an associative binary operation that
-- has an identity).  Instances should satisfy the following:
--
-- [Right identity] @x '<>' 'mempty' = x@
-- [Left identity]  @'mempty' '<>' x = x@
-- [Associativity]  @x '<>' (y '<>' z) = (x '<>' y) '<>' z@ ('Semigroup' law)
-- [Concatenation]  @'mconcat' = 'foldr' ('<>') 'mempty'@
--
-- You can alternatively define `mconcat` instead of `mempty`, in which case the
-- laws are:
--
-- [Unit]: @'mconcat' ('pure' x) = x@
-- [Multiplication]: @'mconcat' ('join' xss) = 'mconcat' ('fmap' 'mconcat' xss)@
-- [Subclass]: @'mconcat' ('toList' xs) = 'sconcat' xs@
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
        -- ==== __Examples__
        -- >>> "Hello world" <> mempty
        -- "Hello world"
        --
        -- >>> mempty <> [1, 2, 3]
        -- [1,2,3]
        mempty :: a
        mempty = mconcat []
        {-# INLINE mempty #-}

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
        {-# INLINE mconcat #-}
        -- INLINE in the hope of fusion with mconcat's argument (see !4890)

        {-# MINIMAL mempty | mconcat #-}

-- | @since base-4.9.0.0
instance Semigroup [a] where
        (<>) = (++)
        {-# INLINE (<>) #-}

        stimes n x
          | n < 0 = errorWithoutStackTrace "stimes: [], negative multiplier"
          | otherwise = rep n
          where
            rep 0 = []
            rep i = x ++ rep (i - 1)

-- | @since base-2.01
instance Monoid [a] where
        {-# INLINE mempty #-}
        mempty  = []
        {-# INLINE mconcat #-}
        mconcat xss = [x | xs <- xss, x <- xs]
-- See Note: [List comprehensions and inlining]

-- | @since base-4.9.0.0
instance Semigroup Void where
    a <> _ = a
    stimes _ a = a

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
benefit, as described in compiler/GHC/HsToCore/ListComp.hs: if optimizations
needed to make foldr/build forms efficient are turned off, we'll get reasonably
efficient translations anyway.
-}

-- | @since base-4.9.0.0
instance Semigroup (NonEmpty a) where
        (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

-- | @since base-4.9.0.0
instance Semigroup b => Semigroup (a -> b) where
        f <> g = \x -> f x <> g x
        stimes n f e = stimes n (f e)

-- | @since base-2.01
instance Monoid b => Monoid (a -> b) where
        mempty _ = mempty
        -- If `b` has a specialised mconcat, use that, rather than the default
        -- mconcat, which can be much less efficient.  Inline in the hope that
        -- it may result in list fusion.
        mconcat = \fs x -> mconcat $ map (\f -> f x) fs
        {-# INLINE mconcat #-}

-- | @since base-4.9.0.0
instance Semigroup () where
        _ <> _      = ()
        sconcat _   = ()
        stimes  _ _ = ()

-- | @since base-2.01
instance Monoid () where
        -- Should it be strict?
        mempty        = ()
        mconcat _     = ()

-- | @since base-4.15
instance Semigroup a => Semigroup (Solo a) where
  MkSolo a <> MkSolo b = MkSolo (a <> b)
  stimes n (MkSolo a) = MkSolo (stimes n a)

-- | @since base-4.15
instance Monoid a => Monoid (Solo a) where
  mempty = MkSolo mempty

-- | @since base-4.9.0.0
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
        (a,b) <> (a',b') = (a<>a',b<>b')
        stimes n (a,b) = (stimes n a, stimes n b)

-- | @since base-2.01
instance (Monoid a, Monoid b) => Monoid (a,b) where
        mempty = (mempty, mempty)

-- | @since base-4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
        (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
        stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

-- | @since base-2.01
instance (Monoid a, Monoid b, Monoid c) => Monoid (a,b,c) where
        mempty = (mempty, mempty, mempty)

-- | @since base-4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d)
         => Semigroup (a, b, c, d) where
        (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
        stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

-- | @since base-2.01
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (a,b,c,d) where
        mempty = (mempty, mempty, mempty, mempty)

-- | @since base-4.9.0.0
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e)
         => Semigroup (a, b, c, d, e) where
        (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
        stimes n (a,b,c,d,e) =
            (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

-- | @since base-2.01
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
                Monoid (a,b,c,d,e) where
        mempty = (mempty, mempty, mempty, mempty, mempty)


-- | @since base-4.9.0.0
instance Semigroup Ordering where
    LT <> _ = LT
    EQ <> y = y
    GT <> _ = GT

    stimes n x = case compare n 0 of
      LT -> errorWithoutStackTrace "stimes: Ordering, negative multiplier"
      EQ -> EQ
      GT -> x

-- lexicographical ordering
-- | @since base-2.01
instance Monoid Ordering where
    mempty             = EQ

-- | @since base-4.9.0.0
instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> b       = b
    a       <> Nothing = a
    Just a  <> Just b  = Just (a <> b)

    stimes _ Nothing = Nothing
    stimes n (Just a) = case compare n 0 of
      LT -> errorWithoutStackTrace "stimes: Maybe, negative multiplier"
      EQ -> Nothing
      GT -> Just (stimes n a)

-- | Lift a semigroup into 'Maybe' forming a 'Monoid' according to
-- <http://en.wikipedia.org/wiki/Monoid>: \"Any semigroup @S@ may be
-- turned into a monoid simply by adjoining an element @e@ not in @S@
-- and defining @e*e = e@ and @e*s = s = s*e@ for all @s ∈ S@.\"
--
-- /Since 4.11.0/: constraint on inner @a@ value generalised from
-- 'Monoid' to 'Semigroup'.
--
-- @since base-2.01
instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

-- | @since base-4.15
instance Applicative Solo where
  pure = MkSolo

  -- Note: we really want to match strictly here. This lets us write,
  -- for example,
  --
  -- forceSpine :: Foldable f => f a -> ()
  -- forceSpine xs
  --   | MkSolo r <- traverse_ MkSolo xs
  --   = r
  MkSolo f <*> MkSolo x = MkSolo (f x)
  liftA2 f (MkSolo x) (MkSolo y) = MkSolo (f x y)

-- | For tuples, the 'Monoid' constraint on @a@ determines
-- how the first values merge.
-- For example, 'String's concatenate:
--
-- > ("hello ", (+15)) <*> ("world!", 2002)
-- > ("hello world!",2017)
--
-- @since base-2.01
instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u <> v, f x)
    liftA2 f (u, x) (v, y) = (u <> v, f x y)

-- | @since base-4.15
instance Monad Solo where
  MkSolo x >>= f = f x

-- | @since base-4.9.0.0
instance Monoid a => Monad ((,) a) where
    (u, a) >>= k = case k a of (v, b) -> (u <> v, b)

-- | @since base-4.14.0.0
instance Functor ((,,) a b) where
    fmap f (a, b, c) = (a, b, f c)

-- | @since base-4.14.0.0
instance (Monoid a, Monoid b) => Applicative ((,,) a b) where
    pure x = (mempty, mempty, x)
    (a, b, f) <*> (a', b', x) = (a <> a', b <> b', f x)

-- | @since base-4.14.0.0
instance (Monoid a, Monoid b) => Monad ((,,) a b) where
    (u, v, a) >>= k = case k a of (u', v', b) -> (u <> u', v <> v', b)

-- | @since base-4.14.0.0
instance Functor ((,,,) a b c) where
    fmap f (a, b, c, d) = (a, b, c, f d)

-- | @since base-4.14.0.0
instance (Monoid a, Monoid b, Monoid c) => Applicative ((,,,) a b c) where
    pure x = (mempty, mempty, mempty, x)
    (a, b, c, f) <*> (a', b', c', x) = (a <> a', b <> b', c <> c', f x)

-- | @since base-4.14.0.0
instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c) where
    (u, v, w, a) >>= k = case k a of (u', v', w', b) -> (u <> u', v <> v', w <> w', b)

-- | @since base-4.18.0.0
instance Functor ((,,,,) a b c d) where
    fmap f (a, b, c, d, e) = (a, b, c, d, f e)

-- | @since base-4.18.0.0
instance Functor ((,,,,,) a b c d e) where
    fmap fun (a, b, c, d, e, f) = (a, b, c, d, e, fun f)

-- | @since base-4.18.0.0
instance Functor ((,,,,,,) a b c d e f) where
    fmap fun (a, b, c, d, e, f, g) = (a, b, c, d, e, f, fun g)

-- | @since base-4.10.0.0
instance Semigroup a => Semigroup (IO a) where
    (<>) = liftA2 (<>)

-- | @since base-4.9.0.0
instance Monoid a => Monoid (IO a) where
    mempty = pure mempty

{- | A type @f@ is a Functor if it provides a function @fmap@ which, given any types @a@ and @b@
lets you apply any function from @(a -> b)@ to turn an @f a@ into an @f b@, preserving the
structure of @f@. Furthermore @f@ needs to adhere to the following:

[Identity]    @'fmap' 'id' == 'id'@
[Composition] @'fmap' (f . g) == 'fmap' f . 'fmap' g@

Note, that the second law follows from the free theorem of the type 'fmap' and
the first law, so you need only check that the former condition holds.
See these articles by <https://www.schoolofhaskell.com/user/edwardk/snippets/fmap School of Haskell> or
<https://github.com/quchen/articles/blob/master/second_functor_law.md David Luposchainsky>
for an explanation.
-}

class Functor f where
    -- | 'fmap' is used to apply a function of type @(a -> b)@ to a value of type @f a@,
    -- where f is a functor, to produce a value of type @f b@.
    -- Note that for any type constructor with more than one parameter (e.g., `Either`),
    -- only the last type parameter can be modified with `fmap` (e.g., `b` in `Either a b`).
    --
    -- Some type constructors with two parameters or more have a @'Data.Bifunctor'@ instance that allows
    -- both the last and the penultimate parameters to be mapped over.
    --
    -- ==== __Examples__
    --
    -- Convert from a @'Data.Maybe.Maybe' Int@ to a @Maybe String@
    -- using 'Prelude.show':
    --
    -- >>> fmap show Nothing
    -- Nothing
    -- >>> fmap show (Just 3)
    -- Just "3"
    --
    -- Convert from an @'Data.Either.Either' Int Int@ to an
    -- @Either Int String@ using 'Prelude.show':
    --
    -- >>> fmap show (Left 17)
    -- Left 17
    -- >>> fmap show (Right 17)
    -- Right "17"
    --
    -- Double each element of a list:
    --
    -- >>> fmap (*2) [1,2,3]
    -- [2,4,6]
    --
    -- Apply 'Prelude.even' to the second element of a pair:
    --
    -- >>> fmap even (2,2)
    -- (2,True)
    --
    -- It may seem surprising that the function is only applied to the last element of the tuple
    -- compared to the list example above which applies it to every element in the list.
    -- To understand, remember that tuples are type constructors with multiple type parameters:
    -- a tuple of 3 elements @(a,b,c)@ can also be written @(,,) a b c@ and its @Functor@ instance
    -- is defined for @Functor ((,,) a b)@ (i.e., only the third parameter is free to be mapped over
    -- with @fmap@).
    --
    -- It explains why @fmap@ can be used with tuples containing values of different types as in the
    -- following example:
    --
    -- >>> fmap even ("hello", 1.0, 4)
    -- ("hello",1.0,True)

    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    --
    -- ==== __Examples__
    --
    -- Perform a computation with 'Maybe' and replace the result with a
    -- constant value if it is 'Just':
    --
    -- >>> 'a' <$ Just 2
    -- Just 'a'
    -- >>> 'a' <$ Nothing
    -- Nothing
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
--   * @m1 '<*>' m2 = m1 '>>=' (\\x1 -> m2 '>>=' (\\x2 -> 'return' (x1 x2)))@
--
--   * @('*>') = ('>>')@
--
-- (which implies that 'pure' and '<*>' satisfy the applicative functor laws).

class Functor f => Applicative f where
    {-# MINIMAL pure, ((<*>) | liftA2) #-}
    -- | Lift a value into the Structure.
    --
    -- ==== __Examples__
    --
    -- >>> pure 1 :: Maybe Int
    -- Just 1
    --
    -- >>> pure 'z' :: [Char]
    -- "z"
    --
    -- >>> pure (pure ":D") :: Maybe [String]
    -- Just [":D"]
    pure :: a -> f a

    -- | Sequential application.
    --
    -- A few functors support an implementation of '<*>' that is more
    -- efficient than the default one.
    --
    -- ==== __Example__
    -- Used in combination with @'(Data.Functor.<$>)'@, @'(<*>)'@ can be used to build a record.
    --
    -- >>> data MyState = MyState {arg1 :: Foo, arg2 :: Bar, arg3 :: Baz}
    --
    -- >>> produceFoo :: Applicative f => f Foo
    -- >>> produceBar :: Applicative f => f Bar
    -- >>> produceBaz :: Applicative f => f Baz
    --
    -- >>> mkState :: Applicative f => f MyState
    -- >>> mkState = MyState <$> produceFoo <*> produceBar <*> produceBaz
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
    -- ==== __Example__
    --
    -- >>> liftA2 (,) (Just 3) (Just 5)
    -- Just (3,5)
    --
    -- >>> liftA2 (+) [1, 2, 3] [4, 5, 6]
    -- [5,6,7,6,7,8,7,8,9]
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    -- | Sequence actions, discarding the value of the first argument.
    --
    -- ==== __Examples__
    -- If used in conjunction with the Applicative instance for 'Maybe',
    -- you can chain Maybe computations, with a possible "early return"
    -- in case of 'Nothing'.
    --
    -- >>> Just 2 *> Just 3
    -- Just 3
    --
    -- >>> Nothing *> Just 3
    -- Nothing
    --
    -- Of course a more interesting use case would be to have effectful
    -- computations instead of just returning pure values.
    --
    -- >>> import Data.Char
    -- >>> import GHC.Internal.Text.ParserCombinators.ReadP
    -- >>> let p = string "my name is " *> munch1 isAlpha <* eof
    -- >>> readP_to_S p "my name is Simon"
    -- [("Simon","")]

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
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const

-- | A variant of '<*>' with the types of the arguments reversed. It differs from
-- @`flip` `(<*>)`@ in that the effects are resolved in the order the arguments are
-- presented.
--
-- ==== __Examples__
-- >>> (<**>) (print 1) (id <$ print 2)
-- 1
-- 2
--
-- >>> flip (<*>) (print 1) (id <$ print 2)
-- 2
-- 1
--
-- >>> ZipList [4, 5, 6] <**> ZipList [(+1), (*2), (/3)]
-- ZipList {getZipList = [5.0,10.0,2.0]}

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)

-- | Lift a function to actions.
-- Equivalent to Functor's `fmap` but implemented using only `Applicative`'s methods:
-- @'liftA' f a = 'pure' f '<*>' a@
--
-- As such this function may be used to implement a `Functor` instance from an `Applicative` one.
--
-- ==== __Examples__
-- Using the Applicative instance for Lists:
--
-- >>> liftA (+1) [1, 2]
-- [2,3]
--
-- Or the Applicative instance for 'Maybe'
--
-- >>> liftA (+1) (Just 3)
-- Just 4

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a
-- Caution: since this may be used for `fmap`, we can't use the obvious
-- definition of liftA = fmap.

-- | Lift a ternary function to actions.

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
-- >>> join [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
-- [1,2,3,4,5,6,7,8,9]
--
-- >>> join (Just (Just 3))
-- Just 3
--
-- A common use of 'join' is to run an 'IO' computation returned from
-- an 'GHC.Conc.STM' transaction, since 'GHC.Conc.STM' transactions
-- can't perform 'IO' directly. Recall that
--
-- @
-- 'GHC.Internal.Conc.atomically' :: STM a -> IO a
-- @
--
-- is used to run 'GHC.Conc.STM' transactions atomically. So, by
-- specializing the types of 'GHC.Internal.Conc.atomically' and 'join' to
--
-- @
-- 'GHC.Internal.Conc.atomically' :: STM (IO b) -> IO (IO b)
-- 'join'       :: IO (IO b)  -> IO b
-- @
--
-- we can compose them as
--
-- @
-- 'join' . 'GHC.Internal.Conc.atomically' :: STM (IO b) -> IO b
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
* @m1 '<*>' m2 = m1 '>>=' (\\x1 -> m2 '>>=' (\\x2 -> 'return' (x1 x2)))@

The above laws imply:

* @'fmap' f xs  =  xs '>>=' 'return' . f@
* @('>>') = ('*>')@

and that 'pure' and ('<*>') satisfy the applicative functor laws.

The instances of 'Monad' for 'GHC.List.List', 'Data.Maybe.Maybe' and 'System.IO.IO'
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
    --
    -- An alternative name for this function is \'bind\', but some people
    -- may refer to it as \'flatMap\', which results from it being equivialent
    -- to
    --
    -- @\\x f -> 'join' ('fmap' f x) :: Monad m => m a -> (a -> m b) -> m b@
    --
    -- which can be seen as mapping a value with
    -- @Monad m => m a -> m (m b)@ and then \'flattening\' @m (m b)@ to @m b@ using 'join'.
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
    --
    -- or in terms of @'(>>=)'@ as
    --
    -- > as >>= const bs
    (>>)        :: forall a b. m a -> m b -> m b
    m >> k = m >>= \_ -> k -- See Note [Recursive bindings for Applicative/Monad]
    {-# INLINE (>>) #-}

    -- | Inject a value into the monadic type.
    -- This function should /not/ be different from its default implementation
    -- as 'pure'. The justification for the existence of this function is
    -- merely historic.
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
--
-- > as >>= f == f =<< as
{-# SPECIALISE (=<<) :: (a -> [b]) -> [a] -> [b] #-}
(=<<)           :: Monad m => (a -> m b) -> m a -> m b
f =<< x         = x >>= f

-- | Conditional execution of 'Applicative' expressions. For example,
--
-- ==== __Examples__
--
-- > when debug (putStrLn "Debugging")
--
-- will output the string @Debugging@ if the Boolean value @debug@
-- is 'True', and otherwise do nothing.
--
-- >>> putStr "pi:" >> when False (print 3.14159)
-- pi:
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
-- This is equivalent to 'fmap' but specialised to Monads.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

-- | Promote a function to a monad, scanning the monadic arguments from
-- left to right.
--
-- ==== __Examples__
--
-- >>> liftM2 (+) [0,1] [0,2]
-- [0,2,1,3]
--
-- >>> liftM2 (+) (Just 1) Nothing
-- Nothing
--
-- >>> liftM2 (+) (+ 3) (* 2) 5
-- 18
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

> liftM<n> f x1 x2 ... xn

==== __Examples__

>>> pure (\x y z -> x + y * z) `ap` Just 1 `ap` Just 5 `ap` Just 10
Just 51
-}
ap                :: (Monad m) => m (a -> b) -> m a -> m b
ap m1 m2          = do { x1 <- m1; x2 <- m2; return (x1 x2) }
-- Since many Applicative instances define (<*>) = ap, we
-- cannot define ap = (<*>)
{-# INLINABLE ap #-}
{-# SPECIALISE ap :: IO (a -> b) -> IO a -> IO b #-}
{-# SPECIALISE ap :: Maybe (a -> b) -> Maybe a -> Maybe b #-}

-- instances for Prelude types

-- | @since base-2.01
instance Functor ((->) r) where
    fmap = (.)

-- | @since base-2.01
instance Applicative ((->) r) where
    pure = const
    (<*>) f g x = f x (g x)
    liftA2 q f g x = q (f x) (g x)

-- | @since base-2.01
instance Monad ((->) r) where
    f >>= k = \ r -> k (f r) r

-- | @since base-4.15
instance Functor Solo where
  fmap f (MkSolo a) = MkSolo (f a)

  -- Being strict in the `Solo` argument here seems most consistent
  -- with the concept behind `Solo`: always strict in the wrapper and lazy
  -- in the contents.
  x <$ MkSolo _ = MkSolo x

-- | @since base-2.01
instance Functor ((,) a) where
    fmap f (x,y) = (x, f y)

-- | @since base-2.01
instance  Functor Maybe  where
    fmap _ Nothing       = Nothing
    fmap f (Just a)      = Just (f a)

-- | @since base-2.01
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing

    liftA2 f (Just x) (Just y) = Just (f x y)
    liftA2 _ _ _ = Nothing

    Just _m1 *> m2      = m2
    Nothing  *> _m2     = Nothing

-- | @since base-2.01
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
--
-- ==== __Examples__
--
-- >>> Nothing <|> Just 42
-- Just 42
--
-- >>> [1, 2] <|> [3, 4]
-- [1,2,3,4]
--
-- >>> empty <|> print (2^15)
-- 32768
class Applicative f => Alternative f where
    -- | The identity of '<|>'
    --
    -- > empty <|> a     == a
    -- > a     <|> empty == a
    empty :: f a
    -- | An associative binary operation
    (<|>) :: f a -> f a -> f a

    -- | One or more.
    --
    -- ==== __Examples__
    --
    -- >>> some (putStr "la")
    -- lalalalalalalalala... * goes on forever *
    --
    -- >>> some Nothing
    -- nothing
    --
    -- >>> take 5 <$> some (Just 1)
    -- * hangs forever *
    --
    -- Note that this function can be used with Parsers based on
    -- Applicatives. In that case @some parser@ will attempt to
    -- parse @parser@ one or more times until it fails.
    some :: f a -> f [a]
    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v

    -- | Zero or more.
    --
    -- ==== __Examples__
    --
    -- >>> many (putStr "la")
    -- lalalalalalalalala... * goes on forever *
    --
    -- >>> many Nothing
    -- Just []
    --
    -- >>> take 5 <$> many (Just 1)
    -- * hangs forever *
    --
    -- Note that this function can be used with Parsers based on
    -- Applicatives. In that case @many parser@ will attempt to
    -- parse @parser@ zero or more times until it fails.
    many :: f a -> f [a]
    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = liftA2 (:) v many_v


-- | Picks the leftmost 'Just' value, or, alternatively, 'Nothing'.
--
-- @since base-2.01
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

-- | Picks the leftmost 'Just' value, or, alternatively, 'Nothing'.
--
-- @since base-2.01
instance MonadPlus Maybe

---------------------------------------------
-- The non-empty list type

infixr 5 :|

-- | Non-empty (and non-strict) list type.
--
-- @since base-4.9.0.0
data NonEmpty a = a :| [a]
  deriving ( Eq  -- ^ @since base-4.9.0.0
           , Ord -- ^ @since base-4.9.0.0
           )

-- | @since base-4.9.0.0
instance Functor NonEmpty where
  fmap f ~(a :| as) = f a :| fmap f as
  b <$ ~(_ :| as)   = b   :| (b <$ as)

-- | @since base-4.9.0.0
instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) = ap
  liftA2 = liftM2

-- | @since base-4.9.0.0
instance Monad NonEmpty where
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f
          toList ~(c :| cs) = c : cs

----------------------------------------------
-- The list type

-- | @since base-2.01
instance Functor [] where
    {-# INLINE fmap #-}
    fmap = map

-- See Note: [List comprehensions and inlining]
-- | @since base-2.01
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
-- | @since base-2.01
instance Monad []  where
    {-# INLINE (>>=) #-}
    xs >>= f             = [y | x <- xs, y <- f x]
    {-# INLINE (>>) #-}
    (>>) = (*>)

-- | Combines lists by concatenation, starting from the empty list.
--
-- @since base-2.01
instance Alternative [] where
    empty = []
    (<|>) = (++)

-- | Combines lists by concatenation, starting from the empty list.
--
-- @since base-2.01
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
-- bloated code when compiling
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
-- this means that @map id == id@
--
-- ==== __Examples__
--
-- >>> map (+1) [1, 2, 3]
-- [2,3,4]
--
-- >>> map id [1, 2, 3]
-- [1,2,3]
--
-- >>> map (\n -> 3 * n + 1) [1, 2, 3]
-- [4,7,10]
map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
  -- We want the RULEs "map" and "map/coerce" to fire first.
  -- map is recursive, so won't inline anyway,
  -- but saying so is more explicit, and silences warnings
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-} -- See Note [Inline FB functions] in GHC.Internal.List
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
  e.g. append, filter, iterate, repeat, etc. in GHC.Internal.List

  See also Note [Inline FB functions] in GHC.Internal.List

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
-- See Note [Getting the map/coerce RULE to work] in GHC.Core.SimpleOpt

----------------------------------------------
--              append
----------------------------------------------

-- | '(++)' appends two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
--
-- If the first list is not finite, the result is the first list.
--
-- ==== __Performance considerations__
--
-- This function takes linear time in the number of elements of the
-- __first__ list. Thus it is better to associate repeated
-- applications of '(++)' to the right (which is the default behaviour):
-- @xs ++ (ys ++ zs)@ or simply @xs ++ ys ++ zs@, but not @(xs ++ ys) ++ zs@.
-- For the same reason 'GHC.Internal.Data.List.concat' @=@ 'GHC.Internal.Data.List.foldr' '(++)' @[]@
-- has linear performance, while 'GHC.Internal.Data.List.foldl' '(++)' @[]@ is prone
-- to quadratic slowdown
--
-- ==== __Examples__
--
-- >>> [1, 2, 3] ++ [4, 5, 6]
-- [1,2,3,4,5,6]
--
-- >>> [] ++ [1, 2, 3]
-- [1,2,3]
--
-- >>> [3, 2, 1] ++ []
-- [3,2,1]
(++) :: [a] -> [a] -> [a]
{-# NOINLINE [2] (++) #-}
  -- Give time for the RULEs for (++) to fire in InitialPhase
  -- It's recursive, so won't inline anyway,
  -- but saying so is more explicit
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

{-# RULES
"++/literal"      forall x. (++) (unpackCString# x)     = unpackAppendCString# x
"++/literal_utf8" forall x. (++) (unpackCStringUtf8# x) = unpackAppendCStringUtf8# x #-}

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

-- | 'String' is an alias for a list of characters.
--
-- String constants in Haskell are values of type 'String'.
-- That means if you write a string literal like @"hello world"@,
-- it will have the type @[Char]@, which is the same as @String@.
--
-- __Note:__ You can ask the compiler to automatically infer different types
-- with the @-XOverloadedStrings@ language extension, for example
--  @"hello world" :: Text@. See t'Data.String.IsString' for more information.
--
-- Because @String@ is just a list of characters, you can use normal list functions
-- to do basic string manipulation. See "Data.List" for operations on lists.
--
-- === __Performance considerations__
--
-- @[Char]@ is a relatively memory-inefficient type.
-- It is a linked list of boxed word-size characters, internally it looks something like:
--
-- > ╭─────┬───┬──╮  ╭─────┬───┬──╮  ╭─────┬───┬──╮  ╭────╮
-- > │ (:) │   │ ─┼─>│ (:) │   │ ─┼─>│ (:) │   │ ─┼─>│ [] │
-- > ╰─────┴─┼─┴──╯  ╰─────┴─┼─┴──╯  ╰─────┴─┼─┴──╯  ╰────╯
-- >         v               v               v
-- >        'a'             'b'             'c'
--
-- The @String@ "abc" will use @5*3+1 = 16@ (in general @5n+1@)
-- words of space in memory.
--
-- Furthermore, operations like '(++)' (string concatenation) are @O(n)@
-- (in the left argument).
--
-- For historical reasons, the @base@ library uses @String@ in a lot of places
-- for the conceptual simplicity, but library code dealing with user-data
-- should use the [text](https://hackage.haskell.org/package/text)
-- package for Unicode text, or the the
-- [bytestring](https://hackage.haskell.org/package/bytestring) package
-- for binary data.
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
-- eqString also has a BuiltInRule in GHC.Core.Opt.ConstantFold:
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
--
-- This function might seem useless at first glance, but it can be very useful
-- in a higher order context.
--
-- ==== __Examples__
--
-- >>> length $ filter id [True, True, False, True]
-- 3
--
-- >>> Just (Just 3) >>= id
-- Just 3
--
-- >>> foldr id 0 [(^3), (*5), (+2)]
-- 1000
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
-- | @const x y@ always evaluates to @x@, ignoring its second argument.
--
-- > const x = \_ -> x
--
-- This function might seem useless at first glance, but it can be very useful
-- in a higher order context.
--
-- ==== __Examples__
--
-- >>> const 42 "hello"
-- 42
--
-- >>> map (const 42) [0..3]
-- [42,42,42,42]
const                   :: a -> b -> a
const x _               =  x

-- | Right to left function composition.
--
-- prop> (f . g) x = f (g x)
--
-- prop> f . id = f = id . f
--
-- ==== __Examples__
--
-- >>> map ((*2) . length) [[], [0, 1, 2], [0]]
-- [0,6,2]
--
-- >>> foldr (.) id [(+1), (*3), (^3)] 2
-- 25
--
-- >>> let (...) = (.).(.) in ((*2)...(+)) 5 10
-- 30
{-# INLINE (.) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
--
-- prop> flip f x y = f y x
--
-- prop> flip . flip = id
--
-- ==== __Examples__
--
-- >>> flip (++) "hello" "world"
-- "worldhello"
--
-- >>> let (.>) = flip (.) in (+1) .> show $ 5
-- "6"
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

-- Note: Before base-4.19, ($) was not representation polymorphic
-- in both type parameters but only in the return type.
-- The generalization forced a change to the implementation,
-- changing its laziness, affecting expressions like (($) undefined): before
-- base-4.19 the expression (($) undefined) `seq` () was equivalent to
-- (\x -> undefined x) `seq` () and thus would just evaluate to (), but now
-- it is equivalent to undefined `seq` () which diverges.

{- | @'($)'@ is the __function application__ operator.

Applying @'($)'@ to a function @f@ and an argument @x@ gives the same result as applying @f@ to @x@ directly. The definition is akin to this:

@
($) :: (a -> b) -> a -> b
($) f x = f x
@

This is @'id'@ specialized from @a -> a@ to @(a -> b) -> (a -> b)@ which by the associativity of @(->)@
is the same as @(a -> b) -> a -> b@.

On the face of it, this may appear pointless! But it's actually one of the most useful and important operators in Haskell.

The order of operations is very different between @($)@ and normal function application. Normal function application has precedence 10 - higher than any operator - and associates to the left. So these two definitions are equivalent:

@
expr = min 5 1 + 5
expr = ((min 5) 1) + 5
@

@($)@ has precedence 0 (the lowest) and associates to the right, so these are equivalent:

@
expr = min 5 $ 1 + 5
expr = (min 5) (1 + 5)
@

==== __Examples__

A common use cases of @($)@ is to avoid parentheses in complex expressions.

For example, instead of using nested parentheses in the following
 Haskell function:

@
-- | Sum numbers in a string: strSum "100  5 -7" == 98
strSum :: 'String' -> 'Int'
strSum s = 'sum' ('GHC.Internal.Data.Maybe.mapMaybe' 'GHC.Internal.Text.Read.readMaybe' ('words' s))
@

we can deploy the function application operator:

@
-- | Sum numbers in a string: strSum "100  5 -7" == 98
strSum :: 'String' -> 'Int'
strSum s = 'sum' '$' 'GHC.Internal.Data.Maybe.mapMaybe' 'GHC.Internal.Text.Read.readMaybe' '$' 'words' s
@

@($)@ is also used as a section (a partially applied operator), in order to indicate that we wish to apply some yet-unspecified function to a given value. For example, to apply the argument @5@ to a list of functions:

@
applyFive :: [Int]
applyFive = map ($ 5) [(+1), (2^)]
>>> [6, 32]
@

==== __Technical Remark (Representation Polymorphism)__

@($)@ is fully representation-polymorphic. This allows it to also be used with arguments of unlifted and even unboxed kinds, such as unboxed integers:

@
fastMod :: Int -> Int -> Int
fastMod (I# x) (I# m) = I# $ remInt# x m
@
-}
{-# INLINE ($) #-}
($) :: forall repa repb (a :: TYPE repa) (b :: TYPE repb). (a -> b) -> a -> b
($) f = f

-- | Strict (call-by-value) application operator. It takes a function and an
-- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- the function with that value.

($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
{-# INLINE ($!) #-}
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

-- | @since base-2.01
instance  Functor IO where
   fmap f x = x >>= (pure . f)

-- | @since base-2.01
instance Applicative IO where
    {-# INLINE pure #-}
    {-# INLINE (*>) #-}
    {-# INLINE liftA2 #-}
    pure  = returnIO
    (*>)  = thenIO
    (<*>) = ap
    liftA2 = liftM2

-- | @since base-2.01
instance  Monad IO  where
    {-# INLINE (>>)   #-}
    {-# INLINE (>>=)  #-}
    (>>)      = (*>)
    (>>=)     = bindIO

-- | Takes the first non-throwing 'IO' action\'s result.
-- 'empty' throws an exception.
--
-- @since base-4.9.0.0
instance Alternative IO where
    empty = failIO "mzero"
    (<|>) = mplusIO

-- | Takes the first non-throwing 'IO' action\'s result.
-- 'mzero' throws an exception.
--
-- @since base-4.9.0.0
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
Returns the tag of a constructor application; this function was once used
by the deriving code for Eq, Ord and Enum.
-}
{-# INLINE getTag #-}
getTag :: forall {lev :: Levity} (a :: TYPE (BoxedRep lev))
       .  DataToTag a => a -> Int#
getTag = dataToTag#

----------------------------------------------
-- GHC.Internal.Numeric primops
----------------------------------------------

-- Definitions of the boxed PrimOps; these will be
-- used in the case of partial applications, etc.

-- See Note [INLINE division wrappers]
{-# INLINE quotInt #-}
{-# INLINE remInt #-}
{-# INLINE divInt #-}
{-# INLINE modInt #-}
{-# INLINE quotRemInt #-}
{-# INLINE divModInt #-}

-- | Used to implement `quot` for the `Integral` typeclass.
--   This performs integer division on its two parameters, truncated towards zero.
--
-- ==== __Example__
-- >>> quotInt 10 2
-- 5
--
-- >>> quot 10 2
-- 5
quotInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
-- | Used to implement `rem` for the `Integral` typeclass.
--   This gives the remainder after integer division of its two parameters, satisfying
--
-- > ((x `quot` y) * y) + (x `rem` y) == x
--
-- ==== __Example__
-- >>> remInt 3 2
-- 1
--
-- >>> rem 3 2
-- 1
remInt  :: Int -> Int -> Int
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
-- | Used to implement `div` for the `Integral` typeclass.
--   This performs integer division on its two parameters, truncated towards negative infinity.
--
-- ==== __Example__
-- >>> 10 `divInt` 2
-- 5
--
-- >>> 10 `div` 2
-- 5
divInt  :: Int -> Int -> Int
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
-- | Used to implement `mod` for the `Integral` typeclass.
--   This performs the modulo operation, satisfying
--
-- > ((x `div` y) * y) + (x `mod` y) == x
--
-- ==== __Example__
-- >>> 7 `modInt` 3
-- 1
--
-- >>> 7 `mod` 3
-- 1
modInt  :: Int -> Int -> Int
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)


-- | Used to implement `quotRem` for the `Integral` typeclass.
--   This gives a tuple equivalent to
--
-- > (quot x y, mod x y)
--
-- ==== __Example__
-- >>> quotRemInt 10 2
-- (5,0)
--
-- >>> quotRem 10 2
-- (5,0)
quotRemInt :: Int -> Int -> (Int, Int)
(I# x) `quotRemInt` (I# y) = case x `quotRemInt#` y of
                             (# q, r #) ->
                                 (I# q, I# r)

-- | Used to implement `divMod` for the `Integral` typeclass.
--   This gives a tuple equivalent to
--
-- > (div x y, mod x y)
--
-- ==== __Example__
-- >>> divModInt 10 2
-- (5,0)
--
-- >>> divMod 10 2
-- (5,0)
divModInt :: Int -> Int -> (Int, Int)
(I# x) `divModInt` (I# y) = case x `divModInt#` y of
                            (# q, r #) -> (I# q, I# r)

{- Note [INLINE division wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Int division functions such as 'quotRemInt' and 'divModInt' have
been manually worker/wrappered, presumably because they construct
*nested* products.
We intend to preserve the exact worker/wrapper split, hence we mark
the wrappers INLINE (#19267). That makes sure the optimiser doesn't
accidentally inline the worker into the wrapper, undoing the manual
split again.
-}

-- Wrappers for the shift operations.  The uncheckedShift# family are
-- undefined when the amount being shifted by is greater than the size
-- in bits of Int#, so these wrappers perform a check and return
-- either zero or -1 appropriately.
--
-- Note that these wrappers still produce undefined results when the
-- second argument (the shift amount) is negative.

-- | This function is used to implement branchless shifts. If the number of bits
-- to shift is greater than or equal to the type size in bits, then the shift
-- must return 0.  Instead of doing a test, we use a mask obtained via this
-- function which is branchless too.
--
--    shift_mask m b
--      | b < m     = 0xFF..FF
--      | otherwise = 0
--
shift_mask :: Int# -> Int# -> Int#
{-# INLINE shift_mask #-}
shift_mask m b = negateInt# (b <# m)

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
shiftL# :: Word# -> Int# -> Word#
a `shiftL#` b = (a `uncheckedShiftL#` b) `and#` int2Word# (shift_mask WORD_SIZE_IN_BITS# b)

-- | Shift the argument right by the specified number of bits
-- (which must be non-negative).
-- The "RL" means "right, logical" (as opposed to RA for arithmetic)
-- (although an arithmetic right shift wouldn't make sense for Word#)
shiftRL# :: Word# -> Int# -> Word#
a `shiftRL#` b = (a `uncheckedShiftRL#` b) `and#` int2Word# (shift_mask WORD_SIZE_IN_BITS# b)

-- | Shift the argument left by the specified number of bits
-- (which must be non-negative).
iShiftL# :: Int# -> Int# -> Int#
a `iShiftL#` b = (a `uncheckedIShiftL#` b) `andI#` shift_mask WORD_SIZE_IN_BITS# b

-- | Shift the argument right (signed) by the specified number of bits
-- (which must be non-negative).
-- The "RA" means "right, arithmetic" (as opposed to RL for logical)
iShiftRA# :: Int# -> Int# -> Int#
a `iShiftRA#` b | isTrue# (b >=# WORD_SIZE_IN_BITS#) = negateInt# (a <# 0#)
                | otherwise                          = a `uncheckedIShiftRA#` b

-- | Shift the argument right (unsigned) by the specified number of bits
-- (which must be non-negative).
-- The "RL" means "right, logical" (as opposed to RA for arithmetic)
iShiftRL# :: Int# -> Int# -> Int#
a `iShiftRL#` b = (a `uncheckedIShiftRL#` b) `andI#` shift_mask WORD_SIZE_IN_BITS# b

-- Rules for C strings (the functions themselves are now in GHC.CString)
{-# RULES
"unpack"       [~1] forall a   . unpackCString# a             = build (unpackFoldrCString# a)
"unpack-list"  [1]  forall a   . unpackFoldrCString# a (:) [] = unpackCString# a
"unpack-append"     forall a n . unpackFoldrCString# a (:) n  = unpackAppendCString# a n
"unpack-append-nil" forall a   . unpackAppendCString# a []    = unpackCString# a

"unpack-utf8"       [~1] forall a   . unpackCStringUtf8# a             = build (unpackFoldrCStringUtf8# a)
"unpack-list-utf8"  [1]  forall a   . unpackFoldrCStringUtf8# a (:) [] = unpackCStringUtf8# a
"unpack-append-utf8"     forall a n . unpackFoldrCStringUtf8# a (:) n  = unpackAppendCStringUtf8# a n
"unpack-append-nil-utf8" forall a   . unpackAppendCStringUtf8# a []    = unpackCStringUtf8# a

-- There's a built-in rule (in GHC.Core.Op.ConstantFold) for
--      unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n

-- See also the Note [String literals in GHC] in CString.hs

  #-}
