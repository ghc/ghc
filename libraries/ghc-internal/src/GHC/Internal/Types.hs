{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples,
             MultiParamTypeClasses, RoleAnnotations, CPP, TypeOperators,
             PolyKinds, NegativeLiterals, DataKinds, ScopedTypeVariables,
             TypeApplications, StandaloneKindSignatures, GADTs,
             FlexibleInstances, UndecidableInstances, UnboxedSums #-}
-- NegativeLiterals: see Note [Fixity of (->)]
{-# OPTIONS_HADDOCK print-explicit-runtime-reps #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Internal.Types
-- Copyright   :  (c) The University of Glasgow 2009
-- License     :  see libraries/ghc-internal/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- GHC type definitions.
-- Use GHC.Exts from the base package instead of importing this
-- module directly.
--
-----------------------------------------------------------------------------

module GHC.Internal.Types (
        -- * Built-in types
        Bool(..), Char(..), Int(..), Word(..),
        Float(..), Double(..),
        Ordering(..), IO(..),

        List,   -- List( [], (:) )
          -- List constructors are not exported
          -- because they are built-in syntax

        isTrue#,
        SPEC(..),
        Symbol,
        Any,

        -- * Type equality
        type (~), type (~~), Coercible,

        -- * Representation polymorphism
        TYPE, CONSTRAINT,
        Levity(..), RuntimeRep(..),
        LiftedRep, UnliftedRep,
        Type, UnliftedType, Constraint,
          -- The historical type * should ideally be written as
          -- `type *`, without the parentheses. But that's a true
          -- pain to parse, and for little gain.
        ZeroBitRep, ZeroBitType,
        VecCount(..), VecElem(..),
        Void#,

        -- * Boxing constructors
        DictBox( MkDictBox ),
        WordBox( MkWordBox), IntBox( MkIntBox),
        FloatBox( MkFloatBox), DoubleBox( MkDoubleBox),

        -- * Multiplicity types
        Multiplicity(..), MultMul,

        -- * Runtime type representation
        Module(..), TrName(..), TyCon(..), TypeLitSort(..),
        KindRep(..), KindBndr,

        -- * Unboxed tuples
        Unit#,
        Solo#(..),
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

        -- * Unboxed sums
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

    ) where

import GHC.Internal.Prim

infixr 5 :

{- *********************************************************************
*                                                                      *
                  Functions
*                                                                      *
********************************************************************* -}

infixr -1 ->
{-
Note [Fixity of (->)]
~~~~~~~~~~~~~~~~~~~~~
This declaration is important for :info (->) command (issue #10145)
1) The parser parses -> as if it had lower fixity than 0,
   so we conventionally use -1 (issue #15235).
2) Fixities outside the 0-9 range are exceptionally allowed
   for (->) (see checkPrecP in RdrHsSyn)
3) The negative fixity -1 must be parsed as a single token,
   hence this module requires NegativeLiterals.
-}

-- | The regular function type
type (->) = FUN 'Many
-- See Note [Linear types] in Multiplicity

{- *********************************************************************
*                                                                      *
                  Kinds
*                                                                      *
********************************************************************* -}



-- | The runtime representation of lifted types.
type LiftedRep = 'BoxedRep 'Lifted

-- | The runtime representation of unlifted types.
type UnliftedRep = 'BoxedRep 'Unlifted

-- | The runtime representation of a zero-width tuple,
--   represented by no bits at all
type ZeroBitRep = 'TupleRep '[]

-------------------------
-- | The kind of lifted constraints
type Constraint = CONSTRAINT LiftedRep

-- | The kind of types with lifted values. For example @Int :: Type@.
type Type = TYPE LiftedRep

-- | The kind of boxed, unlifted values, for example @Array#@ or a user-defined
-- unlifted data type, using @-XUnliftedDataTypes@.
type UnliftedType = TYPE UnliftedRep

-- | The kind of the empty unboxed tuple type (# #)
type ZeroBitType = TYPE ZeroBitRep

-------------------------
data Multiplicity = Many | One

type family MultMul (a :: Multiplicity) (b :: Multiplicity) :: Multiplicity where
  MultMul 'One x = x
  MultMul x 'One = x
  MultMul 'Many x = 'Many
  MultMul x 'Many = 'Many

{- *********************************************************************
*                                                                      *
                  Symbol
*                                                                      *
********************************************************************* -}

-- | (Kind) This is the kind of type-level symbols.
data Symbol

-- Symbol is declared here because class IP needs it

{- *********************************************************************
*                                                                      *
                  Any
*                                                                      *
********************************************************************* -}

-- | The type constructor @Any :: forall k. k@ is a type to which you can unsafely coerce any type, and back.
--
-- For @unsafeCoerce@ this means for all lifted types @t@ that
-- @unsafeCoerce (unsafeCoerce x :: Any) :: t@ is equivalent to @x@ and safe.
--
-- The same is true for *all* types when using
-- @
--   unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
--                   (a :: TYPE r1) (b :: TYPE r2).
--                   a -> b
-- @
-- but /only/ if you instantiate @r1@ and @r2@ to the /same/ runtime representation.
-- For example using @(unsafeCoerce# :: forall (a :: TYPE IntRep) (b :: TYPE IntRep). a -> b) x@
-- is fine, but @(unsafeCoerce# :: forall (a :: TYPE IntRep) (b :: TYPE FloatRep). a -> b)@
-- will likely cause seg-faults or worse.
-- For this resason, users should always prefer unsafeCoerce over unsafeCoerce# when possible.
--
-- Here are some more examples:
-- @
--    bad_a1 :: Any @(TYPE 'IntRep)
--    bad_a1 = unsafeCoerce# True
--
--    bad_a2 :: Any @(TYPE ('BoxedRep 'UnliftedRep))
--    bad_a2 = unsafeCoerce# True
-- @
-- Here @bad_a1@ is bad because we started with @True :: (Bool :: Type)@, represented by a boxed heap pointer,
-- and coerced it to @a1 :: Any @(TYPE 'IntRep)@, whose representation is a non-pointer integer.
-- That's why we had to use `unsafeCoerce#`; it is really unsafe because it can change representations.
-- Similarly @bad_a2@ is bad because although both @True@ and @bad_a2@ are represented by a heap pointer,
-- @True@ is lifted but @bad_a2@ is not; bugs here may be rather subtle.
--
-- If you must use unsafeCoerce# to cast to `Any`, type annotations are recommended
-- to make sure that @Any@ has the correct kind. As casting between different runtimereps is
-- unsound. For example to cast a @ByteArray#@ to @Any@ you might use:
-- @
--    unsafeCoerce# b :: (Any :: TYPE ('BoxedRep 'Unlifted))
-- @
type family Any :: k where { }
-- See Note [Any types] in GHC.Builtin.Types. Also, for a bit of history on Any see
-- #10886. Note that this must be a *closed* type family: we need to ensure
-- that this can't reduce to a `data` type for the results discussed in
-- Note [Any types].

{- *********************************************************************
*                                                                      *
                  Lists

   NB: lists are built-in syntax, and hence not explicitly exported
*                                                                      *
********************************************************************* -}

-- | The builtin linked list type.
--
-- In Haskell, lists are one of the most important data types as they are
-- often used analogous to loops in imperative programming languages.
-- These lists are singly linked, which makes them unsuited for operations
-- that require \(\mathcal{O}(1)\) access. Instead, they are intended to
-- be traversed.
--
-- You can use @List a@ or @[a]@ in type signatures:
--
-- > length :: [a] -> Int
--
-- or
--
-- > length :: List a -> Int
--
-- They are fully equivalent, and @List a@ will be normalised to @[a]@.
--
-- ==== Usage
--
-- Lists are constructed recursively using the right-associative constructor operator (or /cons/)
-- @(:) :: a -> [a] -> [a]@, which prepends an element to a list,
-- and the empty list @[]@.
--
-- @
-- (1 : 2 : 3 : []) == (1 : (2 : (3 : []))) == [1, 2, 3]
-- @
--
-- Lists can also be constructed using list literals
-- of the form @[x_1, x_2, ..., x_n]@
-- which are syntactic sugar and, unless @-XOverloadedLists@ is enabled,
-- are translated into uses of @(:)@ and @[]@
--
-- 'Data.String.String' literals, like @"I &#x1F49C; hs"@, are translated into
-- Lists of characters, @[\'I\', \' \', \'&#x1F49C;\', \' \', \'h\', \'s\']@.
--
-- ==== __Implementation__
--
-- Internally and in memory, all the above are represented like this,
-- with arrows being pointers to locations in memory.
--
-- > ╭───┬───┬──╮   ╭───┬───┬──╮   ╭───┬───┬──╮   ╭────╮
-- > │(:)│   │ ─┼──>│(:)│   │ ─┼──>│(:)│   │ ─┼──>│ [] │
-- > ╰───┴─┼─┴──╯   ╰───┴─┼─┴──╯   ╰───┴─┼─┴──╯   ╰────╯
-- >       v              v              v
-- >       1              2              3
--
-- ==== __Examples__
--
-- @
-- >>> [\'H\', \'a\', \'s\', \'k\', \'e\', \'l\', \'l\']
-- \"Haskell\"
-- @
--
-- @
-- >>> 1 : [4, 1, 5, 9]
-- [1,4,1,5,9]
-- @
--
-- @
-- >>> [] : [] : []
-- [[],[]]
-- @
--
-- @since 0.10.0
--
data List a = [] | a : List a


{- *********************************************************************
*                                                                      *
                  Ordering
*                                                                      *
********************************************************************* -}

data Ordering = LT | EQ | GT


{- *********************************************************************
*                                                                      *
                  Int, Char, Word, Float, Double
*                                                                      *
********************************************************************* -}

{- | The character type 'Char' represents Unicode codespace
and its elements are code points as in definitions
[D9 and D10 of the Unicode Standard](https://www.unicode.org/versions/Unicode15.0.0/ch03.pdf#G2212).

Character literals in Haskell are single-quoted: @\'Q\'@, @\'Я\'@ or @\'Ω\'@.
To represent a single quote itself use @\'\\''@, and to represent a backslash
use @\'\\\\\'@. The full grammar can be found in the section 2.6 of the
[Haskell 2010 Language Report](https://www.haskell.org/definition/haskell2010.pdf#section.2.6).

To specify a character by its code point one can use decimal, hexadecimal
or octal notation: @\'\\65\'@, @\'\\x41\'@ and @\'\\o101\'@ are all alternative forms
of @\'A\'@. The largest code point is @\'\\x10ffff\'@.

There is a special escape syntax for ASCII control characters:

+-------------+-------------------+---------------------------+
| Escape      | Alternatives      | Meaning                   |
+=============+===================+===========================+
| @'\\NUL'@   | @'\\0'@           | null character            |
+-------------+-------------------+---------------------------+
| @'\\SOH'@   | @'\\1'@           | start of heading          |
+-------------+-------------------+---------------------------+
| @'\\STX'@   | @'\\2'@           | start of text             |
+-------------+-------------------+---------------------------+
| @'\\ETX'@   | @'\\3'@           | end of text               |
+-------------+-------------------+---------------------------+
| @'\\EOT'@   | @'\\4'@           | end of transmission       |
+-------------+-------------------+---------------------------+
| @'\\ENQ'@   | @'\\5'@           | enquiry                   |
+-------------+-------------------+---------------------------+
| @'\\ACK'@   | @'\\6'@           | acknowledge               |
+-------------+-------------------+---------------------------+
| @'\\BEL'@   | @'\\7'@, @'\\a'@  | bell (alert)              |
+-------------+-------------------+---------------------------+
| @'\\BS'@    | @'\\8'@, @'\\b'@  | backspace                 |
+-------------+-------------------+---------------------------+
| @'\\HT'@    | @'\\9'@, @'\\t'@  | horizontal tab            |
+-------------+-------------------+---------------------------+
| @'\\LF'@    | @'\\10'@, @'\\n'@ | line feed (new line)      |
+-------------+-------------------+---------------------------+
| @'\\VT'@    | @'\\11'@, @'\\v'@ | vertical tab              |
+-------------+-------------------+---------------------------+
| @'\\FF'@    | @'\\12'@, @'\\f'@ | form feed                 |
+-------------+-------------------+---------------------------+
| @'\\CR'@    | @'\\13'@, @'\\r'@ | carriage return           |
+-------------+-------------------+---------------------------+
| @'\\SO'@    | @'\\14'@          | shift out                 |
+-------------+-------------------+---------------------------+
| @'\\SI'@    | @'\\15'@          | shift in                  |
+-------------+-------------------+---------------------------+
| @'\\DLE'@   | @'\\16'@          | data link escape          |
+-------------+-------------------+---------------------------+
| @'\\DC1'@   | @'\\17'@          | device control 1          |
+-------------+-------------------+---------------------------+
| @'\\DC2'@   | @'\\18'@          | device control 2          |
+-------------+-------------------+---------------------------+
| @'\\DC3'@   | @'\\19'@          | device control 3          |
+-------------+-------------------+---------------------------+
| @'\\DC4'@   | @'\\20'@          | device control 4          |
+-------------+-------------------+---------------------------+
| @'\\NAK'@   | @'\\21'@          | negative acknowledge      |
+-------------+-------------------+---------------------------+
| @'\\SYN'@   | @'\\22'@          | synchronous idle          |
+-------------+-------------------+---------------------------+
| @'\\ETB'@   | @'\\23'@          | end of transmission block |
+-------------+-------------------+---------------------------+
| @'\\CAN'@   | @'\\24'@          | cancel                    |
+-------------+-------------------+---------------------------+
| @'\\EM'@    | @'\\25'@          | end of medium             |
+-------------+-------------------+---------------------------+
| @'\\SUB'@   | @'\\26'@          | substitute                |
+-------------+-------------------+---------------------------+
| @'\\ESC'@   | @'\\27'@          | escape                    |
+-------------+-------------------+---------------------------+
| @'\\FS'@    | @'\\28'@          | file separator            |
+-------------+-------------------+---------------------------+
| @'\\GS'@    | @'\\29'@          | group separator           |
+-------------+-------------------+---------------------------+
| @'\\RS'@    | @'\\30'@          | record separator          |
+-------------+-------------------+---------------------------+
| @'\\US'@    | @'\\31'@          | unit separator            |
+-------------+-------------------+---------------------------+
| @'\\SP'@    | @'\\32'@, @' '@   | space                     |
+-------------+-------------------+---------------------------+
| @'\\DEL'@   | @'\\127'@         | delete                    |
+-------------+-------------------+---------------------------+

[Data.Char](https://hackage.haskell.org/package/base/docs/Data-Char.html)
provides utilities to work with 'Char'.

-}
data {-# CTYPE "HsChar" #-} Char = C# Char#

-- | A fixed-precision integer type with at least the range @[-2^29 .. 2^29-1]@.
-- The exact range for a given implementation can be determined by using
-- 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.
data {-# CTYPE "HsInt" #-} Int = I# Int#

-- |A 'Word' is an unsigned integral type, with the same size as 'Int'.
data {-# CTYPE "HsWord" #-} Word = W# Word#

-- | Single-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE single-precision type.
data {-# CTYPE "HsFloat" #-} Float = F# Float#

-- | Double-precision floating point numbers.
-- It is desirable that this type be at least equal in range and precision
-- to the IEEE double-precision type.
data {-# CTYPE "HsDouble" #-} Double = D# Double#


{- *********************************************************************
*                                                                      *
                    IO
*                                                                      *
********************************************************************* -}

{- |
A value of type @'IO' a@ is a computation which, when performed,
does some I\/O before returning a value of type @a@.

There is really only one way to \"perform\" an I\/O action: bind it to
@Main.main@ in your program.  When your program is run, the I\/O will
be performed.  It isn't possible to perform I\/O from an arbitrary
function, unless that function is itself in the 'IO' monad and called
at some point, directly or indirectly, from @Main.main@.

'IO' is a monad, so 'IO' actions can be combined using either the do-notation
or the 'Prelude.>>' and 'Prelude.>>=' operations from the 'Prelude.Monad'
class.
-}
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))


{- *********************************************************************
*                                                                      *
                    (~) and Coercible

*                                                                      *
********************************************************************* -}

{-
Note [Kind-changing of (~) and Coercible]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(~) and Coercible are tricky to define. To the user, they must appear as
constraints, but we cannot define them as such in Haskell. But we also cannot
just define them only in GHC.Internal.Prim (like (->)), because we need a real module
for them, e.g. to compile the constructor's info table.

Furthermore the type of MkCoercible cannot be written in Haskell
(no syntax for ~#R).

So we define them as regular data types in GHC.Internal.Types, and do magic in GHC.Builtin.Types,
inside GHC, to change the kind and type.
-}


-- | Lifted, heterogeneous equality. By lifted, we mean that it
-- can be bogus (deferred type error). By heterogeneous, the two
-- types @a@ and @b@ might have different kinds. Because @~~@ can
-- appear unexpectedly in error messages to users who do not care
-- about the difference between heterogeneous equality @~~@ and
-- homogeneous equality @~@, this is printed as @~@ unless
-- @-fprint-equality-relations@ is set.
--
-- In @0.7.0@, the fixity was set to @infix 4@ to match the fixity of 'Data.Type.Equality.:~~:'.
class a ~~ b

  -- See also Note [The equality types story] in GHC.Builtin.Types.Prim

-- | Lifted, homogeneous equality. By lifted, we mean that it
-- can be bogus (deferred type error). By homogeneous, the two
-- types @a@ and @b@ must have the same kinds.

-- In @0.7.0@, the fixity was set to @infix 4@ to match the fixity of 'Data.Type.Equality.:~:'.
class a ~ b

infix 4 ~, ~~
  -- See also Note [The equality types story] in GHC.Builtin.Types.Prim

-- | @Coercible@ is a two-parameter class that has instances for types @a@ and @b@ if
--      the compiler can infer that they have the same representation. This class
--      does not have regular instances; instead they are created on-the-fly during
--      type-checking. Trying to manually declare an instance of @Coercible@
--      is an error.
--
--      Nevertheless one can pretend that the following three kinds of instances
--      exist. First, as a trivial base-case:
--
--      @instance Coercible a a@
--
--      Furthermore, for every type constructor there is
--      an instance that allows to coerce under the type constructor. For
--      example, let @D@ be a prototypical type constructor (@data@ or
--      @newtype@) with three type arguments, which have roles @nominal@,
--      @representational@ resp. @phantom@. Then there is an instance of
--      the form
--
--      @instance Coercible b b\' => Coercible (D a b c) (D a b\' c\')@
--
--      Note that the @nominal@ type arguments are equal, the
--      @representational@ type arguments can differ, but need to have a
--      @Coercible@ instance themself, and the @phantom@ type arguments can be
--      changed arbitrarily.
--
--      The third kind of instance exists for every @newtype NT = MkNT T@ and
--      comes in two variants, namely
--
--      @instance Coercible a T => Coercible a NT@
--
--      @instance Coercible T b => Coercible NT b@
--
--      This instance is only usable if the constructor @MkNT@ is in scope.
--
--      If, as a library author of a type constructor like @Set a@, you
--      want to prevent a user of your module to write
--      @coerce :: Set T -> Set NT@,
--      you need to set the role of @Set@\'s type parameter to @nominal@,
--      by writing
--
--      @type role Set nominal@
--
--      For more details about this feature, please refer to
--      <http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/coercible.pdf Safe Coercions>
--      by Joachim Breitner, Richard A. Eisenberg, Simon Peyton Jones and Stephanie Weirich.
--
-- @since 0.4.0
class Coercible (a :: k) (b :: k)
  -- See also Note [The equality types story] in GHC.Builtin.Types.Prim

{- *********************************************************************
*                                                                      *
                   Bool, and isTrue#
*                                                                      *
********************************************************************* -}

data {-# CTYPE "HsBool" #-} Bool = False | True

{-# INLINE isTrue# #-}
-- | Alias for 'tagToEnum#'. Returns True if its parameter is 1# and False
--   if it is 0#.
isTrue# :: Int# -> Bool   -- See Note [Optimizing isTrue#]
isTrue# x = tagToEnum# x

{- Note [Optimizing isTrue#]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Current definition of isTrue# is a temporary workaround. We would like to
have functions isTrue# and isFalse# defined like this:

    isTrue# :: Int# -> Bool
    isTrue# 1# = True
    isTrue# _  = False

    isFalse# :: Int# -> Bool
    isFalse# 0# = True
    isFalse# _  = False

These functions would allow us to safely check if a tag can represent True
or False. Using isTrue# and isFalse# as defined above will not introduce
additional case into the code. When we scrutinize return value of isTrue#
or isFalse#, either explicitly in a case expression or implicitly in a guard,
the result will always be a single case expression (given that optimizations
are turned on). This results from case-of-case transformation. Consider this
code (this is both valid Haskell and Core):

case isTrue# (a ># b) of
    True  -> e1
    False -> e2

Inlining isTrue# gives:

case (case (a ># b) of { 1# -> True; _ -> False } ) of
    True  -> e1
    False -> e2

Case-of-case transforms that to:

case (a ># b) of
  1# -> case True of
          True  -> e1
          False -> e2
  _  -> case False of
          True  -> e1
          False -> e2

Which is then simplified by case-of-known-constructor:

case (a ># b) of
  1# -> e1
  _  -> e2

While we get good Core here, the code generator will generate very bad Cmm
if e1 or e2 do allocation. It will push heap checks into case alternatives
which results in about 2.5% increase in code size. Until this is improved we
just make isTrue# an alias to tagToEnum#. This is a temporary solution (if
you're reading this in 2023 then things went wrong). See #8326.
-}


{- *********************************************************************
*                                                                      *
                    SPEC
*                                                                      *
********************************************************************* -}

-- | 'SPEC' is used by GHC in the @SpecConstr@ pass in order to inform
-- the compiler when to be particularly aggressive. In particular, it
-- tells GHC to specialize regardless of size or the number of
-- specializations. However, not all loops fall into this category.
--
-- Libraries can specify this by using 'SPEC' data type to inform which
-- loops should be aggressively specialized. For example,
-- instead of
--
-- > loop x where loop arg = ...
--
-- write
--
-- > loop SPEC x where loop !_ arg = ...
--
-- There is no semantic difference between 'SPEC' and 'SPEC2',
-- we just need a type with two constructors lest it is optimised away
-- before @SpecConstr@.
--
-- This type is reexported from "GHC.Exts" since GHC 9.0 and @base-4.15@.
-- For compatibility with earlier releases import it from "GHC.Internal.Types"
-- in @ghc-internal@ package.
--
-- @since 0.3.1.0
--
data SPEC = SPEC | SPEC2


{- *********************************************************************
*                                                                      *
                    Levity polymorphism
*                                                                      *
********************************************************************* -}

-- | Whether a boxed type is lifted or unlifted.
data Levity = Lifted | Unlifted

-- | GHC maintains a property that the kind of all inhabited types
-- (as distinct from type constructors or type-level data) tells us
-- the runtime representation of values of that type. This datatype
-- encodes the choice of runtime value.
-- Note that 'TYPE' is parameterised by 'RuntimeRep'; this is precisely
-- what we mean by the fact that a type's kind encodes the runtime
-- representation.
--
-- For boxed values (that is, values that are represented by a pointer),
-- a further distinction is made, between lifted types (that contain ⊥),
-- and unlifted ones (that don't).
data RuntimeRep = VecRep VecCount VecElem   -- ^ a SIMD vector type
                | TupleRep [RuntimeRep]     -- ^ An unboxed tuple of the given reps
                | SumRep [RuntimeRep]       -- ^ An unboxed sum of the given reps
                | BoxedRep Levity -- ^ boxed; represented by a pointer
                | IntRep          -- ^ signed, word-sized value
                | Int8Rep         -- ^ signed,  8-bit value
                | Int16Rep        -- ^ signed, 16-bit value
                | Int32Rep        -- ^ signed, 32-bit value
                | Int64Rep        -- ^ signed, 64-bit value
                | WordRep         -- ^ unsigned, word-sized value
                | Word8Rep        -- ^ unsigned,  8-bit value
                | Word16Rep       -- ^ unsigned, 16-bit value
                | Word32Rep       -- ^ unsigned, 32-bit value
                | Word64Rep       -- ^ unsigned, 64-bit value
                | AddrRep         -- ^ A pointer, but /not/ to a Haskell value
                | FloatRep        -- ^ a 32-bit floating point number
                | DoubleRep       -- ^ a 64-bit floating point number

-- RuntimeRep is intimately tied to TyCon.RuntimeRep (in GHC proper). See
-- Note [RuntimeRep and PrimRep] in RepType.
-- See also Note [Wiring in RuntimeRep] in GHC.Builtin.Types
-- See also Note [TYPE and CONSTRAINT] in GHC.Builtin.Type.Prim

-- | Length of a SIMD vector type
data VecCount = Vec2
              | Vec4
              | Vec8
              | Vec16
              | Vec32
              | Vec64
-- Enum, Bounded instances in GHC.Internal.Enum

-- | Element of a SIMD vector type
data VecElem = Int8ElemRep
             | Int16ElemRep
             | Int32ElemRep
             | Int64ElemRep
             | Word8ElemRep
             | Word16ElemRep
             | Word32ElemRep
             | Word64ElemRep
             | FloatElemRep
             | DoubleElemRep
-- Enum, Bounded instances in GHC.Internal.Enum

{-# DEPRECATED Void# "Void# is now an alias for the unboxed tuple (# #)." #-}
type Void# = (# #)

{- *********************************************************************
*                                                                      *
             Boxing data constructors
*                                                                      *
********************************************************************* -}

-- These "boxing" data types allow us to wrap up a value of kind (TYPE rr)
-- in a box of kind Type, for each rr.
data LiftBox   (a :: TYPE UnliftedRep) = MkLiftBox a

data IntBox    (a :: TYPE IntRep)      = MkIntBox a
data Int8Box   (a :: TYPE Int8Rep)     = MkInt8Box a
data Int16Box  (a :: TYPE Int16Rep)    = MkInt16Box a
data Int32Box  (a :: TYPE Int32Rep)    = MkInt32Box a
data Int64Box  (a :: TYPE Int64Rep)    = MkInt64Box a

data WordBox   (a :: TYPE WordRep)     = MkWordBox a
data Word8Box  (a :: TYPE Word8Rep)    = MkWord8Box a
data Word16Box (a :: TYPE Word16Rep)   = MkWord16Box a
data Word32Box (a :: TYPE Word32Rep)   = MkWord32Box a
data Word64Box (a :: TYPE Word64Rep)   = MkWord64Box a

data FloatBox  (a :: TYPE FloatRep)    = MkFloatBox a
data DoubleBox (a :: TYPE DoubleRep)   = MkDoubleBox a

-- | Data type `Dict` provides a simple way to wrap up a (lifted)
--   constraint as a type
data DictBox c where
  MkDictBox :: c => DictBox c


{- *********************************************************************
*                                                                      *
             Runtime representation of TyCon
*                                                                      *
********************************************************************* -}

{- Note [Runtime representation of modules and tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We generate a binding for M.$modName and M.$tcT for every module M and
data type T.  Things to think about

  - We want them to be economical on space; ideally pure data with no thunks.

  - We do this for every module (except this module GHC.Internal.Types), so we can't
    depend on anything else (eg string unpacking code)

That's why we have these terribly low-level representations.  The TrName
type lets us use the TrNameS constructor when allocating static data;
but we also need TrNameD for the case where we are deserialising a TyCon
or Module (for example when deserialising a TypeRep), in which case we
can't conveniently come up with an Addr#.
-}

#include "MachDeps.h"

data Module = Module
                TrName   -- ^ Package name
                TrName   -- ^ Module name

data TrName
  = TrNameS Addr#  -- ^ Static
  | TrNameD [Char] -- ^ Dynamic

-- | A de Bruijn index for a binder within a 'KindRep'.
type KindBndr = Int

-- | The representation produced by GHC for conjuring up the kind of a
-- 'Data.Typeable.TypeRep'.

-- See Note [Representing TyCon kinds: KindRep] in GHC.Tc.Instance.Typeable.
data KindRep = KindRepTyConApp TyCon [KindRep]
             | KindRepVar !KindBndr
             | KindRepApp KindRep KindRep
             | KindRepFun KindRep KindRep
             | KindRepTYPE !RuntimeRep
             | KindRepTypeLitS TypeLitSort Addr#
             | KindRepTypeLitD TypeLitSort [Char]

data TypeLitSort = TypeLitSymbol
                 | TypeLitNat
                 | TypeLitChar

-- Show instance for TyCon found in GHC.Internal.Show
data TyCon = TyCon Word64#    -- ^ Fingerprint (high)
                   Word64#    -- ^ Fingerprint (low)
                   Module     -- ^ Module in which this is defined
                   TrName     -- ^ Type constructor name
                   Int#       -- ^ How many kind variables do we accept?
                   KindRep    -- ^ A representation of the type's kind

{- *********************************************************************
*                                                                      *
             Unboxed tuples and sums
*                                                                      *
********************************************************************* -}

type Unit# :: TYPE (TupleRep '[])
data Unit# = (# #)

type Solo# :: TYPE rep -> TYPE (TupleRep '[rep])
data Solo# a = MkSolo# a

type Tuple0# = Unit#
type Tuple1# = Solo#

type Tuple2# :: TYPE r1 -> TYPE r2 -> TYPE (TupleRep [r1, r2])
data Tuple2# a b =
  (# a,b #)
type Tuple3# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE (TupleRep [r1, r2, r3])
data Tuple3# a b c =
  (# a,b,c #)
type Tuple4# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE (TupleRep [r1, r2, r3, r4])
data Tuple4# a b c d =
  (# a,b,c,d #)
type Tuple5# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE (TupleRep [r1, r2, r3, r4, r5])
data Tuple5# a b c d e =
  (# a,b,c,d,e #)
type Tuple6# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6])
data Tuple6# a b c d e f =
  (# a,b,c,d,e,f #)
type Tuple7# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7])
data Tuple7# a b c d e f g =
  (# a,b,c,d,e,f,g #)
type Tuple8# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8])
data Tuple8# a b c d e f g h =
  (# a,b,c,d,e,f,g,h #)
type Tuple9# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9])
data Tuple9# a b c d e f g h i =
  (# a,b,c,d,e,f,g,h,i #)
type Tuple10# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10])
data Tuple10# a b c d e f g h i j =
  (# a,b,c,d,e,f,g,h,i,j #)
type Tuple11# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11])
data Tuple11# a b c d e f g h i j k =
  (# a,b,c,d,e,f,g,h,i,j,k #)
type Tuple12# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12])
data Tuple12# a b c d e f g h i j k l =
  (# a,b,c,d,e,f,g,h,i,j,k,l #)
type Tuple13# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13])
data Tuple13# a b c d e f g h i j k l m =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m #)
type Tuple14# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14])
data Tuple14# a b c d e f g h i j k l m n =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n #)
type Tuple15# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15])
data Tuple15# a b c d e f g h i j k l m n o =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o #)
type Tuple16# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16])
data Tuple16# a b c d e f g h i j k l m n o p =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p #)
type Tuple17# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17])
data Tuple17# a b c d e f g h i j k l m n o p q =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q #)
type Tuple18# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18])
data Tuple18# a b c d e f g h i j k l m n o p q r =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r #)
type Tuple19# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19])
data Tuple19# a b c d e f g h i j k l m n o p q r s =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s #)
type Tuple20# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20])
data Tuple20# a b c d e f g h i j k l m n o p q r s t =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t #)
type Tuple21# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21])
data Tuple21# a b c d e f g h i j k l m n o p q r s t u =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u #)
type Tuple22# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22])
data Tuple22# a b c d e f g h i j k l m n o p q r s t u v =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v #)
type Tuple23# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23])
data Tuple23# a b c d e f g h i j k l m n o p q r s t u v w =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w #)
type Tuple24# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24])
data Tuple24# a b c d e f g h i j k l m n o p q r s t u v w x =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x #)
type Tuple25# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25])
data Tuple25# a b c d e f g h i j k l m n o p q r s t u v w x y =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y #)
type Tuple26# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26])
data Tuple26# a b c d e f g h i j k l m n o p q r s t u v w x y z =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z #)
type Tuple27# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27])
data Tuple27# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1 #)
type Tuple28# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28])
data Tuple28# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1 #)
type Tuple29# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29])
data Tuple29# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1 #)
type Tuple30# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30])
data Tuple30# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1 #)
type Tuple31# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31])
data Tuple31# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1 #)
type Tuple32# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32])
data Tuple32# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1 #)
type Tuple33# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33])
data Tuple33# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1 #)
type Tuple34# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34])
data Tuple34# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1 #)
type Tuple35# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35])
data Tuple35# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1 #)
type Tuple36# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36])
data Tuple36# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1 #)
type Tuple37# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37])
data Tuple37# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1 #)
type Tuple38# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38])
data Tuple38# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1 #)
type Tuple39# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39])
data Tuple39# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1 #)
type Tuple40# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40])
data Tuple40# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1 #)
type Tuple41# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41])
data Tuple41# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1 #)
type Tuple42# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42])
data Tuple42# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1 #)
type Tuple43# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43])
data Tuple43# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1 #)
type Tuple44# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44])
data Tuple44# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1 #)
type Tuple45# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45])
data Tuple45# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1 #)
type Tuple46# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46])
data Tuple46# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1 #)
type Tuple47# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47])
data Tuple47# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1 #)
type Tuple48# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48])
data Tuple48# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1 #)
type Tuple49# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49])
data Tuple49# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1 #)
type Tuple50# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50])
data Tuple50# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1 #)
type Tuple51# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51])
data Tuple51# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1 #)
type Tuple52# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52])
data Tuple52# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1 #)
type Tuple53# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53])
data Tuple53# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2 #)
type Tuple54# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54])
data Tuple54# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2 #)
type Tuple55# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55])
data Tuple55# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2 #)
type Tuple56# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56])
data Tuple56# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2 #)
type Tuple57# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57])
data Tuple57# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2 #)
type Tuple58# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58])
data Tuple58# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2 #)
type Tuple59# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59])
data Tuple59# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2 #)
type Tuple60# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60])
data Tuple60# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2 #)
type Tuple61# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61])
data Tuple61# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2 #)
type Tuple62# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 -> TYPE r62 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62])
data Tuple62# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2 #)
type Tuple63# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 -> TYPE r62 -> TYPE r63 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62, r63])
data Tuple63# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2 #)
type Tuple64# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 ->
  TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 ->
  TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 ->
  TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 ->
  TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 ->
  TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 ->
  TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 -> TYPE r62 -> TYPE r63 -> TYPE r64 ->
  TYPE (TupleRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22,
  r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45,
  r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62, r63, r64])
data Tuple64# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1
     r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2 =
  (# a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1,
     u1,v1,w1,x1,y1,z1,a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2 #)

{-
Note [Unboxed sum with arity 64]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While tuples are defined up to arity 64, sums can maximally have 63 alternatives.
This is due to the Unique layout for unboxed sums, which allots only six bits
for encoding the alternative.
-}

type Sum2# :: TYPE r1 -> TYPE r2 -> TYPE (SumRep [r1, r2])
data Sum2# a b =
    (# a | #)
  | (# | b #)

type Sum3# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE (SumRep [r1, r2, r3])
data Sum3# a b c =
    (# a | | #)
  | (# | b | #)
  | (# | | c #)

type Sum4# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE (SumRep [r1, r2, r3, r4])
data Sum4# a b c d =
    (# a | | | #)
  | (# | b | | #)
  | (# | | c | #)
  | (# | | | d #)

type Sum5# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE (SumRep [r1, r2, r3, r4, r5])
data Sum5# a b c d e =
    (# a | | | | #)
  | (# | b | | | #)
  | (# | | c | | #)
  | (# | | | d | #)
  | (# | | | | e #)

type Sum6# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6])
data Sum6# a b c d e f =
    (# a | | | | | #)
  | (# | b | | | | #)
  | (# | | c | | | #)
  | (# | | | d | | #)
  | (# | | | | e | #)
  | (# | | | | | f #)

type Sum7# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7])
data Sum7# a b c d e f g =
    (# a | | | | | | #)
  | (# | b | | | | | #)
  | (# | | c | | | | #)
  | (# | | | d | | | #)
  | (# | | | | e | | #)
  | (# | | | | | f | #)
  | (# | | | | | | g #)

type Sum8# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8])
data Sum8# a b c d e f g h =
    (# a | | | | | | | #)
  | (# | b | | | | | | #)
  | (# | | c | | | | | #)
  | (# | | | d | | | | #)
  | (# | | | | e | | | #)
  | (# | | | | | f | | #)
  | (# | | | | | | g | #)
  | (# | | | | | | | h #)

type Sum9# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9])
data Sum9# a b c d e f g h i =
    (# a | | | | | | | | #)
  | (# | b | | | | | | | #)
  | (# | | c | | | | | | #)
  | (# | | | d | | | | | #)
  | (# | | | | e | | | | #)
  | (# | | | | | f | | | #)
  | (# | | | | | | g | | #)
  | (# | | | | | | | h | #)
  | (# | | | | | | | | i #)

type Sum10# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10])
data Sum10# a b c d e f g h i j =
    (# a | | | | | | | | | #)
  | (# | b | | | | | | | | #)
  | (# | | c | | | | | | | #)
  | (# | | | d | | | | | | #)
  | (# | | | | e | | | | | #)
  | (# | | | | | f | | | | #)
  | (# | | | | | | g | | | #)
  | (# | | | | | | | h | | #)
  | (# | | | | | | | | i | #)
  | (# | | | | | | | | | j #)

type Sum11# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11])
data Sum11# a b c d e f g h i j k =
    (# a | | | | | | | | | | #)
  | (# | b | | | | | | | | | #)
  | (# | | c | | | | | | | | #)
  | (# | | | d | | | | | | | #)
  | (# | | | | e | | | | | | #)
  | (# | | | | | f | | | | | #)
  | (# | | | | | | g | | | | #)
  | (# | | | | | | | h | | | #)
  | (# | | | | | | | | i | | #)
  | (# | | | | | | | | | j | #)
  | (# | | | | | | | | | | k #)

type Sum12# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12])
data Sum12# a b c d e f g h i j k l =
    (# a | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | #)
  | (# | | c | | | | | | | | | #)
  | (# | | | d | | | | | | | | #)
  | (# | | | | e | | | | | | | #)
  | (# | | | | | f | | | | | | #)
  | (# | | | | | | g | | | | | #)
  | (# | | | | | | | h | | | | #)
  | (# | | | | | | | | i | | | #)
  | (# | | | | | | | | | j | | #)
  | (# | | | | | | | | | | k | #)
  | (# | | | | | | | | | | | l #)

type Sum13# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13])
data Sum13# a b c d e f g h i j k l m =
    (# a | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | #)
  | (# | | | | e | | | | | | | | #)
  | (# | | | | | f | | | | | | | #)
  | (# | | | | | | g | | | | | | #)
  | (# | | | | | | | h | | | | | #)
  | (# | | | | | | | | i | | | | #)
  | (# | | | | | | | | | j | | | #)
  | (# | | | | | | | | | | k | | #)
  | (# | | | | | | | | | | | l | #)
  | (# | | | | | | | | | | | | m #)

type Sum14# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14])
data Sum14# a b c d e f g h i j k l m n =
    (# a | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | #)
  | (# | | | | | | g | | | | | | | #)
  | (# | | | | | | | h | | | | | | #)
  | (# | | | | | | | | i | | | | | #)
  | (# | | | | | | | | | j | | | | #)
  | (# | | | | | | | | | | k | | | #)
  | (# | | | | | | | | | | | l | | #)
  | (# | | | | | | | | | | | | m | #)
  | (# | | | | | | | | | | | | | n #)

type Sum15# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15])
data Sum15# a b c d e f g h i j k l m n o =
    (# a | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | #)
  | (# | | | | | | | | i | | | | | | #)
  | (# | | | | | | | | | j | | | | | #)
  | (# | | | | | | | | | | k | | | | #)
  | (# | | | | | | | | | | | l | | | #)
  | (# | | | | | | | | | | | | m | | #)
  | (# | | | | | | | | | | | | | n | #)
  | (# | | | | | | | | | | | | | | o #)

type Sum16# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16])
data Sum16# a b c d e f g h i j k l m n o p =
    (# a | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | #)
  | (# | | | | | | | | | | k | | | | | #)
  | (# | | | | | | | | | | | l | | | | #)
  | (# | | | | | | | | | | | | m | | | #)
  | (# | | | | | | | | | | | | | n | | #)
  | (# | | | | | | | | | | | | | | o | #)
  | (# | | | | | | | | | | | | | | | p #)

type Sum17# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17])
data Sum17# a b c d e f g h i j k l m n o p q =
    (# a | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | #)
  | (# | | | | | | | | | | | | m | | | | #)
  | (# | | | | | | | | | | | | | n | | | #)
  | (# | | | | | | | | | | | | | | o | | #)
  | (# | | | | | | | | | | | | | | | p | #)
  | (# | | | | | | | | | | | | | | | | q #)

type Sum18# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18])
data Sum18# a b c d e f g h i j k l m n o p q r =
    (# a | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | #)
  | (# | | | | | | | | | | | | | | o | | | #)
  | (# | | | | | | | | | | | | | | | p | | #)
  | (# | | | | | | | | | | | | | | | | q | #)
  | (# | | | | | | | | | | | | | | | | | r #)

type Sum19# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19])
data Sum19# a b c d e f g h i j k l m n o p q r s =
    (# a | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | #)
  | (# | | | | | | | | | | | | | | | | q | | #)
  | (# | | | | | | | | | | | | | | | | | r | #)
  | (# | | | | | | | | | | | | | | | | | | s #)

type Sum20# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20])
data Sum20# a b c d e f g h i j k l m n o p q r s t =
    (# a | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | #)
  | (# | | | | | | | | | | | | | | | | | | s | #)
  | (# | | | | | | | | | | | | | | | | | | | t #)

type Sum21# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21])
data Sum21# a b c d e f g h i j k l m n o p q r s t u =
    (# a | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | #)
  | (# | | | | | | | | | | | | | | | | | | | | u #)

type Sum22# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22])
data Sum22# a b c d e f g h i j k l m n o p q r s t u v =
    (# a | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v #)

type Sum23# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23])
data Sum23# a b c d e f g h i j k l m n o p q r s t u v w =
    (# a | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w #)

type Sum24# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24])
data Sum24# a b c d e f g h i j k l m n o p q r s t u v w x =
    (# a | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x #)

type Sum25# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25])
data Sum25# a b c d e f g h i j k l m n o p q r s t u v w x y =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y #)

type Sum26# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26])
data Sum26# a b c d e f g h i j k l m n o p q r s t u v w x y z =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z #)

type Sum27# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27])
data Sum27# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 #)

type Sum28# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28])
data Sum28# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 #)

type Sum29# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29])
data Sum29# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 #)

type Sum30# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30])
data Sum30# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 #)

type Sum31# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31])
data Sum31# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 #)

type Sum32# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32])
data Sum32# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 #)

type Sum33# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33])
data Sum33# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 #)

type Sum34# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34])
data Sum34# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 #)

type Sum35# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35])
data Sum35# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 #)

type Sum36# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36])
data Sum36# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 #)

type Sum37# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37])
data Sum37# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 #)

type Sum38# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38])
data Sum38# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 #)

type Sum39# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39])
data Sum39# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 #)

type Sum40# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40])
data Sum40# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 #)

type Sum41# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41])
data Sum41# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 #)

type Sum42# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42])
data Sum42# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 #)

type Sum43# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43])
data Sum43# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 #)

type Sum44# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44])
data Sum44# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 #)

type Sum45# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45])
data Sum45# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 #)

type Sum46# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46])
data Sum46# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 #)

type Sum47# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47])
data Sum47# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 #)

type Sum48# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48])
data Sum48# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 #)

type Sum49# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49])
data Sum49# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 #)

type Sum50# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50])
data Sum50# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 #)

type Sum51# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51])
data Sum51# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 #)

type Sum52# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52])
data Sum52# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 #)

type Sum53# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53])
data Sum53# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 #)

type Sum54# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54])
data Sum54# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 #)

type Sum55# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55])
data Sum55# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 #)

type Sum56# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56])
data Sum56# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 #)

type Sum57# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57])
data Sum57# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 #)

type Sum58# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58])
data Sum58# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f2 #)

type Sum59# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59])
data Sum59# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g2 #)

type Sum60# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60])
data Sum60# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h2 #)

type Sum61# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61])
data Sum61# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i2 #)

type Sum62# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 -> TYPE r62 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62])
data Sum62# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j2 #)

type Sum63# :: TYPE r1 -> TYPE r2 -> TYPE r3 -> TYPE r4 -> TYPE r5 -> TYPE r6 -> TYPE r7 -> TYPE r8 -> TYPE r9 -> TYPE r10 -> TYPE r11 -> TYPE r12 -> TYPE r13 -> TYPE r14 -> TYPE r15 -> TYPE r16 -> TYPE r17 -> TYPE r18 -> TYPE r19 -> TYPE r20 -> TYPE r21 -> TYPE r22 -> TYPE r23 -> TYPE r24 -> TYPE r25 -> TYPE r26 -> TYPE r27 -> TYPE r28 -> TYPE r29 -> TYPE r30 -> TYPE r31 -> TYPE r32 -> TYPE r33 -> TYPE r34 -> TYPE r35 -> TYPE r36 -> TYPE r37 -> TYPE r38 -> TYPE r39 -> TYPE r40 -> TYPE r41 -> TYPE r42 -> TYPE r43 -> TYPE r44 -> TYPE r45 -> TYPE r46 -> TYPE r47 -> TYPE r48 -> TYPE r49 -> TYPE r50 -> TYPE r51 -> TYPE r52 -> TYPE r53 -> TYPE r54 -> TYPE r55 -> TYPE r56 -> TYPE r57 -> TYPE r58 -> TYPE r59 -> TYPE r60 -> TYPE r61 -> TYPE r62 -> TYPE r63 -> TYPE (SumRep [r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14, r15, r16, r17, r18, r19, r20, r21, r22, r23, r24, r25, r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36, r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, r48, r49, r50, r51, r52, r53, r54, r55, r56, r57, r58, r59, r60, r61, r62, r63])
data Sum63# a b c d e f g h i j k l m n o p q r s t u v w x y z a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1 m1 n1 o1 p1 q1 r1 s1 t1 u1 v1 w1 x1 y1 z1 a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 =
    (# a | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | b | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | c | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | d | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | e | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | f | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | g | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | h | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | i | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | j | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | k | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | l | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | m | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | n | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | o | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | p | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | q | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | r | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | s | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | t | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | u | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | v | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | w | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | x | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | y | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | z | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | a1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | b1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | c1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i1 | | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j1 | | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k1 | | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | l1 | | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | m1 | | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | n1 | | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | o1 | | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | p1 | | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | q1 | | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | r1 | | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | s1 | | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | t1 | | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | u1 | | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | v1 | | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | w1 | | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | x1 | | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | y1 | | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | z1 | | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | a2 | | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | b2 | | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | c2 | | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | d2 | | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | e2 | | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | f2 | | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | g2 | | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | h2 | | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | i2 | | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | j2 | #)
  | (# | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | k2 #)
