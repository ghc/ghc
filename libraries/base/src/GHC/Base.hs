{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module GHC.Base
    ( NonEmpty(..)
    , Monad(..)
    , Applicative(..)
    , Alternative(..)
    , Functor(..)
    , MonadPlus(..)
    , String
    , Monoid(..)
    , Semigroup(..)
    , Opaque(..)
    , ord
    , (.)
    , (<**>)
    , liftA
    , liftA3
    , id
    , when
    , assert
    , mapM
    , sequence
    , (=<<)
    , join
    , liftM
    , liftM2
    , liftM3
    , liftM4
    , liftM5
    , ap
    , failIO
    , ($)
    , otherwise
    , foldr
    , const
    , flip
    , (++)
    , map
    , ($!)
    , shiftL#
    , shiftRL#
    , iShiftL#
    , iShiftRA#
    , iShiftRL#
    , build
    , augment
    , breakpoint
    , breakpointCond
    , unIO
    , until
    , asTypeOf
    , eqString
    , returnIO
    , bindIO
    , thenIO
    , mapFB
    , unsafeChr
    , maxInt
    , minInt
    , getTag
    , quotInt
    , remInt
    , divInt
    , modInt
    , quotRemInt
    , divModInt
    , shift_mask,
    
    -- * From GHC.Classes
    -- ** Implicit paramaters
    IP(..),

    -- ** Equality and ordering
    -- | Do not import these classes from this module. Import them
    -- from @Prelude@ instead.
    Eq(..),
    Ord(..),
    -- *** Monomorphic equality operators
    -- $matching_overloaded_methods_in_rules
    eqInt, neInt,
    eqWord, neWord,
    eqChar, neChar,
    eqFloat, eqDouble,
    -- *** Monomorphic comparison operators
    gtInt, geInt, leInt, ltInt, compareInt, compareInt#,
    gtWord, geWord, leWord, ltWord, compareWord, compareWord#,

    -- ** Functions over Bool
    -- | Do not import these functions from this module. Import them
    -- from @Prelude@ instead.
    (&&), (||), not,

    -- ** Integer arithmetic
    divInt#, divInt8#, divInt16#, divInt32#,
    modInt#, modInt8#, modInt16#, modInt32#,
    divModInt#, divModInt8#, divModInt16#, divModInt32#,

    -- * From GHC.CString
    -- ** Ascii variants
    unpackCString#, unpackAppendCString#, unpackFoldrCString#,
    cstringLength#,

    -- ** Utf variants
    unpackCStringUtf8#, unpackAppendCStringUtf8#, unpackFoldrCStringUtf8#,

    -- ** Other
    unpackNBytes#,

    -- * From GHC.Magic
    inline, noinline, lazy, oneShot, runRW#,

    -- * From GHC.Magic.Dict
    WithDict(withDict),

    -- * From GHC.Types
    -- ** Built-in types
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

    -- ** Type equality
    type (~), type (~~), Coercible,

    -- ** Representation polymorphism
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

    -- ** Boxing constructors
    DictBox( MkDictBox ),
    WordBox( MkWordBox), IntBox( MkIntBox),
    FloatBox( MkFloatBox), DoubleBox( MkDoubleBox),

    -- ** Multiplicity types
    Multiplicity(..), MultMul,

    -- ** Runtime type representation
    Module(..), TrName(..), TyCon(..), TypeLitSort(..),
    KindRep(..), KindBndr,

    -- * From GHC.Prim
    -- * From GHC.Prim.Ext
    -- * From GHC.Prim.PtrEq
    -- * From GHC.Err
    absentErr, error, errorWithoutStackTrace, undefined,

    -- * From GHC.Maybe
    Maybe (..)
    ) where

import "ghc-base" GHC.Base
