-- |
-- Module      : Basement.Imports
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- re-export of all the base prelude and basic primitive stuffs
{-# LANGUAGE CPP #-}
module Basement.Imports
    ( (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Control.Category..)
    , (Control.Applicative.<$>)
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Control.Category.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Basement.Error.error
    , Prelude.and
    , Prelude.undefined
    , Prelude.seq
    , Prelude.Show
    , Basement.Show.show
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Functor (..)
    , Control.Applicative.Applicative (..)
    , Prelude.Monad (..)
    , Control.Monad.when
    , Control.Monad.unless
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Int
    , Prelude.Integer
    , Basement.Compat.Natural.Natural
    , Basement.Types.OffsetSize.Offset
    , Basement.Types.OffsetSize.CountOf
    , Prelude.Char
    , Basement.PrimType.PrimType
    , Basement.Types.Char7.Char7
    , Basement.Types.AsciiString.AsciiString
    , Basement.UTF8.Base.String
    , Basement.UArray.UArray
    , Basement.BoxedArray.Array
    , Basement.Compat.NumLiteral.Integral (..)
    , Basement.Compat.NumLiteral.Fractional (..)
    , Basement.Compat.NumLiteral.HasNegation (..)
    , Data.Int.Int8, Data.Int.Int16, Data.Int.Int32, Data.Int.Int64
    , Data.Word.Word8, Data.Word.Word16, Data.Word.Word32, Data.Word.Word64, Data.Word.Word
    , Prelude.Double, Prelude.Float
    , Prelude.IO
    , FP32
    , FP64
    , Basement.Compat.IsList.IsList (..)
    , GHC.Exts.IsString (..)
    , GHC.Generics.Generic (..)
    , Prelude.Either (..)
    , Data.Data.Data (..)
    , Data.Data.mkNoRepType
    , Data.Data.DataType
    , Data.Typeable.Typeable
    , Data.Monoid.Monoid (..)
#if MIN_VERSION_base(4,10,0)
    -- , (Basement.Compat.Semigroup.<>)
    , Basement.Compat.Semigroup.Semigroup(..)
#else
    , (Data.Monoid.<>)
    , Basement.Compat.Semigroup.Semigroup
#endif
    , Control.Exception.Exception
    , Control.Exception.throw
    , Control.Exception.throwIO
    , GHC.Ptr.Ptr(..)
    , ifThenElse
    ) where

import qualified Prelude
import qualified Control.Category
import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Monoid
import qualified Data.Data
import qualified Data.Typeable
import qualified Data.Word
import qualified Data.Int
import qualified Basement.Compat.IsList
import qualified Basement.Compat.Natural
import qualified Basement.Compat.NumLiteral
import qualified Basement.Compat.Semigroup
import qualified Basement.UArray
import qualified Basement.BoxedArray
import qualified Basement.UTF8.Base
import qualified Basement.Error
import qualified Basement.Show
import qualified Basement.PrimType
import qualified Basement.Types.OffsetSize
import qualified Basement.Types.AsciiString
import qualified Basement.Types.Char7
import qualified GHC.Exts
import qualified GHC.Generics
import qualified GHC.Ptr
import           GHC.Exts (fromString)

-- | for support of if .. then .. else
ifThenElse :: Prelude.Bool -> a -> a -> a
ifThenElse Prelude.True  e1 _  = e1
ifThenElse Prelude.False _  e2 = e2

-- | IEEE754 Floating point Binary32, simple precision (Also known as Float)
type FP32 = Prelude.Float

-- | IEEE754 Floating point Binary64, double precision (Also known as Double)
type FP64 = Prelude.Double
