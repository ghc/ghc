-- |
-- Module      : Basement.Compat.Base
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- internal re-export of all the good base bits
module Basement.Compat.Base
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
    , Prelude.error
    , Prelude.and
    , Prelude.undefined
    , Prelude.seq
    , Prelude.Show (..)
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
    , Prelude.Char
    , Basement.Compat.NumLiteral.Integral (..)
    , Basement.Compat.NumLiteral.Fractional (..)
    , Basement.Compat.NumLiteral.HasNegation (..)
    , Data.Int.Int8, Data.Int.Int16, Data.Int.Int32, Data.Int.Int64
    , Data.Word.Word8, Data.Word.Word16, Data.Word.Word32, Data.Word.Word64, Data.Word.Word
    , Prelude.Double, Prelude.Float
    , Prelude.IO
    , Basement.Compat.IsList.IsList (..)
    , GHC.Exts.IsString (..)
    , GHC.Generics.Generic
    , Prelude.Either (..)
    , Data.Data.Data (..)
    , Data.Data.mkNoRepType
    , Data.Data.DataType
    , Basement.Compat.Typeable.Typeable
    , Data.Monoid.Monoid (..)
    , (Data.Monoid.<>)
    , Control.Exception.Exception
    , Control.Exception.throw
    , Control.Exception.throwIO
    , GHC.Ptr.Ptr(..)
    , ifThenElse
    , internalError
    ) where

import qualified Prelude
import qualified Control.Category
import qualified Control.Applicative
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Monoid
import qualified Data.Data
import qualified Data.Word
import qualified Data.Int
import qualified Basement.Compat.IsList
import qualified Basement.Compat.NumLiteral
import qualified Basement.Compat.Typeable
import qualified GHC.Exts
import qualified GHC.Generics
import qualified GHC.Ptr
import           GHC.Exts (fromString)

-- | Only to use internally for internal error cases
internalError :: [Prelude.Char] -> a
internalError s = Prelude.error ("Internal Error: the impossible happened: " Prelude.++ s)

-- | for support of if .. then .. else
ifThenElse :: Prelude.Bool -> a -> a -> a
ifThenElse Prelude.True  e1 _  = e1
ifThenElse Prelude.False _  e2 = e2
