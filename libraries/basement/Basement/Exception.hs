-- |
-- Module      : Basement.Exception
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- Common part for vectors
--
{-# LANGUAGE DeriveDataTypeable #-}
module Basement.Exception
    ( OutOfBound(..)
    , OutOfBoundOperation(..)
    , isOutOfBound
    , outOfBound
    , primOutOfBound
    , InvalidRecast(..)
    , RecastSourceSize(..)
    , RecastDestinationSize(..)
    , NonEmptyCollectionIsEmpty(..)
    ) where

import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           Basement.Monad

-- | The type of operation that triggers an OutOfBound exception.
--
-- * OOB_Index: reading an immutable vector
-- * OOB_Read: reading a mutable vector
-- * OOB_Write: write a mutable vector
-- * OOB_MemCopy: copying a vector
-- * OOB_MemSet: initializing a mutable vector
data OutOfBoundOperation = OOB_Read | OOB_Write | OOB_MemSet | OOB_MemCopy | OOB_Index
    deriving (Show,Eq,Typeable)

-- | Exception during an operation accessing the vector out of bound
--
-- Represent the type of operation, the index accessed, and the total length of the vector.
data OutOfBound = OutOfBound OutOfBoundOperation Int Int
    deriving (Show,Typeable)

instance Exception OutOfBound

outOfBound :: OutOfBoundOperation -> Offset ty -> CountOf ty -> a
outOfBound oobop (Offset ofs) (CountOf sz) = throw (OutOfBound oobop ofs sz)
{-# INLINE outOfBound #-}

primOutOfBound :: PrimMonad prim => OutOfBoundOperation -> Offset ty -> CountOf ty -> prim a
primOutOfBound oobop (Offset ofs) (CountOf sz) = primThrow (OutOfBound oobop ofs sz)
{-# INLINE primOutOfBound #-}

isOutOfBound :: Offset ty -> CountOf ty -> Bool
isOutOfBound (Offset ty) (CountOf sz) = ty < 0 || ty >= sz
{-# INLINE isOutOfBound #-}

newtype RecastSourceSize      = RecastSourceSize Int
    deriving (Show,Eq,Typeable)
newtype RecastDestinationSize = RecastDestinationSize Int
    deriving (Show,Eq,Typeable)

data InvalidRecast = InvalidRecast RecastSourceSize RecastDestinationSize
    deriving (Show,Typeable)

instance Exception InvalidRecast

-- | Exception for using NonEmpty assertion with an empty collection
data NonEmptyCollectionIsEmpty = NonEmptyCollectionIsEmpty
    deriving (Show,Typeable)

instance Exception NonEmptyCollectionIsEmpty
