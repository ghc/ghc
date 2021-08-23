module GHC.Builtin.Uniques where

import GHC.Prelude
import GHC.Types.Unique
import {-# SOURCE #-} GHC.Types.Name
import GHC.Types.Basic
import GHC.Data.FastString

-- Needed by GHC.Builtin.Types
knownUniqueName :: Unique -> Maybe Name

mkSumTyConUnique :: Arity -> Unique
mkSumDataConUnique :: ConTagZ -> Arity -> Unique

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleDataConUnique :: Arity -> Unique

mkTupleTyConUnique :: Boxity -> Arity -> Unique
mkTupleDataConUnique :: Boxity -> Arity -> Unique

mkAlphaTyVarUnique     :: Int -> Unique
mkPreludeClassUnique   :: Int -> Unique
mkPrimOpIdUnique       :: Int -> Unique
mkPrimOpWrapperUnique  :: Int -> Unique
mkPreludeMiscIdUnique  :: Int -> Unique

mkPseudoUniqueE, mkBuiltinUnique :: Int -> Unique

mkRegSingleUnique, mkRegPairUnique, mkRegSubUnique, mkRegClassUnique :: Int -> Unique
mkVarOccUnique, mkDataOccUnique, mkTvOccUnique, mkTcOccUnique :: FastString -> Unique

initExitJoinUnique :: Unique

mkPreludeTyConUnique   :: Int -> Unique
tyConRepNameUnique :: Unique -> Unique

mkPreludeDataConUnique :: Int -> Unique
dataConTyRepNameUnique, dataConWorkerUnique :: Unique -> Unique
