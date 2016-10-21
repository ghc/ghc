module CompressArgs (
    CompressScheme, -- abstract
    genCompressScheme,
    compressArgs,
    uncompressArgs
    ) where

import Type
import TyCoRep
import Panic

import Data.List ( findIndex, dropWhileEnd )
import Data.Maybe ( isNothing )

-- We want to analyze the data con type only once. The resulting information
-- is given by a list of offsets.
-- The list may be shorted.
-- Abstract by design.
newtype CompressScheme = CS ([Maybe Int])

genCompressScheme :: Type -> CompressScheme
genCompressScheme funTy = CS $ shorten $ go pis
 where
    (pis,_) = splitPiTys funTy

    shorten = dropWhileEnd isNothing

    go (Named tyBndr : pis)
      | Just i <- findIndex (isRedundandTyVar (binderVar tyBndr)) pis
      = Just i : go pis
    go (_ : pis)
      = Nothing : go pis
    go []
      = []


compressArgs    ::                               CompressScheme -> [a] -> [a]
uncompressArgs  :: (a -> Type) -> (Type -> a) -> CompressScheme -> [a] -> [a]

compressArgs (CS cs) args = go cs args
 where
    go (Just _  : pis) (_ : args) =     go pis args
    go (Nothing : pis) (a : args) = a : go pis args
    go []              args       = args
    go _               []         = panic "compressArgs: not enough args"

uncompressArgs typeOf mkType (CS cs) args = go cs args
 where
    go (Just i  : pis) args       = mkType (typeOf (args' !! i)) : args'
      where args' = go pis args
    go (Nothing : pis) (a : args) = a : args'
      where args' = go pis args
    go []              args       = args
    -- Error conditions below
    go _               []         = panic "uncompressArgs: not enough args"

isRedundandTyVar :: TyVar -> TyBinder -> Bool
isRedundandTyVar v (Anon t) | Just v' <- getTyVar_maybe t, v == v' = True
isRedundandTyVar _ _ = False

