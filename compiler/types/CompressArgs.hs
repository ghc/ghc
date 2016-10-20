module CompressArgs (compressArgs, uncompressArgs) where

import Type
import TyCoRep
import Panic

import Data.List ( findIndex )

compressArgs    ::                               Type -> [a] -> [a]
uncompressArgs  :: (a -> Type) -> (Type -> a) -> Type -> [a] -> [a]

compressArgs funTy args = go pis args
 where
    (pis,_) = splitPiTys funTy

    -- Remove redundant type type arguments
    go (Named tyBndr : pis) (_ : args)
      | any (isRedundandTyVar (binderVar tyBndr)) pis
      = go pis args

    go (_ : pis) (a : args) = a : go pis args
    go [] [] = []
    -- Error conditions below
    go [] _ = panic "compressArgs: not enough arrows in type"
    go _ [] = panic "compressArgs: not enough args"

uncompressArgs typeOf mkType funTy args = go pis args
 where
    (pis,_) = splitPiTys funTy

    go (Named tyBndr : pis) args
      | Just i <- findIndex (isRedundandTyVar (binderVar tyBndr)) pis
      -- This is a type argument we have to recover
      = let args' = go pis args
        in mkType (typeOf (args' !! i)) : args'

    go (_ : pis) (a : args) = a : go pis args
    go [] [] = []
    -- Error conditions below
    go [] _ = panic "uncompressArgs: not enough arrows in type"
    go _ [] = panic "uncompressArgs: not enough args"

isRedundandTyVar :: TyVar -> TyBinder -> Bool
isRedundandTyVar v (Anon t) | Just v' <- getTyVar_maybe t, v == v' = True
isRedundandTyVar _ _ = False

