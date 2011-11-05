-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Interface.ExtractFnArgDocs
-- Copyright   :  (c) Isaac Dupree 2009,
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Interface.ExtractFnArgDocs (
  getDeclFnArgDocs, getSigFnArgDocs, getTypeFnArgDocs
) where

import Haddock.Types

import qualified Data.Map as Map
import Data.Map (Map)

import GHC

-- the type of Name doesn't matter, except in 6.10 where
-- HsDocString = HsDoc Name, so we can't just say "HsDecl name" yet.

getDeclFnArgDocs :: HsDecl Name -> Map Int HsDocString
getDeclFnArgDocs (SigD (TypeSig _ ty)) = getTypeFnArgDocs ty
getDeclFnArgDocs (ForD (ForeignImport _ ty _ _)) = getTypeFnArgDocs ty
getDeclFnArgDocs (TyClD (TySynonym {tcdSynRhs = ty})) = getTypeFnArgDocs ty
getDeclFnArgDocs _ = Map.empty

getSigFnArgDocs :: Sig Name -> Map Int HsDocString
getSigFnArgDocs (TypeSig _ ty) = getTypeFnArgDocs ty
getSigFnArgDocs _ = Map.empty

getTypeFnArgDocs :: LHsType Name -> Map Int HsDocString
getTypeFnArgDocs ty = getLTypeDocs 0 ty


getLTypeDocs :: Int -> LHsType Name -> Map Int HsDocString
getLTypeDocs n (L _ ty) = getTypeDocs n ty

getTypeDocs :: Int -> HsType Name -> Map Int HsDocString
getTypeDocs n (HsForAllTy _ _ _ ty) = getLTypeDocs n ty
getTypeDocs n (HsFunTy (L _ (HsDocTy _arg_type (L _ doc))) res_type) =
      Map.insert n doc $ getLTypeDocs (n+1) res_type
getTypeDocs n (HsFunTy _ res_type) = getLTypeDocs (n+1) res_type
getTypeDocs n (HsDocTy _res_type (L _ doc)) = Map.singleton n doc
getTypeDocs _ _res_type = Map.empty

