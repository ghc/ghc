{-# LANGUAGE ExplicitNamespaces #-}
module Main where

import Control.Monad (unless)
import Data.Maybe (isJust, isNothing)
import GHC.Builtin.WiredIn.Types (liftedTypeKind, maybeTyCon)
import GHC.Core.Coercion (Role(Nominal), abstractNativeIndices, mkCoVarCo, mkTyConAppCo)
import GHC.Core.Predicate (mkNomEqPred)
import GHC.Core.TyCo.Rep (Type(..))
import GHC.Core.Type (mkTyConApp, mkTyVarTy, data ManyTy)
import GHC.Data.FastString (fsLit)
import GHC.Types.Id (mkSysLocal, mkSysLocalOrCoVar)
import GHC.Types.Name (mkSystemName)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (mkUniqueGrimilyWithTag)
import GHC.Types.Var (mkTyVar)

-- | Check whether a cast mentions the index selected for abstraction.
main :: IO ()
main = do
    let unique = mkUniqueGrimilyWithTag 'R'
        tyVar name i = mkTyVar (mkSystemName (unique i) (mkTyVarOcc name))
        a = mkTyVarTy (tyVar "a" 81001 liftedTypeKind)
        b = mkTyVarTy (tyVar "b" 81002 liftedTypeKind)
        domain = mkTyConApp maybeTyCon [a]
        x = mkSysLocal (fsLit "x") (unique 81003) ManyTy domain
        y = mkSysLocal (fsLit "y") (unique 81004) ManyTy domain
        cv i a' b' = mkCoVarCo (mkSysLocalOrCoVar (fsLit "co") (unique i) ManyTy (mkNomEqPred a' b'))
        independent = mkTyConAppCo Nominal maybeTyCon [cv 81005 a b]
        dependent = cv 81006 (TermVarTy x) (TermVarTy y)
        abstract = abstractNativeIndices [(TermVarTy x, tyVar "t" 81007 domain)]
    unless (isJust (abstract (CastTy (TermVarTy x) independent))) $
        fail "The cast shares the kind parameter a. Its proof is independent of x."
    unless (isNothing (abstract (CoercionTy dependent))) $
        fail "The endpoint of the coercion variable mentions x."
