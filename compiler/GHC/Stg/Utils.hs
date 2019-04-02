{-# LANGUAGE CPP, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}

module GHC.Stg.Utils
    ( mkStgAltTypeFromStgAlts
    , bindersOf, bindersOfTop, bindersOfTopBinds

    , stripStgTicksTop, stripStgTicksTopE
    , idArgs

    , seqTopBinds
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Id
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.DataCon
import GHC.Core ( AltCon(..) )
import GHC.Types.Tickish

import GHC.Types.RepType
import GHC.Stg.Syntax

import GHC.Utils.Misc
import GHC.Utils.Outputable

import GHC.Utils.Panic.Plain
import GHC.Utils.Panic

-- Checks if id is a top level error application.
-- isErrorAp_maybe :: Id ->

-- | Extract the default case alternative
-- findDefaultStg :: [Alt b] -> ([Alt b], Maybe (Expr b))
findDefaultStg :: [GenStgAlt p] -> ([(AltCon, [BinderP p], GenStgExpr p)],
                       Maybe (GenStgExpr p))
findDefaultStg ((DEFAULT, args, rhs) : alts) = ASSERT( null args ) (alts, Just rhs)
findDefaultStg alts                        =                     (alts, Nothing)

mkStgAltTypeFromStgAlts :: forall p. Id -> [GenStgAlt p] -> AltType
mkStgAltTypeFromStgAlts bndr alts
  | isUnboxedTupleType bndr_ty || isUnboxedSumType bndr_ty
  = MultiValAlt (length prim_reps)  -- always use MultiValAlt for unboxed tuples

  | otherwise
  = case prim_reps of
      [rep] | isGcPtrRep rep ->
        case tyConAppTyCon_maybe (unwrapType bndr_ty) of
          Just tc
            | isAbstractTyCon tc -> look_for_better_tycon
            | isAlgTyCon tc      -> AlgAlt tc
            | otherwise          -> ASSERT2( _is_poly_alt_tycon tc, ppr tc )
                                    PolyAlt
          Nothing                -> PolyAlt
      [non_gcd] -> PrimAlt non_gcd
      not_unary -> MultiValAlt (length not_unary)
  where
   bndr_ty   = idType bndr
   prim_reps = typePrimRep bndr_ty

   _is_poly_alt_tycon tc
        =  isFunTyCon tc
        || isPrimTyCon tc   -- "Any" is lifted but primitive
        || isFamilyTyCon tc -- Type family; e.g. Any, or arising from strict
                            -- function application where argument has a
                            -- type-family type

   -- Sometimes, the TyCon is a AbstractTyCon which may not have any
   -- constructors inside it.  Then we may get a better TyCon by
   -- grabbing the one from a constructor alternative
   -- if one exists.
   look_for_better_tycon
        | (((DataAlt con) ,_, _) : _) <- data_alts =
                AlgAlt (dataConTyCon con)
        | otherwise =
                ASSERT(null data_alts)
                PolyAlt
        where
                (data_alts, _deflt) = findDefaultStg alts

bindersOf :: BinderP a ~ Id => GenStgBinding a -> [Id]
bindersOf (StgNonRec binder _) = [binder]
bindersOf (StgRec pairs)       = [binder | (binder, _) <- pairs]

bindersOfTop :: BinderP a ~ Id => GenStgTopBinding a -> [Id]
bindersOfTop (StgTopLifted bind) = bindersOf bind
bindersOfTop (StgTopStringLit binder _) = [binder]

-- All ids we bind something to on the top level.
bindersOfTopBinds :: BinderP a ~ Id => [GenStgTopBinding a] -> [Id]
-- bindersOfTopBinds binds = mapUnionVarSet (mkVarSet . bindersOfTop) binds
bindersOfTopBinds binds = foldr ((++) . bindersOfTop) [] binds

idArgs :: [StgArg] -> [Id]
idArgs args = [v | StgVarArg v <- args]

-- | Strip ticks of a given type from an STG expression.
stripStgTicksTop :: (StgTickish -> Bool) -> GenStgExpr p -> ([StgTickish], GenStgExpr p)
stripStgTicksTop p = go []
   where go ts (StgTick t e) | p t = go (t:ts) e
         -- This special case avoid building a thunk for "reverse ts" when there are no ticks
         go [] other               = ([], other)
         go ts other               = (reverse ts, other)

-- | Strip ticks of a given type from an STG expression returning only the expression.
stripStgTicksTopE :: (StgTickish -> Bool) -> GenStgExpr p -> GenStgExpr p
stripStgTicksTopE p = go
   where go (StgTick t e) | p t = go e
         go other               = other
------------------------------------

-- We do not force types here, mostly because they might not be used at all.

seqTopBinds :: [GenStgTopBinding p] -> ()
seqTopBinds binds = seqList (map seqTop binds) ()

seqTop :: GenStgTopBinding p -> ()
seqTop (StgTopStringLit !_v !_s) = ()
seqTop (StgTopLifted bind)     = seqBinds bind

seqBinds :: GenStgBinding p -> ()
seqBinds (StgNonRec !_v rhs) = (seqRhs rhs)
seqBinds (StgRec pairs)    = seqList (map (\v rhs -> v `seq` seqRhs rhs) pairs) ()

-- For top level lets we have to turn lets into closures.
seqRhs :: GenStgRhs p -> ()
seqRhs (StgRhsCon !_ccs !_con !_n ticks args)   = seqArgs args `seq` seqList ticks ()
seqRhs (StgRhsClosure !_ext !_ccs !_flag args body) = seqExpr body `seq` seqList args ()

seqExpr :: GenStgExpr p -> ()
seqExpr (StgCase scrut !_bndr !_ty alts) = seqList (map seqAlt alts) (seqExpr scrut)
seqExpr (StgLet !_x binds body)         = (seqBinds binds) `seq` (seqExpr body)
seqExpr (StgLetNoEscape !_x binds body) = (seqBinds binds) `seq` (seqExpr body)
seqExpr (StgTick !_t e)                 = seqExpr e
seqExpr (StgConApp !_con !_n args _tys) = seqArgs args

seqExpr (StgApp !_x !_f args)           = seqArgs args
seqExpr (StgLit !_lit)                  = ()
seqExpr (StgOpApp !_op args !_res_ty)   = seqArgs args

seqAlt :: GenStgAlt p -> ()
seqAlt (!_altCon, !_bndrs, rhs) = seqExpr rhs

seqArgs :: [StgArg] -> ()
seqArgs args = seqList (map seqArg args) ()

seqArg :: StgArg -> ()
seqArg (StgVarArg !_v) = ()
seqArg (StgLitArg !_l) = ()
