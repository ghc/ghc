{-# LANGUAGE CPP, ScopedTypeVariables, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module GHC.Stg.Utils
    ( mkStgAltTypeFromStgAlts
    , bindersOf, bindersOfX, bindersOfTop, bindersOfTopBinds

    , stripStgTicksTop, stripStgTicksTopE
    , idArgs

    , mkUnarisedId, mkUnarisedIds

    , hasNoNonZeroWidthArgs
    ) where

import GHC.Prelude

import GHC.Types.Id
import GHC.Core.Type
import GHC.Core.TyCon
import GHC.Core.Multiplicity     ( scaledThing )
import GHC.Core.DataCon
import GHC.Core ( AltCon(..) )
import GHC.Types.Tickish
import GHC.Types.Unique.Supply

import GHC.Types.RepType
import GHC.Stg.Syntax

import GHC.Utils.Outputable

import GHC.Utils.Panic

import GHC.Data.FastString

-- | Returns whether there are any arguments with a non-zero-width runtime
-- representation.
--
-- Returns True if the datacon has no or /just/ zero-width arguments.
hasNoNonZeroWidthArgs :: DataCon -> Bool
hasNoNonZeroWidthArgs = all (isZeroBitTy . scaledThing) . dataConRepArgTys

mkUnarisedIds :: MonadUnique m => FastString -> [UnaryType] -> m [Id]
mkUnarisedIds fs tys = mapM (mkUnarisedId fs) tys

mkUnarisedId :: MonadUnique m => FastString -> UnaryType -> m Id
mkUnarisedId s t = mkSysLocalM s ManyTy t

-- Checks if id is a top level error application.
-- isErrorAp_maybe :: Id ->

-- | Extract the default case alternative
-- findDefaultStg :: [Alt b] -> ([Alt b], Maybe (Expr b))
findDefaultStg
  :: [GenStgAlt p]
  -> ([GenStgAlt p], Maybe (GenStgExpr p))
findDefaultStg (GenStgAlt{ alt_con    = DEFAULT
                         , alt_bndrs  = args
                         , alt_rhs    = rhs} : alts) = assert( null args ) (alts, Just rhs)
findDefaultStg alts                                  = (alts, Nothing)

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
            | otherwise          -> assertPpr ( _is_poly_alt_tycon tc) (ppr tc)
                                    PolyAlt
          Nothing                -> PolyAlt
      [non_gcd] -> PrimAlt non_gcd
      not_unary -> MultiValAlt (length not_unary)
  where
   bndr_ty   = idType bndr
   prim_reps = typePrimRep bndr_ty

   _is_poly_alt_tycon tc
        =  isPrimTyCon tc   -- "Any" is lifted but primitive
        || isFamilyTyCon tc -- Type family; e.g. Any, or arising from strict
                            -- function application where argument has a
                            -- type-family type

   -- Sometimes, the TyCon is a AbstractTyCon which may not have any
   -- constructors inside it.  Then we may get a better TyCon by
   -- grabbing the one from a constructor alternative
   -- if one exists.
   look_for_better_tycon
        | (DataAlt con : _) <- alt_con <$> data_alts =
                AlgAlt (dataConTyCon con)
        | otherwise =
                assert(null data_alts)
                PolyAlt
        where
                (data_alts, _deflt) = findDefaultStg alts

bindersOf :: BinderP a ~ Id => GenStgBinding a -> [Id]
bindersOf (StgNonRec binder _) = [binder]
bindersOf (StgRec pairs)       = [binder | (binder, _) <- pairs]

bindersOfX :: GenStgBinding a -> [BinderP a]
bindersOfX (StgNonRec binder _) = [binder]
bindersOfX (StgRec pairs)       = [binder | (binder, _) <- pairs]

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
