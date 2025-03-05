{-# LANGUAGE GADTs #-}

{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1996-1998
-}

-- | Final zonking to 'Type'. See Note [Zonking to Type].
--
-- Distinct from the intra-typechecker zonking in "GHC.Tc.Zonk.TcType";
-- see Note [Module structure for zonking].
module GHC.Tc.Zonk.Type (
        -- * Zonking
        -- | For a description of "zonking", see Note [What is zonking?].
        ZonkTcM,
        zonkTopDecls, zonkTopExpr, zonkTopLExpr,
        zonkTopBndrs,
        zonkTyVarBindersX, zonkTyVarBinderX,
        zonkTyBndrX, zonkTyBndrsX,
        zonkTcTypeToType,  zonkTcTypeToTypeX,
        zonkTcTypesToTypesX, zonkScaledTcTypesToTypesX,
        zonkTyVarOcc,
        zonkCoToCo,
        zonkEvBinds, zonkTcEvBinds,
        zonkTcMethInfoToMethInfoX,
        lookupTyVarX,

        -- ** 'ZonkEnv', and the 'ZonkT' and 'ZonkBndrT' monad transformers
        module GHC.Tc.Zonk.Env,

        -- * Coercion holes
        isFilledCoercionHole, unpackCoercionHole, unpackCoercionHole_maybe,

        -- * Rewriter sets
        zonkRewriterSet, zonkCtRewriterSet, zonkCtEvRewriterSet,

        -- * Tidying
        tcInitTidyEnv, tcInitOpenTidyEnv,


  ) where

import GHC.Prelude

import GHC.Builtin.Types

import GHC.Core.TyCo.Ppr ( pprTyVar )

import GHC.Hs

import {-# SOURCE #-} GHC.Tc.Gen.Splice (runTopSplice)
import GHC.Tc.Types ( TcM )
import GHC.Tc.Types.TcRef
import GHC.Tc.TyCl.Build ( TcMethInfo, MethInfo )
import GHC.Tc.Utils.Env ( tcLookupGlobalOnly )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.Monad ( newZonkAnyType, setSrcSpanA, liftZonkM, traceTc, addErr )
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Evidence
import GHC.Tc.Errors.Types
import GHC.Tc.Zonk.Env
-- Very little shared code between GHC.Tc.Zonk.TcType and GHC.Tc.Zonk.Type.
-- See Note [Module structure for zonking]
import GHC.Tc.Zonk.TcType
    ( tcInitTidyEnv, tcInitOpenTidyEnv
    , writeMetaTyVarRef
    , checkCoercionHole
    , zonkCoVar )

import GHC.Core.Type
import GHC.Core.Coercion
import GHC.Core.TyCon

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Monad
import GHC.Utils.Panic

import GHC.Core.Multiplicity
import GHC.Core
import GHC.Core.Predicate

import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Id
import GHC.Types.TypeEnv
import GHC.Types.Basic
import GHC.Types.SrcLoc
import GHC.Types.Unique.Set
import GHC.Types.Unique.FM
import GHC.Types.TyThing

import GHC.Tc.Types.BasicTypes

import GHC.Data.Maybe
import GHC.Data.Bag

import Control.Monad
import Control.Monad.Trans.Class ( lift )
import Data.Semigroup
import Data.List.NonEmpty ( NonEmpty )
import Data.Foldable ( toList )

{- Note [What is zonking?]
~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC relies heavily on mutability in the typechecker for efficient operation.
For this reason, throughout much of the type checking process, meta type
variables (the MetaTv constructor of TcTyVarDetails) are represented by mutable
variables (known as TcRefs).

Zonking is the process of replacing each such mutable variable with a Type.
This involves traversing the entire type expression, but the interesting part,
replacing the mutable variables, occurs in zonkTyVarOcc.

There are two ways to zonk a Type, using one of two entirely separate zonkers,
that share essentially no code:

*  GHC.Tc.Zonk.TcType.zonkTcType, which is used /during/ type checking:
   * It leaves unfilled metavars untouched, so the resulting Type can contain TcTyVars
   * It is only defined for Type and Coercion, not for HsExpr
   * It works in a very stripped-down monad, ZonkM, make it clear that it uses
     very few effects (for example, it can't throw errors).

* GHC.Tc.Zonk.Type.zonkTcTypeToType, is used /after/ typechecking is complete:
  * It always returns a Type with no remaining TcTyVars; no meta-tyvars remain.
  * It does defaulting, replacing an unconstrained TcTyVar with Any, or failing
     (determined by the ZonkFlexi parameter used; see GHC.Tc.Zonk.Type.commitFlexi).
  * It works over HsExpr and HsBinds as well as Type and Coercion. As part of this,
    it also removes the mutable variables in evidence bindings.
  * It works in the full TcM monad, augmented with an environment.
    More precisely, it uses ZonkTcM and ZonkBndrTcM, which augment TcM with a
    ZonkEnv environment using the zonking monad transformers ZonkT and ZonkBndrT
    (see Note [The ZonkEnv] in GHC.Tc.Zonk.Env).

    Why TcM rather than a smaller monad? See Note [Using TcM for zonking to Type].

Note [Module structure for zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As remarked in Note [What is zonking?], there are really two different zonkers;
we have GHC.Tc.Zonk.TcType for zonking within the typechecker and
GHC.Tc.Zonk.Type for the final zonking pass.

The code relating to zonking is thus split up across the following modules:

  I. Zonking within the typechecker
    1. GHC.Tc.Zonk.Monad
    2. GHC.Tc.Zonk.TcType

  II. Final zonking to Type
    1. GHC.Tc.Zonk.Env
    2. GHC.Tc.Zonk.Type

I.1. GHC.Tc.Zonk.Monad - the ZonkM monad

  GHC.Tc.Zonk.Monad defines the ZonkM monad, which is a stripped down version
  of TcM which has just enough information to be able to zonk types.

  This is the monad used for zonking inside the typechecker,
  as used in GHC.Tc.Zonk.TcType.

  Crucially, it never errors. It is the monad we use when reporting errors
  (see ErrCtxt), and it would be quite bad if we could error in the middle
  of reporting an error!

I.2. GHC.Tc.Zonk.TcType - zonking types in the typechecker

  GHC.Tc.Zonk.TcType contains code for zonking types and constraints, for use
  within the typechecker. It uses the ZonkM monad.
  For example, it defines:

    zonkTcType :: TcType -> ZonkM TcType
    zonkCt     :: Ct     -> ZonkM Ct

II.1. GHC.Tc.Zonk.Env - the ZonkEnv and ZonkT/ZonkBndrT monad transformers

   GHC.Tc.Zonk.Env defines the the ZonkT and ZonkBndrT monad transformers.
   These are essentially "ReaderT ZonkEnv" and "StateT ZonkEnv", except
   that ZonkBndrT use continuation-passing style instead of an explicit state.
   See Note [The ZonkEnv] in GHC.Tc.Zonk.Env.

   These are used for the final zonking to type, in GHC.Tc.Zonk.Type.

II.2. GHC.Tc.Zonk.Type - final zonking to type

  GHC.Tc.Zonk.Type is concerned with the "final zonking" pass, after we finish
  typechecking. It zonks not only types, but terms. It uses the monads

    type ZonkTcM     = ZonkT     TcM
    type ZonkBndrTcM = ZonkBndrTcM

  for example:

    zonkTyBndrX       :: TcTyVar  -> ZonkBndrTcM TyVar
    zonkTcTypeToTypeX :: TcType   -> ZonkT     TcM Type

Note that ZonkTcM does a lot more things than ZonkM:

  - it uses a separate ZonkEnv state to accumulate zonked type
      (see Note [The ZonkEnv] in GHC.Tc.Zonk.Env)
  - it defaults type variables,
      (see Note [Un-unified unification variables] in GHC.Tc.Zonk.Env)
  - turns TcTyVars into TyVars,
  - ...

This means that there is essentially no code shared between "GHC.Tc.Zonk.TcType"
and "GHC.Tc.Zonk.Type'; they're really two different zonkers.

Note [Zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~
Zonking to Type is a final zonking pass done *after* typechecking.
It runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void
 c) convert each TcId to an Id by zonking its type

The type variables are converted by binding mutable tyvars to immutable ones
and then zonking as normal.

The Ids are converted by binding them in the normal Tc envt; that
way we maintain sharing; eg an Id is zonked at its binding site and they
all occurrences of that Id point to the common zonked copy

It's all pretty boring stuff, because HsSyn is such a large type, and
the environment manipulation is tiresome.

Note [Sharing when zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Problem:

    In GHC.Tc.Zonk.TcType.zonkTcTyVar, we short-circuit (Indirect ty) to
    (Indirect zty), see Note [Sharing in zonking] in GHC.Tc.Zonk.TcType.
    But we /can't/ do this when zonking a TcType to a Type (#15552, esp comment:3).
    Suppose we have

       alpha -> alpha
         where
            alpha is already unified:
             alpha := T{tc-tycon} Int -> Int
         and T is knot-tied

    By "knot-tied" I mean that the occurrence of T is currently a TcTyCon,
    but the global env contains a mapping "T" :-> T{knot-tied-tc}. See
    Note [Type checking recursive type and class declarations] in
    GHC.Tc.TyCl.

    Now we call zonkTcTypeToType on that (alpha -> alpha). If we follow
    the same path as Note [Sharing in zonking] in GHC.Tc.Zonk.TcType, we'll
    update alpha to
       alpha := T{knot-tied-tc} Int -> Int

    But alas, if we encounter alpha for a /second/ time, we end up
    looking at T{knot-tied-tc} and fall into a black hole. The whole
    point of zonkTcTypeToType is that it produces a type full of
    knot-tied tycons, and you must not look at the result!!

    To put it another way (zonkTcTypeToType . zonkTcTypeToType) is not
    the same as zonkTcTypeToType. (If we distinguished TcType from
    Type, this issue would have been a type error!)

Solutions: (see #15552 for other variants)

One possible solution is simply not to do the short-circuiting.
That has less sharing, but maybe sharing is rare. And indeed,
that usually turns out to be viable from a perf point of view

But zonkTyVarOcc implements something a bit better

* ZonkEnv contains ze_meta_tv_env, which maps
      from a MetaTyVar (unification variable)
      to a Type (not a TcType)

* In zonkTyVarOcc, we check this map to see if we have zonked
  this variable before. If so, use the previous answer; if not
  zonk it, and extend the map.

* The map is of course stateful, held in a TcRef. (That is unlike
  the treatment of lexically-scoped variables in ze_tv_env and
  ze_id_env.)

* In zonkTyVarOcc we read the TcRef to look up the unification
  variable:
    - if we get a hit we use the zonked result;
    - if not, in zonk_meta we see if the variable is `Indirect ty`,
      zonk that, and update the map (in finish_meta)
  But Nota Bene that the "update map" step must re-read the TcRef
  (or, more precisely, use updTcRef) because the zonking of the
  `Indirect ty` may have added lots of stuff to the map.  See
  #19668 for an example where this made an asymptotic difference!

Is it worth the extra work of carrying ze_meta_tv_env? Some
non-systematic perf measurements suggest that compiler allocation is
reduced overall (by 0.5% or so) but compile time really doesn't
change.  But in some cases it makes a HUGE difference: see test
T9198 and #19668.  So yes, it seems worth it.

Note [Using TcM for zonking to Type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The main zonking monads currently wrap TcM, because we need access to
the full TcM monad in order to expand typed TH splices.
See zonkExpr (HsTypedSplice s _) = ...

After the Typed TH plan has been implemented, this should no longer be necessary,
and we should be able to use a stripped down monad, similar to the ZonkM monad
which we use for zonking within the typechecker (but we will need a place to
accumulate errors).

Note [Inlining ZonkBndrT computations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Computations that use the ZonkBndrT monad transformer must be inlined:
ZonkBndrT uses continuation-passing style; failing to inline means applying
an unknown continuation (unknown function call), which prevents many
optimisations from taking place.

See test cases T14683, which regresses without these changes.
-}

-- Why do we use TcM below? See Note [Using TcM for zonking to Type]

-- | Zonking monad for a computation that zonks to Type, reading from a 'ZonkEnv'
-- but not extending or modifying it.
--
-- See Note [Zonking to Type].
type ZonkTcM = ZonkT TcM

-- | Zonking monad for a computation that zonks to Type, reading from
-- and extending or modifying a 'ZonkEnv'.
--
-- See Note [Zonking to Type].
type ZonkBndrTcM = ZonkBndrT TcM

wrapLocZonkMA :: (a -> ZonkTcM b) -> GenLocated (EpAnn ann) a
              -> ZonkTcM (GenLocated (EpAnn ann) b)
wrapLocZonkMA fn (L loc a) = ZonkT $ \ ze ->
  setSrcSpanA loc $
  do { b <- runZonkT (fn a) ze
     ; return (L loc b) }

wrapLocZonkBndrMA :: (a -> ZonkBndrTcM b) -> GenLocated (EpAnn ann) a
                  -> ZonkBndrTcM (GenLocated (EpAnn ann) b)
wrapLocZonkBndrMA fn (L loc a) = ZonkBndrT $ \ k -> ZonkT $ \ ze ->
  setSrcSpanA loc $
  runZonkT ( runZonkBndrT (fn a) $ \ b -> k (L loc b) ) ze

--------------------------------------------------------------------------------

zonkTyBndrsX :: [TcTyVar] -> ZonkBndrTcM [TcTyVar]
zonkTyBndrsX = traverse zonkTyBndrX
{-# INLINE zonkTyBndrsX #-} -- See Note [Inlining ZonkBndrT computations]

zonkTyBndrX :: TcTyVar -> ZonkBndrTcM TyVar
-- This guarantees to return a TyVar (not a TcTyVar)
-- then we add it to the envt, so all occurrences are replaced
--
-- It does not clone: the new TyVar has the sane Name
-- as the old one.  This important when zonking the
-- TyVarBndrs of a TyCon, whose Names may scope.
zonkTyBndrX tv
  = assertPpr (isImmutableTyVar tv) (ppr tv <+> dcolon <+> ppr (tyVarKind tv)) $
    do { ki <- noBinders $ zonkTcTypeToTypeX (tyVarKind tv)
               -- Internal names tidy up better, for iface files.
       ; let tv' = mkTyVar (tyVarName tv) ki
       ; extendTyZonkEnv tv'
       ; return tv' }
{-# INLINE zonkTyBndrX #-} -- See Note [Inlining ZonkBndrT computations]

zonkTyVarBindersX :: [VarBndr TcTyVar vis]
                  -> ZonkBndrTcM [VarBndr TyVar vis]
zonkTyVarBindersX = traverse zonkTyVarBinderX
{-# INLINE zonkTyVarBindersX #-} -- See Note [Inlining ZonkBndrT computations]

zonkTyVarBinderX :: VarBndr TcTyVar vis
                 -> ZonkBndrTcM (VarBndr TyVar vis)
-- Takes a TcTyVar and guarantees to return a TyVar
zonkTyVarBinderX (Bndr tv vis)
  = do { tv' <- zonkTyBndrX tv
       ; return (Bndr tv' vis) }
{-# INLINE zonkTyVarBinderX #-} -- See Note [Inlining ZonkBndrT computations]

zonkTyVarOcc :: HasDebugCallStack => TcTyVar -> ZonkTcM Type
zonkTyVarOcc tv
  = do { ZonkEnv { ze_tv_env = tv_env } <- getZonkEnv

       ; let lookup_in_tv_env    -- Look up in the env just as we do for Ids
               = case lookupVarEnv tv_env tv of
                   Nothing  -> -- TyVar/SkolemTv/RuntimeUnk that isn't in the ZonkEnv
                               -- This can happen for RuntimeUnk variables (which
                               -- should stay as RuntimeUnk), but I think it should
                               -- not happen for SkolemTv.
                               mkTyVarTy <$> updateTyVarKindM zonkTcTypeToTypeX tv

                   Just tv' -> return (mkTyVarTy tv')

             zonk_meta ref Flexi
               = do { kind <- zonkTcTypeToTypeX (tyVarKind tv)
                    ; ty <- commitFlexi tv kind

                    ; lift $ liftZonkM $ writeMetaTyVarRef tv ref ty  -- Belt and braces
                    ; finish_meta ty }

             zonk_meta _ (Indirect ty)
               = do { zty <- zonkTcTypeToTypeX ty
                    ; finish_meta zty }

             finish_meta ty
               = do { extendMetaEnv tv ty
                    ; return ty }

       ; if isTcTyVar tv
         then case tcTyVarDetails tv of
           SkolemTv {}    -> lookup_in_tv_env
           RuntimeUnk {}  -> lookup_in_tv_env
           MetaTv { mtv_ref = ref }
             -> do { mb_ty <- lookupMetaTv tv
                     -- See Note [Sharing when zonking to Type]
                   ; case mb_ty of
                       Just ty -> return ty
                       Nothing -> do { mtv_details <- readTcRef ref
                                     ; zonk_meta ref mtv_details } }

         -- This should never really happen;
         -- TyVars should not occur in the typechecker
         else lookup_in_tv_env }

extendMetaEnv :: TcTyVar -> Type -> ZonkTcM ()
extendMetaEnv tv ty =
  ZonkT $ \ ( ZonkEnv { ze_meta_tv_env = mtv_env_ref } ) ->
    updTcRef mtv_env_ref (\env -> extendVarEnv env tv ty)

lookupMetaTv :: TcTyVar -> ZonkTcM (Maybe Type)
lookupMetaTv tv =
  ZonkT $ \ ( ZonkEnv { ze_meta_tv_env = mtv_env_ref } ) ->
    do { mtv_env <- readTcRef mtv_env_ref
       ; return $ lookupVarEnv mtv_env tv }

lookupTyVarX :: TcTyVar -> ZonkTcM TyVar
lookupTyVarX tv
  = do { ZonkEnv { ze_tv_env = tv_env } <- getZonkEnv
       ; let !res = case lookupVarEnv tv_env tv of
                      Just tv -> tv
                      Nothing -> pprPanic "lookupTyVarOcc" (ppr tv $$ ppr tv_env)
       ; return res }

commitFlexi :: TcTyVar -> Kind -> ZonkTcM Type
commitFlexi tv zonked_kind
  = do { flexi <- ze_flexi <$> getZonkEnv
       ; lift $ case flexi of
         SkolemiseFlexi  -> return (mkTyVarTy (mkTyVar name zonked_kind))

         DefaultFlexi
             -- Normally, RuntimeRep variables are defaulted in GHC.Tc.Utils.TcMType.defaultTyVar
             -- But that sees only type variables that appear in, say, an inferred type.
             -- Defaulting here, in the zonker, is needed to catch e.g.
             --    y :: Bool
             --    y = (\x -> True) undefined
             -- We need *some* known RuntimeRep for the x and undefined, but no one
             -- will choose it until we get here, in the zonker.
           | isRuntimeRepTy zonked_kind
           -> do { traceTc "Defaulting flexi tyvar to LiftedRep:" (pprTyVar tv)
                 ; return liftedRepTy }
           | isLevityTy zonked_kind
           -> do { traceTc "Defaulting flexi tyvar to Lifted:" (pprTyVar tv)
                 ; return liftedDataConTy }
           | isMultiplicityTy zonked_kind
           -> do { traceTc "Defaulting flexi tyvar to Many:" (pprTyVar tv)
                 ; return manyDataConTy }
           | Just (ConcreteFRR origin) <- isConcreteTyVar_maybe tv
           -> do { addErr $ TcRnZonkerMessage (ZonkerCannotDefaultConcrete origin)
                 ; return (anyTypeOfKind zonked_kind) }
           | otherwise
           -> do { traceTc "Defaulting flexi tyvar to ZonkAny:" (pprTyVar tv)
                   -- See Note [Any types] in GHC.Builtin.Types, esp wrinkle (Any4)
                 ; newZonkAnyType zonked_kind }

         RuntimeUnkFlexi
           -> do { traceTc "Defaulting flexi tyvar to RuntimeUnk:" (pprTyVar tv)
                 ; return (mkTyVarTy (mkTcTyVar name zonked_kind RuntimeUnk)) }
                           -- This is where RuntimeUnks are born:
                           -- otherwise-unconstrained unification variables are
                           -- turned into RuntimeUnks as they leave the
                           -- typechecker's monad

         NoFlexi -> pprPanic "NoFlexi" (ppr tv <+> dcolon <+> ppr zonked_kind) }

  where
     name = tyVarName tv


zonkCoVarOcc :: CoVar -> ZonkTcM Coercion
zonkCoVarOcc cv
  = do { ZonkEnv { ze_tv_env = tyco_env } <- getZonkEnv
         -- don't look in the knot-tied env
       ; case lookupVarEnv tyco_env cv of
          Just cv' -> return $ mkCoVarCo cv'
          _        -> mkCoVarCo <$> (lift $ liftZonkM $ zonkCoVar cv) }

zonkCoHole :: CoercionHole -> ZonkTcM Coercion
zonkCoHole hole@(CoercionHole { ch_ref = ref, ch_co_var = cv })
  = do { contents <- readTcRef ref
       ; case contents of
           Just co -> do { co' <- zonkCoToCo co
                         ; lift $ liftZonkM $ checkCoercionHole cv co' }

              -- This next case should happen only in the presence of
              -- (undeferred) type errors. Originally, I put in a panic
              -- here, but that caused too many uses of `failIfErrsM`.
           Nothing -> do { lift $ traceTc "Zonking unfilled coercion hole" (ppr hole)
                         ; cv' <- lift $ liftZonkM $ zonkCoVar cv
                         ; return $ mkCoVarCo cv' } }
                             -- This will be an out-of-scope variable, but keeping
                             -- this as a coercion hole led to #15787

zonk_tycomapper :: TyCoMapper ZonkEnv TcM
zonk_tycomapper = TyCoMapper
  { tcm_tyvar      = \ env tv -> runZonkT (zonkTyVarOcc tv) env
  , tcm_covar      = \ env cv -> runZonkT (zonkCoVarOcc cv) env
  , tcm_hole       = \ env co -> runZonkT (zonkCoHole   co) env
  , tcm_tycobinder = \ env tcv _vis k -> flip runZonkT env $
                     runZonkBndrT (zonkTyBndrX tcv) $
                     \ tcv' -> ZonkT $ \ env' -> (k env' tcv')
  , tcm_tycon      = \ tc -> zonkTcTyConToTyCon tc
  }

-- Zonk a TyCon by changing a TcTyCon to a regular TyCon
zonkTcTyConToTyCon :: TcTyCon -> TcM TyCon
zonkTcTyConToTyCon tc
  | isTcTyCon tc = do { thing <- tcLookupGlobalOnly (getName tc)
                      ; case thing of
                          ATyCon real_tc -> return real_tc
                          _              -> pprPanic "zonkTcTyCon" (ppr tc $$ ppr thing) }
  | otherwise    = return tc -- it's already zonked

-- | Confused by zonking? See Note [What is zonking?] in "GHC.Tc.Zonk.Type".
zonkTcTypeToType :: TcType -> TcM Type
zonkTcTypeToType ty = initZonkEnv DefaultFlexi $ zonkTcTypeToTypeX ty

zonkScaledTcTypeToTypeX :: Scaled TcType -> ZonkTcM (Scaled TcType)
zonkScaledTcTypeToTypeX (Scaled m ty) = Scaled <$> zonkTcTypeToTypeX m
                                               <*> zonkTcTypeToTypeX ty

zonkTcTypeToTypeX   :: TcType   -> ZonkTcM Type
zonkTcTypesToTypesX :: [TcType] -> ZonkTcM [Type]
zonkCoToCo          :: Coercion -> ZonkTcM Coercion
(zonkTcTypeToTypeX, zonkTcTypesToTypesX, zonkCoToCo)
  = case mapTyCoX zonk_tycomapper of
      (zty, ztys, zco, _) ->
        (ZonkT . flip zty, ZonkT . flip ztys, ZonkT . flip zco)

zonkScaledTcTypesToTypesX :: [Scaled TcType] -> ZonkTcM [Scaled Type]
zonkScaledTcTypesToTypesX scaled_tys =
   mapM zonkScaledTcTypeToTypeX scaled_tys


zonkEnvIds :: ZonkEnv -> TypeEnv
zonkEnvIds (ZonkEnv { ze_id_env = id_env })
  = mkNameEnv [(getName id, AnId id) | id <- nonDetEltsUFM id_env]
  -- It's OK to use nonDetEltsUFM here because we forget the ordering
  -- immediately by creating a TypeEnv

zonkLIdOcc :: LocatedN TcId -> ZonkTcM (LocatedN Id)
zonkLIdOcc = traverse zonkIdOcc

zonkIdOcc :: TcId -> ZonkTcM Id
-- Ids defined in this module should be in the envt;
-- ignore others.  (Actually, data constructors are also
-- not LocalVars, even when locally defined, but that is fine.)
-- (Also foreign-imported things aren't currently in the ZonkEnv;
--  that's ok because they don't need zonking.)
--
-- Actually, Template Haskell works in 'chunks' of declarations, and
-- an earlier chunk won't be in the 'env' that the zonking phase
-- carries around.  Instead it'll be in the tcg_gbl_env, already fully
-- zonked.  There's no point in looking it up there (except for error
-- checking), and it's not conveniently to hand; hence the simple
-- 'orElse' case in the LocalVar branch.
--
-- Even without template splices, in module Main, the checking of
-- 'main' is done as a separate chunk.
zonkIdOcc id
  | isLocalVar id =
    do { ZonkEnv { ze_id_env = id_env } <- getZonkEnv
       ; return $ lookupVarEnv id_env id `orElse` id }
  | otherwise
  = return id

zonkIdOccs :: [TcId] -> ZonkTcM [Id]
zonkIdOccs ids = traverse zonkIdOcc ids

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give
zonkIdBndrX :: TcId -> ZonkBndrTcM Id
zonkIdBndrX v
  = do { id <- noBinders $ zonkIdBndr v
       ; extendIdZonkEnv id
       ; return id }
{-# INLINE zonkIdBndrX #-} -- See Note [Inlining ZonkBndrT computations]

zonkIdBndr :: TcId -> ZonkTcM Id
zonkIdBndr v
  = do { Scaled w' ty' <- zonkScaledTcTypeToTypeX (idScaledType v)
       ; return $ setIdMult (setIdType v ty') w' }

zonkIdBndrs :: [TcId] -> ZonkTcM [Id]
zonkIdBndrs ids = mapM zonkIdBndr ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = initZonkEnv DefaultFlexi $ zonkIdBndrs ids

zonkFieldOcc :: FieldOcc GhcTc -> ZonkTcM (FieldOcc GhcTc)
zonkFieldOcc (FieldOcc lbl (L l sel))
  = FieldOcc lbl . L l <$> zonkIdBndr sel

zonkEvBndrsX :: [EvVar] -> ZonkBndrTcM [EvVar]
zonkEvBndrsX = traverse zonkEvBndrX
{-# INLINE zonkEvBndrsX #-} -- See Note [Inlining ZonkBndrT computations]

zonkEvBndrX :: EvVar -> ZonkBndrTcM EvVar
-- Works for dictionaries and coercions
zonkEvBndrX var
  = do { var' <- noBinders $ zonkEvBndr var
       ; extendZonkEnv [var']
       ; return var' }
{-# INLINE zonkEvBndr #-} -- See Note [Inlining ZonkBndrT computations]

zonkEvBndr :: EvVar -> ZonkTcM EvVar
-- Works for dictionaries and coercions
-- Does not extend the ZonkEnv
zonkEvBndr var
  = updateIdTypeAndMultM ({-# SCC "zonkEvBndr_zonkTcTypeToType" #-} zonkTcTypeToTypeX) var

{-
zonkEvVarOcc :: EvVar -> ZonkTcM EvTerm
zonkEvVarOcc env v
  | isCoVar v
  = EvCoercion <$> zonkCoVarOcc env v
  | otherwise
  = return (EvId $ zonkIdOcc env v)
-}

zonkCoreBndrX :: Var -> ZonkBndrTcM Var
zonkCoreBndrX v
  | isId v    = zonkIdBndrX v
  | otherwise = zonkTyBndrX v
{-# INLINE zonkCoreBndrX #-} -- See Note [Inlining ZonkBndrT computations]

zonkCoreBndrsX :: [Var] -> ZonkBndrTcM [Var]
zonkCoreBndrsX = traverse zonkCoreBndrX
{-# INLINE zonkCoreBndrsX #-} -- See Note [Inlining ZonkBndrT computations]

zonkTopExpr :: HsExpr GhcTc -> TcM (HsExpr GhcTc)
zonkTopExpr e = initZonkEnv DefaultFlexi $ zonkExpr e

zonkTopLExpr :: LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
zonkTopLExpr e = initZonkEnv DefaultFlexi $ zonkLExpr e

zonkTopDecls :: Bag EvBind
             -> LHsBinds GhcTc
             -> [LRuleDecl GhcTc] -> [LTcSpecPrag]
             -> [LForeignDecl GhcTc]
             -> TcM (TypeEnv,
                     Bag EvBind,
                     LHsBinds GhcTc,
                     [LForeignDecl GhcTc],
                     [LTcSpecPrag],
                     [LRuleDecl    GhcTc])
zonkTopDecls ev_binds binds rules imp_specs fords
  = initZonkEnv DefaultFlexi $
    runZonkBndrT (zonkEvBinds ev_binds)   $ \ ev_binds' ->
    runZonkBndrT (zonkRecMonoBinds binds) $ \ binds'    ->
     -- Top level is implicitly recursive
  do  { rules' <- zonkRules rules
      ; specs' <- zonkLTcSpecPrags imp_specs
      ; fords' <- zonkForeignExports fords
      ; ty_env <- zonkEnvIds <$> getZonkEnv
      ; return (ty_env, ev_binds', binds', fords', specs', rules') }


---------------------------------------------
zonkLocalBinds :: HsLocalBinds GhcTc
               -> ZonkBndrTcM (HsLocalBinds GhcTc)
zonkLocalBinds (EmptyLocalBinds x)
  = return (EmptyLocalBinds x)

zonkLocalBinds (HsValBinds _ (ValBinds {}))
  = panic "zonkLocalBinds" -- Not in typechecker output

zonkLocalBinds (HsValBinds x (XValBindsLR (NValBinds binds sigs)))
  = do  { new_binds <- traverse go binds
        ; return (HsValBinds x (XValBindsLR (NValBinds new_binds sigs))) }
  where
    go (r,b)
      = do { b' <- zonkRecMonoBinds b
           ; return (r,b') }

zonkLocalBinds (HsIPBinds x (IPBinds dict_binds binds )) = do
    new_binds <- noBinders $ mapM (wrapLocZonkMA zonk_ip_bind) binds
    extendIdZonkEnvRec [ n | (L _ (IPBind n _ _)) <- new_binds]
    new_dict_binds <- zonkTcEvBinds dict_binds
    return $ HsIPBinds x (IPBinds new_dict_binds new_binds)
  where
    zonk_ip_bind (IPBind dict_id n e)
        = do dict_id' <- zonkIdBndr dict_id
             e'       <- zonkLExpr e
             return (IPBind dict_id' n e')

---------------------------------------------
zonkRecMonoBinds :: LHsBinds GhcTc -> ZonkBndrTcM (LHsBinds GhcTc)
zonkRecMonoBinds binds
  = mfix $ \ new_binds ->
  do { extendIdZonkEnvRec (collectHsBindsBinders CollNoDictBinders new_binds)
     ; noBinders $ zonkMonoBinds binds }

---------------------------------------------
zonkMonoBinds :: LHsBinds GhcTc -> ZonkTcM (LHsBinds GhcTc)
zonkMonoBinds binds = mapM zonk_lbind binds

zonk_lbind :: LHsBind GhcTc -> ZonkTcM (LHsBind GhcTc)
zonk_lbind = wrapLocZonkMA zonk_bind

zonk_bind :: HsBind GhcTc -> ZonkTcM (HsBind GhcTc)
zonk_bind bind@(PatBind { pat_lhs = pat, pat_rhs = grhss
                        , pat_mult = mult_ann
                        , pat_ext = (ty, ticks)})
  = do  { new_pat   <- don'tBind $ zonkPat pat            -- Env already extended
        ; new_grhss <- zonkGRHSs zonkLExpr grhss
        ; new_ty    <- zonkTcTypeToTypeX ty
        ; new_mult  <- zonkMultAnn mult_ann
        ; return (bind { pat_lhs = new_pat, pat_rhs = new_grhss
                       , pat_mult = new_mult
                       , pat_ext = (new_ty, ticks) }) }

zonk_bind (VarBind { var_ext = x
                   , var_id = var, var_rhs = expr })
  = do { new_var  <- zonkIdBndr var
       ; new_expr <- zonkLExpr expr
       ; return (VarBind { var_ext = x
                         , var_id = new_var
                         , var_rhs = new_expr }) }

zonk_bind bind@(FunBind { fun_id = L loc var
                        , fun_matches = ms
                        , fun_ext = (co_fn, ticks) })
  = do { new_var <- zonkIdBndr var
       ; runZonkBndrT (zonkCoFn co_fn) $ \ new_co_fn ->
    do { new_ms <- zonkMatchGroup zonkLExpr ms
       ; return (bind { fun_id = L loc new_var
                      , fun_matches = new_ms
                      , fun_ext = (new_co_fn, ticks) }) } }

zonk_bind (XHsBindsLR (AbsBinds { abs_tvs = tyvars, abs_ev_vars = evs
                                , abs_ev_binds = ev_binds
                                , abs_exports = exports
                                , abs_binds = val_binds
                                , abs_sig = has_sig }))
  = assert ( all isImmutableTyVar tyvars ) $
    runZonkBndrT (zonkTyBndrsX    tyvars  ) $ \ new_tyvars   ->
    runZonkBndrT (zonkEvBndrsX    evs     ) $ \ new_evs      ->
    runZonkBndrT (zonkTcEvBinds_s ev_binds) $ \ new_ev_binds ->
  do { (new_val_bind, new_exports) <- mfix $ \ ~(new_val_binds, _) ->
       runZonkBndrT (extendIdZonkEnvRec $ collectHsBindsBinders CollNoDictBinders new_val_binds) $ \ _ ->
       do { new_val_binds <- mapM zonk_val_bind val_binds
          ; new_exports   <- mapM zonk_export exports
          ; return (new_val_binds, new_exports)
          }
     ; return $ XHsBindsLR $
                AbsBinds { abs_tvs = new_tyvars, abs_ev_vars = new_evs
                         , abs_ev_binds = new_ev_binds
                         , abs_exports = new_exports, abs_binds = new_val_bind
                         , abs_sig = has_sig } }
  where
    zonk_val_bind lbind
      | has_sig
      , (L loc bind@(FunBind { fun_id      = (L mloc mono_id)
                             , fun_matches = ms
                             , fun_ext     = (co_fn, ticks) })) <- lbind
      = do { new_mono_id <- updateIdTypeAndMultM zonkTcTypeToTypeX mono_id
                            -- Specifically /not/ zonkIdBndr; we do not want to
                            -- complain about a representation-polymorphic binder
           ; runZonkBndrT (zonkCoFn co_fn) $ \ new_co_fn ->
        do { new_ms            <- zonkMatchGroup zonkLExpr ms
           ; return $ L loc $
             bind { fun_id      = L mloc new_mono_id
                  , fun_matches = new_ms
                  , fun_ext     = (new_co_fn, ticks) } } }
      | otherwise
      = zonk_lbind lbind   -- The normal case

    zonk_export :: ABExport -> ZonkTcM ABExport
    zonk_export (ABE{ abe_wrap  = wrap
                    , abe_poly  = poly_id
                    , abe_mono  = mono_id
                    , abe_prags = prags })
        = do new_poly_id <- zonkIdBndr poly_id
             new_wrap    <- don'tBind $ zonkCoFn wrap
             new_prags   <- zonkSpecPrags prags
             new_mono_id <- zonkIdOcc mono_id
             return (ABE{ abe_wrap  = new_wrap
                        , abe_poly  = new_poly_id
                        , abe_mono  = new_mono_id
                        , abe_prags = new_prags })

zonk_bind (PatSynBind x bind@(PSB { psb_id   = L loc id
                                  , psb_args = details
                                  , psb_def  = lpat
                                  , psb_dir  = dir }))
  = do { id' <- zonkIdBndr id
       ; runZonkBndrT (zonkPat lpat) $ \ lpat' ->
    do { details' <- zonkPatSynDetails details
       ; dir'     <- zonkPatSynDir dir
       ; return $ PatSynBind x $
                  bind { psb_id   = L loc id'
                       , psb_args = details'
                       , psb_def  = lpat'
                       , psb_dir  = dir' } } }

zonkMultAnn :: HsMultAnn GhcTc -> ZonkTcM (HsMultAnn GhcTc)
zonkMultAnn (HsNoMultAnn mult)
  = do { mult' <- zonkTcTypeToTypeX mult
       ; return (HsNoMultAnn mult') }
zonkMultAnn (HsPct1Ann mult)
  = do { mult' <- zonkTcTypeToTypeX mult
       ; return (HsPct1Ann mult') }
zonkMultAnn (HsMultAnn mult hs_ty)
  = do { mult' <- zonkTcTypeToTypeX mult
       ; return (HsMultAnn mult' hs_ty) }

zonkPatSynDetails :: HsPatSynDetails GhcTc
                  -> ZonkTcM (HsPatSynDetails GhcTc)
zonkPatSynDetails (PrefixCon _ as)
  = PrefixCon noTypeArgs <$> traverse zonkLIdOcc as
zonkPatSynDetails (InfixCon a1 a2)
  = InfixCon <$> zonkLIdOcc a1 <*> zonkLIdOcc a2
zonkPatSynDetails (RecCon flds)
  = RecCon <$> mapM zonkPatSynField flds

zonkPatSynField :: RecordPatSynField GhcTc -> ZonkTcM (RecordPatSynField GhcTc)
zonkPatSynField (RecordPatSynField x y) =
  RecordPatSynField <$> zonkFieldOcc x <*> zonkLIdOcc y

zonkPatSynDir :: HsPatSynDir GhcTc
              -> ZonkTcM (HsPatSynDir GhcTc)
zonkPatSynDir Unidirectional             = return Unidirectional
zonkPatSynDir ImplicitBidirectional      = return ImplicitBidirectional
zonkPatSynDir (ExplicitBidirectional mg) = ExplicitBidirectional <$> zonkMatchGroup zonkLExpr mg

zonkSpecPrags :: TcSpecPrags -> ZonkTcM TcSpecPrags
zonkSpecPrags IsDefaultMethod = return IsDefaultMethod
zonkSpecPrags (SpecPrags ps)  = do { ps' <- zonkLTcSpecPrags ps
                                   ; return (SpecPrags ps') }

zonkLTcSpecPrags :: [LTcSpecPrag] -> ZonkTcM [LTcSpecPrag]
zonkLTcSpecPrags ps
  = mapM zonk_prag ps
  where
    zonk_prag (L loc (SpecPrag id co_fn inl))
        = do { co_fn' <- don'tBind $ zonkCoFn co_fn
             ; id' <- zonkIdOcc id
             ; return (L loc (SpecPrag id' co_fn' inl)) }

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
*                                                                      *
************************************************************************
-}

zonkMatchGroup :: Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ EpAnnCO
               => (LocatedA (body GhcTc) -> ZonkTcM (LocatedA (body GhcTc)))
               -> LMatchGroup GhcTc (LocatedA (body GhcTc))
               -> ZonkTcM (LMatchGroup GhcTc (LocatedA (body GhcTc)))
zonkMatchGroup zBody (L l (MG { mg_alts = ms
                              , mg_ext = MatchGroupTc arg_tys res_ty origin
                              }))
  = do  { ms' <- mapM (zonkMatch zBody) ms
        ; arg_tys' <- zonkScaledTcTypesToTypesX arg_tys
        ; res_ty'  <- zonkTcTypeToTypeX res_ty
        ; return (L l (MG { mg_alts = ms'
                          , mg_ext = MatchGroupTc arg_tys' res_ty' origin
                          })) }

zonkMatch :: Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ EpAnnCO
          => (LocatedA (body GhcTc) -> ZonkTcM (LocatedA (body GhcTc)))
          -> LMatch GhcTc (LocatedA (body GhcTc))
          -> ZonkTcM (LMatch GhcTc (LocatedA (body GhcTc)))
zonkMatch zBody (L loc match@(Match { m_pats = L l pats
                                    , m_grhss = grhss }))
  = runZonkBndrT (zonkPats pats) $ \ new_pats ->
  do  { new_grhss <- zonkGRHSs zBody grhss
      ; return (L loc (match { m_pats = L l new_pats, m_grhss = new_grhss })) }

-------------------------------------------------------------------------
zonkGRHSs :: Anno (GRHS GhcTc (LocatedA (body GhcTc))) ~ EpAnnCO
          => (LocatedA (body GhcTc) -> ZonkTcM (LocatedA (body GhcTc)))
          -> GRHSs GhcTc (LocatedA (body GhcTc))
          -> ZonkTcM (GRHSs GhcTc (LocatedA (body GhcTc)))

zonkGRHSs zBody (GRHSs x grhss binds) =
  runZonkBndrT (zonkLocalBinds binds) $ \ new_binds ->
    do { new_grhss <- mapM (wrapLocZonkMA zonk_grhs) grhss
       ; return (GRHSs x new_grhss new_binds) }
  where
     zonk_grhs (GRHS xx guarded rhs) =
       runZonkBndrT (zonkStmts zonkLExpr guarded) $ \ new_guarded ->
         GRHS xx new_guarded <$> zBody rhs

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
*                                                                      *
************************************************************************
-}

zonkLExprs :: [LHsExpr GhcTc] -> ZonkTcM [LHsExpr GhcTc]
zonkLExpr  :: LHsExpr GhcTc   -> ZonkTcM (LHsExpr GhcTc)
zonkExpr   :: HsExpr GhcTc    -> ZonkTcM (HsExpr GhcTc)

zonkLExprs exprs = mapM zonkLExpr exprs
zonkLExpr  expr  = wrapLocZonkMA zonkExpr expr

zonkExpr (HsVar x (L l id))
  = assertPpr (isNothing (isDataConId_maybe id)) (ppr id) $
  do { id' <- zonkIdOcc id
     ; return (HsVar x (L l id')) }

zonkExpr (HsUnboundVar her occ)
  = do her' <- zonk_her her
       return (HsUnboundVar her' occ)
  where
    zonk_her :: HoleExprRef -> ZonkTcM HoleExprRef
    zonk_her (HER ref ty u)
      = do updTcRefM ref zonkEvTerm
           ty'  <- zonkTcTypeToTypeX ty
           return (HER ref ty' u)


zonkExpr (HsIPVar x _) = dataConCantHappen x

zonkExpr (HsOverLabel x _) = dataConCantHappen x

zonkExpr (HsLit x (XLit (HsRat f ty)))
  = do new_ty <- zonkTcTypeToTypeX ty
       return (HsLit x (XLit $ HsRat f new_ty))

zonkExpr (HsLit x lit)
  = return (HsLit x lit)

zonkExpr (HsOverLit x lit)
  = do  { lit' <- zonkOverLit lit
        ; return (HsOverLit x lit') }

zonkExpr (HsLam x lam_variant matches)
  = do new_matches <- zonkMatchGroup zonkLExpr matches
       return (HsLam x lam_variant new_matches)

zonkExpr (HsApp x e1 e2)
  = do new_e1 <- zonkLExpr e1
       new_e2 <- zonkLExpr e2
       return (HsApp x new_e1 new_e2)

zonkExpr (HsAppType ty e t)
  = do new_e <- zonkLExpr e
       new_ty <- zonkTcTypeToTypeX ty
       return (HsAppType new_ty new_e t)
       -- NB: the type is an HsType; can't zonk that!

zonkExpr (HsTypedBracket hsb_tc body)
  = (\x -> HsTypedBracket x body) <$> zonkBracket hsb_tc

zonkExpr (HsUntypedBracket hsb_tc body)
  = (\x -> HsUntypedBracket x body) <$> zonkBracket hsb_tc

zonkExpr (HsTypedSplice s _) = ZonkT (\ _ -> runTopSplice s) >>= zonkExpr

zonkExpr (HsUntypedSplice x _) = dataConCantHappen x

zonkExpr (OpApp x _ _ _) = dataConCantHappen x

zonkExpr (NegApp x expr op)
  = runZonkBndrT (zonkSyntaxExpr op) $ \ new_op ->
    do { new_expr <- zonkLExpr expr
       ; return (NegApp x new_expr new_op) }

zonkExpr (HsPar x e)
  = do { new_e <- zonkLExpr e
       ; return (HsPar x new_e) }

zonkExpr (SectionL x _ _) = dataConCantHappen x
zonkExpr (SectionR x _ _) = dataConCantHappen x
zonkExpr (ExplicitTuple x tup_args boxed)
  = do { new_tup_args <- mapM zonk_tup_arg tup_args
       ; return (ExplicitTuple x new_tup_args boxed) }
  where
    zonk_tup_arg (Present x e) = do { e' <- zonkLExpr e
                                    ; return (Present x e') }
    zonk_tup_arg (Missing t) = do { t' <- zonkScaledTcTypeToTypeX t
                                  ; return (Missing t') }


zonkExpr (ExplicitSum args alt arity expr)
  = do new_args <- mapM zonkTcTypeToTypeX args
       new_expr <- zonkLExpr expr
       return (ExplicitSum new_args alt arity new_expr)

zonkExpr (HsCase x expr ms)
  = do new_expr <- zonkLExpr expr
       new_ms <- zonkMatchGroup zonkLExpr ms
       return (HsCase x new_expr new_ms)

zonkExpr (HsIf x e1 e2 e3)
  = do new_e1 <- zonkLExpr e1
       new_e2 <- zonkLExpr e2
       new_e3 <- zonkLExpr e3
       return (HsIf x new_e1 new_e2 new_e3)

zonkExpr (HsMultiIf ty alts)
  = do { alts' <- mapM (wrapLocZonkMA zonk_alt) alts
       ; ty'   <- zonkTcTypeToTypeX ty
       ; return $ HsMultiIf ty' alts' }
  where zonk_alt (GRHS x guard expr)
          = runZonkBndrT (zonkStmts zonkLExpr guard) $ \ guard' ->
            do { expr' <- zonkLExpr expr
               ; return $ GRHS x guard' expr' }

zonkExpr (HsLet x binds expr)
  = runZonkBndrT (zonkLocalBinds binds) $ \ new_binds ->
    do { new_expr <- zonkLExpr expr
       ; return (HsLet x new_binds new_expr) }

zonkExpr (HsDo ty do_or_lc (L l stmts))
  = do new_stmts <- don'tBind $ zonkStmts zonkLExpr stmts
       new_ty <- zonkTcTypeToTypeX ty
       return (HsDo new_ty do_or_lc (L l new_stmts))

zonkExpr (ExplicitList ty exprs)
  = do new_ty <- zonkTcTypeToTypeX ty
       new_exprs <- zonkLExprs exprs
       return (ExplicitList new_ty new_exprs)

zonkExpr expr@(RecordCon { rcon_ext = con_expr, rcon_flds = rbinds })
  = do  { new_con_expr <- zonkExpr con_expr
        ; new_rbinds   <- zonkRecFields rbinds
        ; return (expr { rcon_ext  = new_con_expr
                       , rcon_flds = new_rbinds }) }

zonkExpr (ExprWithTySig _ e ty)
  = do { e' <- zonkLExpr e
       ; return (ExprWithTySig noExtField e' ty) }

zonkExpr (ArithSeq expr wit info)
  = do { new_expr <- zonkExpr expr
       ; runZonkBndrT (zonkWit wit) $ \ new_wit ->
    do { new_info <- zonkArithSeq  info
       ; return (ArithSeq new_expr new_wit new_info) } }
   where zonkWit Nothing    = return Nothing
         zonkWit (Just fln) = Just <$> zonkSyntaxExpr fln

zonkExpr (HsPragE x prag expr)
  = do new_expr <- zonkLExpr expr
       return (HsPragE x prag new_expr)

-- arrow notation extensions
zonkExpr (HsProc x pat body)
  = runZonkBndrT (zonkPat pat) $ \ new_pat ->
    do  { new_body <- zonkCmdTop body
        ; return (HsProc x new_pat new_body) }

-- StaticPointers extension
zonkExpr (HsStatic (fvs, ty) expr)
  = do new_ty <- zonkTcTypeToTypeX ty
       HsStatic (fvs, new_ty) <$> zonkLExpr expr

zonkExpr (HsEmbTy x _) = dataConCantHappen x
zonkExpr (HsQual x _ _) = dataConCantHappen x
zonkExpr (HsForAll x _ _) = dataConCantHappen x
zonkExpr (HsFunArr x _ _ _) = dataConCantHappen x

zonkExpr (XExpr (WrapExpr co_fn expr))
  = runZonkBndrT (zonkCoFn co_fn) $ \ new_co_fn ->
    do new_expr <- zonkExpr expr
       return (XExpr (WrapExpr new_co_fn new_expr))

zonkExpr (XExpr (ExpandedThingTc thing e))
  = do e' <- zonkExpr e
       return $ XExpr (ExpandedThingTc thing e')


zonkExpr (XExpr (ConLikeTc con tvs tys))
  = XExpr . ConLikeTc con tvs <$> mapM zonk_scale tys
  where
    zonk_scale (Scaled m ty) = Scaled <$> zonkTcTypeToTypeX m <*> pure ty
    -- Only the multiplicity can contain unification variables
    -- The tvs come straight from the data-con, and so are strictly redundant
    -- See Wrinkles of Note [Typechecking data constructors] in GHC.Tc.Gen.Head

zonkExpr (XExpr (HsRecSelTc (FieldOcc occ (L l v))))
  = do { v' <- zonkIdOcc v
       ; return (XExpr (HsRecSelTc (FieldOcc occ (L l v')))) }

zonkExpr (RecordUpd x _ _)  = dataConCantHappen x
zonkExpr (HsGetField x _ _) = dataConCantHappen x
zonkExpr (HsProjection x _) = dataConCantHappen x
zonkExpr e@(XExpr (HsTick {})) = pprPanic "zonkExpr" (ppr e)
zonkExpr e@(XExpr (HsBinTick {})) = pprPanic "zonkExpr" (ppr e)

-------------------------------------------------------------------------
{-
Note [Skolems in zonkSyntaxExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider rebindable syntax with something like

  (>>=) :: (forall x. blah) -> (forall y. blah') -> blah''

The x and y become skolems that are in scope when type-checking the
arguments to the bind. This means that we must extend the ZonkEnv with
these skolems when zonking the arguments to the bind. But the skolems
are different between the two arguments, and so we should theoretically
carry around different environments to use for the different arguments.

However, this becomes a logistical nightmare, especially in dealing with
the more exotic Stmt forms. So, we simplify by making the critical
assumption that the uniques of the skolems are different. (This assumption
is justified by the use of newUnique in GHC.Tc.Utils.TcMType.instSkolTyCoVarX.)
Now, we can safely just extend one environment.
-}

-- See Note [Skolems in zonkSyntaxExpr]
zonkSyntaxExpr :: SyntaxExpr GhcTc
               -> ZonkBndrTcM (SyntaxExpr GhcTc)
zonkSyntaxExpr (SyntaxExprTc { syn_expr      = expr
                             , syn_arg_wraps = arg_wraps
                             , syn_res_wrap  = res_wrap })
  = do { res_wrap'  <- zonkCoFn res_wrap
       ; expr'      <- noBinders $ zonkExpr expr
       ; arg_wraps' <- traverse zonkCoFn arg_wraps
       ; return SyntaxExprTc { syn_expr      = expr'
                             , syn_arg_wraps = arg_wraps'
                             , syn_res_wrap  = res_wrap' } }
zonkSyntaxExpr NoSyntaxExprTc = return NoSyntaxExprTc

-------------------------------------------------------------------------

zonkLCmd  :: LHsCmd GhcTc -> ZonkTcM (LHsCmd GhcTc)
zonkCmd   :: HsCmd GhcTc  -> ZonkTcM (HsCmd GhcTc)

zonkLCmd  cmd  = wrapLocZonkMA zonkCmd cmd

zonkCmd (XCmd (HsWrap w cmd))
  = runZonkBndrT (zonkCoFn w) $ \ w' ->
    do { cmd' <- zonkCmd cmd
       ; return (XCmd (HsWrap w' cmd')) }
zonkCmd (HsCmdArrApp ty e1 e2 ho rl)
  = do new_e1 <- zonkLExpr e1
       new_e2 <- zonkLExpr e2
       new_ty <- zonkTcTypeToTypeX ty
       return (HsCmdArrApp new_ty new_e1 new_e2 ho rl)

zonkCmd (HsCmdArrForm x op fixity args)
  = do new_op <- zonkLExpr op
       new_args <- mapM zonkCmdTop args
       return (HsCmdArrForm x new_op fixity new_args)

zonkCmd (HsCmdApp x c e)
  = do new_c <- zonkLCmd c
       new_e <- zonkLExpr e
       return (HsCmdApp x new_c new_e)

zonkCmd (HsCmdPar x c)
  = do new_c <- zonkLCmd c
       return (HsCmdPar x new_c)

zonkCmd (HsCmdCase x expr ms)
  = do new_expr <- zonkLExpr expr
       new_ms <- zonkMatchGroup zonkLCmd ms
       return (HsCmdCase x new_expr new_ms)

zonkCmd (HsCmdLam x lam_variant ms)
  = do new_ms <- zonkMatchGroup zonkLCmd ms
       return (HsCmdLam x lam_variant new_ms)

zonkCmd (HsCmdIf x eCond ePred cThen cElse)
  = runZonkBndrT (zonkSyntaxExpr eCond) $ \ new_eCond ->
    do { new_ePred <- zonkLExpr ePred
       ; new_cThen <- zonkLCmd cThen
       ; new_cElse <- zonkLCmd cElse
       ; return (HsCmdIf x new_eCond new_ePred new_cThen new_cElse) }

zonkCmd (HsCmdLet x binds cmd)
  = runZonkBndrT (zonkLocalBinds binds) $ \ new_binds ->
    do new_cmd <- zonkLCmd cmd
       return (HsCmdLet x new_binds new_cmd)

zonkCmd (HsCmdDo ty (L l stmts))
  = do new_stmts <- don'tBind $ zonkStmts zonkLCmd stmts
       new_ty <- zonkTcTypeToTypeX ty
       return (HsCmdDo new_ty (L l new_stmts))



zonkCmdTop :: LHsCmdTop GhcTc -> ZonkTcM (LHsCmdTop GhcTc)
zonkCmdTop cmd = wrapLocZonkMA (zonk_cmd_top) cmd

zonk_cmd_top :: HsCmdTop GhcTc -> ZonkTcM (HsCmdTop GhcTc)
zonk_cmd_top (HsCmdTop (CmdTopTc stack_tys ty ids) cmd)
  = do new_cmd <- zonkLCmd cmd
       new_stack_tys <- zonkTcTypeToTypeX stack_tys
       new_ty <- zonkTcTypeToTypeX ty
       new_ids <- mapSndM zonkExpr ids

       massert (definitelyLiftedType new_stack_tys)
         -- desugarer assumes that this is not representation-polymorphic...
         -- but indeed it should always be lifted due to the typing
         -- rules for arrows

       return (HsCmdTop (CmdTopTc new_stack_tys new_ty new_ids) new_cmd)

-------------------------------------------------------------------------
zonkCoFn :: HsWrapper -> ZonkBndrTcM HsWrapper
zonkCoFn WpHole   = return WpHole
zonkCoFn (WpCompose c1 c2) = do { c1' <- zonkCoFn c1
                                ; c2' <- zonkCoFn c2
                                ; return (WpCompose c1' c2') }
zonkCoFn (WpFun c1 c2 t1)  = do { c1' <- zonkCoFn c1
                                ; c2' <- zonkCoFn c2
                                ; t1' <- noBinders $ zonkScaledTcTypeToTypeX t1
                                ; return (WpFun c1' c2' t1') }
zonkCoFn (WpCast co)   = WpCast  <$> noBinders (zonkCoToCo co)
zonkCoFn (WpEvLam ev)  = WpEvLam <$> zonkEvBndrX ev
zonkCoFn (WpEvApp arg) = WpEvApp <$> noBinders (zonkEvTerm arg)
zonkCoFn (WpTyLam tv)  = assert (isImmutableTyVar tv) $
                         WpTyLam <$> zonkTyBndrX tv
zonkCoFn (WpTyApp ty)  = WpTyApp <$> noBinders (zonkTcTypeToTypeX ty)
zonkCoFn (WpLet bs)    = WpLet   <$> zonkTcEvBinds bs

-------------------------------------------------------------------------
zonkOverLit :: HsOverLit GhcTc -> ZonkTcM (HsOverLit GhcTc)
zonkOverLit lit@(OverLit {ol_ext = x@OverLitTc { ol_witness = e, ol_type = ty } })
  = do  { ty' <- zonkTcTypeToTypeX ty
        ; e' <- zonkExpr e
        ; return (lit { ol_ext = x { ol_witness = e'
                                   , ol_type = ty' } }) }

-------------------------------------------------------------------------
zonkBracket :: HsBracketTc -> ZonkTcM HsBracketTc
zonkBracket (HsBracketTc hsb_thing ty wrap bs)
  = do wrap' <- traverse zonkQuoteWrap wrap
       bs' <- mapM zonk_b bs
       new_ty <- zonkTcTypeToTypeX ty
       return (HsBracketTc hsb_thing new_ty wrap' bs')
  where
    zonkQuoteWrap (QuoteWrapper ev ty) = do
        ev' <- zonkIdOcc ev
        ty' <- zonkTcTypeToTypeX ty
        return (QuoteWrapper ev' ty')

    zonk_b (PendingTcSplice n e) = do e' <- zonkLExpr e
                                      return (PendingTcSplice n e')

-------------------------------------------------------------------------
zonkArithSeq :: ArithSeqInfo GhcTc -> ZonkTcM (ArithSeqInfo GhcTc)

zonkArithSeq (From e)
  = do new_e <- zonkLExpr e
       return (From new_e)

zonkArithSeq (FromThen e1 e2)
  = do new_e1 <- zonkLExpr e1
       new_e2 <- zonkLExpr e2
       return (FromThen new_e1 new_e2)

zonkArithSeq (FromTo e1 e2)
  = do new_e1 <- zonkLExpr e1
       new_e2 <- zonkLExpr e2
       return (FromTo new_e1 new_e2)

zonkArithSeq (FromThenTo e1 e2 e3)
  = do new_e1 <- zonkLExpr e1
       new_e2 <- zonkLExpr e2
       new_e3 <- zonkLExpr e3
       return (FromThenTo new_e1 new_e2 new_e3)

-------------------------------------------------------------------------
zonkStmts :: Anno (StmtLR GhcTc GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
          => (LocatedA (body GhcTc) -> ZonkTcM (LocatedA (body GhcTc)))
          -> [LStmt GhcTc (LocatedA (body GhcTc))]
          -> ZonkBndrTcM [LStmt GhcTc (LocatedA (body GhcTc))]
zonkStmts _ []     = return []
zonkStmts zBody (s:ss) = do { s'  <- wrapLocZonkBndrMA (zonkStmt zBody) s
                            ; ss' <- zonkStmts zBody ss
                            ; return (s' : ss') }

zonkStmt :: Anno (StmtLR GhcTc GhcTc (LocatedA (body GhcTc))) ~ SrcSpanAnnA
         => (LocatedA (body GhcTc) -> ZonkTcM (LocatedA (body GhcTc)))
         -> Stmt GhcTc (LocatedA (body GhcTc))
         -> ZonkBndrTcM (Stmt GhcTc (LocatedA (body GhcTc)))
zonkStmt _ (ParStmt bind_ty stmts_w_bndrs mzip_op bind_op)
  = do { new_bind_op <- zonkSyntaxExpr bind_op
       ; new_bind_ty <- noBinders $ zonkTcTypeToTypeX bind_ty
       ; new_stmts_w_bndrs <- noBinders $ mapM zonk_branch stmts_w_bndrs

       -- Add in the binders after we're done with all the branches.
       ; let new_binders = [ b | ParStmtBlock _ _ bs _ <- toList new_stmts_w_bndrs
                           , b <- bs ]
       ; extendIdZonkEnvRec new_binders
       ; new_mzip <- noBinders $ zonkExpr mzip_op
       ; return (ParStmt new_bind_ty new_stmts_w_bndrs new_mzip new_bind_op)}
  where
    zonk_branch :: ParStmtBlock GhcTc GhcTc
                -> ZonkTcM (ParStmtBlock GhcTc GhcTc)
    zonk_branch (ParStmtBlock x stmts bndrs return_op)
       = runZonkBndrT (zonkStmts zonkLExpr stmts) $ \ new_stmts ->
         runZonkBndrT (zonkSyntaxExpr return_op)  $ \ new_return ->
         do { new_bndrs <- zonkIdOccs bndrs
            ; return (ParStmtBlock x new_stmts new_bndrs new_return) }

zonkStmt zBody (RecStmt { recS_stmts = L _ segStmts, recS_later_ids = lvs
                        , recS_rec_ids = rvs
                        , recS_ret_fn = ret_id, recS_mfix_fn = mfix_id
                        , recS_bind_fn = bind_id
                        , recS_ext =
                                   RecStmtTc { recS_bind_ty = bind_ty
                                             , recS_later_rets = later_rets
                                             , recS_rec_rets = rec_rets
                                             , recS_ret_ty = ret_ty} })
  = do { new_bind_id <- zonkSyntaxExpr bind_id
       ; new_mfix_id <- zonkSyntaxExpr mfix_id
       ; new_ret_id  <- zonkSyntaxExpr ret_id
       ; new_bind_ty <- noBinders $ zonkTcTypeToTypeX bind_ty
       ; new_rvs     <- noBinders $ zonkIdBndrs rvs
       ; new_lvs     <- noBinders $ zonkIdBndrs lvs
       ; new_ret_ty  <- noBinders $ zonkTcTypeToTypeX ret_ty

    -- Zonk the ret-expressions in an environment that
    -- has the polymorphic bindings
       ; rec_stmt <- noBinders $ don'tBind $
          do { extendIdZonkEnvRec new_rvs
             ; new_segStmts   <- zonkStmts zBody segStmts
             ; new_later_rets <- noBinders $ mapM zonkExpr later_rets
             ; new_rec_rets   <- noBinders $ mapM zonkExpr rec_rets
             ; return $
               RecStmt { recS_stmts = noLocA new_segStmts
                       , recS_later_ids = new_lvs
                       , recS_rec_ids = new_rvs, recS_ret_fn = new_ret_id
                       , recS_mfix_fn = new_mfix_id, recS_bind_fn = new_bind_id
                       , recS_ext = RecStmtTc
                           { recS_bind_ty = new_bind_ty
                           , recS_later_rets = new_later_rets
                           , recS_rec_rets = new_rec_rets
                           , recS_ret_ty = new_ret_ty } } }

    -- Only the lvs are needed
       ; extendIdZonkEnvRec new_lvs
       ; return rec_stmt }

zonkStmt zBody (BodyStmt ty body then_op guard_op)
  = do { new_then_op  <- zonkSyntaxExpr then_op
       ; new_guard_op <- zonkSyntaxExpr guard_op
       ; new_body     <- noBinders $ zBody body
       ; new_ty       <- noBinders $ zonkTcTypeToTypeX  ty
       ; return $ BodyStmt new_ty new_body new_then_op new_guard_op }

zonkStmt zBody (LastStmt x body noret ret_op)
  = noBinders $ runZonkBndrT (zonkSyntaxExpr ret_op) $ \ new_ret ->
    do { new_body <- zBody body
       ; return $ LastStmt x new_body noret new_ret }

zonkStmt _ (TransStmt { trS_stmts = stmts, trS_bndrs = binderMap
                      , trS_by = by, trS_form = form, trS_using = using
                      , trS_ret = return_op, trS_bind = bind_op
                      , trS_ext = bind_arg_ty
                      , trS_fmap = liftM_op })
  = do { bind_op'     <- zonkSyntaxExpr bind_op
       ; bind_arg_ty' <- noBinders $ zonkTcTypeToTypeX bind_arg_ty
       ; stmts'       <- zonkStmts zonkLExpr stmts
       ; by'          <- noBinders $ traverse zonkLExpr by
       ; using'       <- noBinders $ zonkLExpr using
       ; return_op'   <- zonkSyntaxExpr return_op
       ; liftM_op'    <- noBinders $ zonkExpr liftM_op
       ; binderMap'   <- mapM zonkBinderMapEntry binderMap
       ; return (TransStmt { trS_stmts = stmts', trS_bndrs = binderMap'
                           , trS_by = by', trS_form = form, trS_using = using'
                           , trS_ret = return_op', trS_bind = bind_op'
                           , trS_ext = bind_arg_ty'
                           , trS_fmap = liftM_op' }) }
  where
    zonkBinderMapEntry (oldBinder, newBinder) = do
        oldBinder' <- noBinders $ zonkIdOcc oldBinder
        newBinder' <- zonkIdBndrX newBinder
        return (oldBinder', newBinder')

zonkStmt _ (LetStmt x binds)
  = LetStmt x <$> zonkLocalBinds binds

zonkStmt zBody (BindStmt xbs pat body)
  = do  { new_bind    <- zonkSyntaxExpr (xbstc_bindOp xbs)
        ; new_w       <- noBinders $ zonkTcTypeToTypeX (xbstc_boundResultMult xbs)
        ; new_bind_ty <- noBinders $ zonkTcTypeToTypeX (xbstc_boundResultType xbs)
        ; new_body    <- noBinders $ zBody body
        ; new_fail <- case xbstc_failOp xbs of
            Nothing      -> return Nothing
            Just fail_op -> fmap Just <$> noBinders $ don'tBind (zonkSyntaxExpr fail_op)

        ; new_pat     <- zonkPat pat
        ; return $
            BindStmt
            (XBindStmtTc
              { xbstc_bindOp = new_bind
              , xbstc_boundResultType = new_bind_ty
              , xbstc_boundResultMult = new_w
              , xbstc_failOp = new_fail
              })
            new_pat new_body }

-- Scopes: join > ops (in reverse order) > pats (in forward order)
--              > rest of stmts
zonkStmt _zBody (XStmtLR (ApplicativeStmt body_ty args mb_join))
  = do  { new_mb_join   <- zonk_join mb_join
        ; new_args      <- zonk_args args
        ; new_body_ty   <- noBinders $ zonkTcTypeToTypeX body_ty
        ; return $ XStmtLR $ ApplicativeStmt new_body_ty new_args new_mb_join }
  where
    zonk_join Nothing  = return Nothing
    zonk_join (Just j) = Just <$> zonkSyntaxExpr j

    get_pat :: (SyntaxExpr GhcTc, ApplicativeArg GhcTc) -> LPat GhcTc
    get_pat (_, ApplicativeArgOne _ pat _ _) = pat
    get_pat (_, ApplicativeArgMany _ _ _ pat _) = pat

    replace_pat :: LPat GhcTc
                -> (SyntaxExpr GhcTc, ApplicativeArg GhcTc)
                -> (SyntaxExpr GhcTc, ApplicativeArg GhcTc)
    replace_pat pat (op, ApplicativeArgOne fail_op _ a isBody)
      = (op, ApplicativeArgOne fail_op pat a isBody)
    replace_pat pat (op, ApplicativeArgMany x a b _ c)
      = (op, ApplicativeArgMany x a b pat c)

    zonk_args args
      = do { new_args_rev <- zonk_args_rev (reverse args)
           ; new_pats     <- zonkPats (map get_pat args)
           ; return $ zipWithEqual replace_pat
                        new_pats (reverse new_args_rev) }

     -- these need to go backward, because if any operators are higher-rank,
     -- later operators may introduce skolems that are in scope for earlier
     -- arguments
    zonk_args_rev ((op, arg) : args)
      = do { new_op   <- zonkSyntaxExpr op
           ; new_arg  <- noBinders $ zonk_arg arg
           ; new_args <- zonk_args_rev args
           ; return $ (new_op, new_arg) : new_args }
    zonk_args_rev [] = return []

    zonk_arg (ApplicativeArgOne fail_op pat expr isBody)
      = do { new_expr <- zonkLExpr expr
           ; new_fail <- forM fail_op $ don'tBind . zonkSyntaxExpr
           ; return (ApplicativeArgOne new_fail pat new_expr isBody) }
    zonk_arg (ApplicativeArgMany x stmts ret pat ctxt)
      = runZonkBndrT (zonkStmts zonkLExpr stmts) $ \ new_stmts ->
        do { new_ret <- zonkExpr ret
           ; return (ApplicativeArgMany x new_stmts new_ret pat ctxt) }

-------------------------------------------------------------------------
zonkRecFields :: HsRecordBinds GhcTc -> ZonkTcM (HsRecordBinds GhcTc)
zonkRecFields (HsRecFields x flds dd)
  = do  { flds' <- mapM zonk_rbind flds
        ; return (HsRecFields x flds' dd) }
  where
    zonk_rbind (L l fld)
      = do { new_id   <- wrapLocZonkMA zonkFieldOcc (hfbLHS fld)
           ; new_expr <- zonkLExpr (hfbRHS fld)
           ; return (L l (fld { hfbLHS = new_id
                              , hfbRHS = new_expr })) }

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Pats]{Patterns}
*                                                                      *
************************************************************************
-}


zonkPat :: LPat GhcTc -> ZonkBndrTcM (LPat GhcTc)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat pat = wrapLocZonkBndrMA zonk_pat pat

zonk_pat :: Pat GhcTc -> ZonkBndrTcM (Pat GhcTc)
zonk_pat (ParPat x p)
  = do  { p' <- zonkPat p
        ; return (ParPat x p') }

zonk_pat (WildPat ty)
  = do  { ty' <- noBinders $ zonkTcTypeToTypeX ty
        ; return (WildPat ty') }

zonk_pat (VarPat x (L l v))
  = do  { v' <- zonkIdBndrX v
        ; return (VarPat x (L l v')) }

zonk_pat (LazyPat x pat)
  = do  { pat' <- zonkPat pat
        ; return (LazyPat x pat') }

zonk_pat (BangPat x pat)
  = do  { pat' <- zonkPat pat
        ; return (BangPat x pat') }

zonk_pat (AsPat x (L loc v) pat)
  = do  { v'   <- zonkIdBndrX v
        ; pat' <- zonkPat pat
        ; return (AsPat x (L loc v') pat') }

zonk_pat (ViewPat ty expr pat)
  = do  { expr' <- noBinders $ zonkLExpr expr
        ; pat'  <- zonkPat pat
        ; ty'   <- noBinders $ zonkTcTypeToTypeX ty
        ; return (ViewPat ty' expr' pat') }

zonk_pat (ListPat ty pats)
  = do  { ty'   <- noBinders $ zonkTcTypeToTypeX ty
        ; pats' <- zonkPats pats
        ; return (ListPat ty' pats') }

zonk_pat (TuplePat tys pats boxed)
  = do  { tys' <- noBinders $ mapM zonkTcTypeToTypeX tys
        ; pats' <- zonkPats pats
        ; return (TuplePat tys' pats' boxed) }

zonk_pat (OrPat ty pats)
  = do  { ty' <- noBinders $ zonkTcTypeToTypeX ty
        ; pats' <- zonkPats pats
        ; return (OrPat ty' pats') }

zonk_pat (SumPat tys pat alt arity )
  = do  { tys' <- noBinders $ mapM zonkTcTypeToTypeX tys
        ; pat' <- zonkPat pat
        ; return (SumPat tys' pat' alt arity) }

zonk_pat p@(ConPat { pat_args = args
                   , pat_con_ext = p'@(ConPatTc
                     { cpt_tvs = tyvars
                     , cpt_dicts = evs
                     , cpt_binds = binds
                     , cpt_wrap = wrapper
                     , cpt_arg_tys = tys
                     })
                   })
  = assert (all isImmutableTyVar tyvars) $
    do  { new_tys     <- noBinders $ mapM zonkTcTypeToTypeX tys
        ; new_tyvars  <- zonkTyBndrsX tyvars
          -- Must zonk the existential variables, because their
          -- /kind/ need potential zonking.
          -- cf typecheck/should_compile/tc221.hs
        ; new_evs     <- zonkEvBndrsX evs
        ; new_binds   <- zonkTcEvBinds binds
        ; new_wrapper <- zonkCoFn wrapper
        ; new_args    <- zonkConStuff args
        ; pure $ p
                 { pat_args = new_args
                 , pat_con_ext = p'
                   { cpt_arg_tys = new_tys
                   , cpt_tvs = new_tyvars
                   , cpt_dicts = new_evs
                   , cpt_binds = new_binds
                   , cpt_wrap = new_wrapper
                   }
                 }
        }

zonk_pat (LitPat x lit) = return (LitPat x lit)

zonk_pat (SigPat ty pat hs_ty)
  = do  { ty' <- noBinders $ zonkTcTypeToTypeX ty
        ; pat' <- zonkPat pat
        ; return (SigPat ty' pat' hs_ty) }

zonk_pat (NPat ty (L l lit) mb_neg eq_expr)
  =  do { eq_expr' <- zonkSyntaxExpr eq_expr
        ; mb_neg' <- case mb_neg of
            Nothing -> return Nothing
            Just n  -> Just <$> zonkSyntaxExpr n
        ; noBinders $
     do { lit' <- zonkOverLit lit
        ; ty'  <- zonkTcTypeToTypeX ty
        ; return (NPat ty' (L l lit') mb_neg' eq_expr') } }

zonk_pat (NPlusKPat ty (L loc n) (L l lit1) lit2 e1 e2)
  = do  { e1' <- zonkSyntaxExpr  e1
        ; e2' <- zonkSyntaxExpr e2
        ; lit1' <- noBinders $ zonkOverLit lit1
        ; lit2' <- noBinders $ zonkOverLit lit2
        ; ty'   <- noBinders $ zonkTcTypeToTypeX ty
        ; n'    <- zonkIdBndrX n
        ; return (NPlusKPat ty' (L loc n') (L l lit1') lit2' e1' e2') }

zonk_pat (EmbTyPat ty tp)
  = do { ty' <- noBinders $ zonkTcTypeToTypeX ty
       ; return (EmbTyPat ty' tp) }

zonk_pat (InvisPat ty tp)
  = do { ty' <- noBinders $ zonkTcTypeToTypeX ty
       ; return (InvisPat ty' tp) }

zonk_pat (XPat ext) = case ext of
  { ExpansionPat orig pat->
    do { pat' <- zonk_pat pat
       ; return $ XPat $ ExpansionPat orig pat' }
  ; CoPat co_fn pat ty ->
    do { co_fn' <- zonkCoFn co_fn
       ; pat'   <- zonkPat (noLocA pat)
       ; ty'    <- noBinders $ zonkTcTypeToTypeX ty
       ; return (XPat $ CoPat co_fn' (unLoc pat') ty')
       } }

zonk_pat pat = pprPanic "zonk_pat" (ppr pat)

---------------------------
zonkConStuff :: HsConPatDetails GhcTc
             -> ZonkBndrTcM (HsConPatDetails GhcTc)
zonkConStuff (PrefixCon tyargs pats)
  = do  { pats' <- zonkPats pats
        ; return (PrefixCon tyargs pats') }

zonkConStuff (InfixCon p1 p2)
  = do  { p1' <- zonkPat p1
        ; p2' <- zonkPat p2
        ; return (InfixCon p1' p2') }

zonkConStuff (RecCon (HsRecFields x rpats dd))
  = do  { pats' <- zonkPats (map (hfbRHS . unLoc) rpats)
        ; let rpats' = zipWith (\(L l rp) p' ->
                                  L l (rp { hfbRHS = p' }))
                               rpats pats'
        ; return (RecCon (HsRecFields x rpats' dd)) }
        -- Field selectors have declared types; hence no zonking

---------------------------
zonkPats :: Traversable f => f (LPat GhcTc) -> ZonkBndrTcM (f (LPat GhcTc))
zonkPats = traverse zonkPat
{-# SPECIALISE zonkPats :: [LPat GhcTc] -> ZonkBndrTcM [LPat GhcTc] #-}
{-# SPECIALISE zonkPats :: NonEmpty (LPat GhcTc) -> ZonkBndrTcM (NonEmpty (LPat GhcTc)) #-}

{-
************************************************************************
*                                                                      *
\subsection[BackSubst-Foreign]{Foreign exports}
*                                                                      *
************************************************************************
-}

zonkForeignExports :: [LForeignDecl GhcTc]
                   -> ZonkTcM [LForeignDecl GhcTc]
zonkForeignExports ls = mapM (wrapLocZonkMA zonkForeignExport) ls

zonkForeignExport :: ForeignDecl GhcTc -> ZonkTcM (ForeignDecl GhcTc)
zonkForeignExport (ForeignExport { fd_name = i, fd_e_ext = co
                                 , fd_fe = spec })
  = do { i' <- zonkLIdOcc i
       ; return (ForeignExport { fd_name = i'
                               , fd_sig_ty = undefined, fd_e_ext = co
                               , fd_fe = spec }) }
zonkForeignExport for_imp
  = return for_imp     -- Foreign imports don't need zonking

zonkRules :: [LRuleDecl GhcTc] -> ZonkTcM [LRuleDecl GhcTc]
zonkRules rs = mapM (wrapLocZonkMA zonkRule) rs

zonkRule :: RuleDecl GhcTc -> ZonkTcM (RuleDecl GhcTc)
zonkRule rule@(HsRule { rd_tmvs = tm_bndrs{-::[RuleBndr TcId]-}
                      , rd_lhs = lhs
                      , rd_rhs = rhs })
  = runZonkBndrT (traverse zonk_tm_bndr tm_bndrs) $ \ new_tm_bndrs ->
    do { -- See Note [Zonking the LHS of a RULE]
       ; new_lhs <- setZonkType SkolemiseFlexi $ zonkLExpr lhs
       ; new_rhs <-                              zonkLExpr rhs
       ; return $ rule { rd_tmvs = new_tm_bndrs
                       , rd_lhs  = new_lhs
                       , rd_rhs  = new_rhs } }
  where
   zonk_tm_bndr :: LRuleBndr GhcTc -> ZonkBndrTcM (LRuleBndr GhcTc)
   zonk_tm_bndr (L l (RuleBndr x (L loc v)))
      = do { v' <- zonk_it v
           ; return (L l (RuleBndr x (L loc v'))) }
   zonk_tm_bndr (L _ (RuleBndrSig {})) = panic "zonk_tm_bndr RuleBndrSig"

   zonk_it v
     | isId v     = zonkIdBndrX v
     | otherwise  = assert (isImmutableTyVar v)
                    zonkTyBndrX v
                    -- DV: used to be "return v", but that is plain
                    -- wrong because we may need to go inside the kind
                    -- of v and zonk there!

{-
************************************************************************
*                                                                      *
              Constraints and evidence
*                                                                      *
************************************************************************
-}

zonkEvTerm :: EvTerm -> ZonkTcM EvTerm
zonkEvTerm (EvExpr e)
  = EvExpr <$> zonkCoreExpr e
zonkEvTerm (EvTypeable ty ev)
  = EvTypeable <$> zonkTcTypeToTypeX ty <*> zonkEvTypeable ev
zonkEvTerm (EvFun { et_tvs = tvs, et_given = evs
                  , et_binds = ev_binds, et_body = body_id })
  = runZonkBndrT (zonkTyBndrsX tvs)       $ \ new_tvs      ->
    runZonkBndrT (zonkEvBndrsX evs)       $ \ new_evs      ->
    runZonkBndrT (zonkTcEvBinds ev_binds) $ \ new_ev_binds ->
  do { new_body_id  <- zonkIdOcc body_id
     ; return (EvFun { et_tvs = new_tvs, et_given = new_evs
                     , et_binds = new_ev_binds, et_body = new_body_id }) }

zonkCoreExpr :: CoreExpr -> ZonkTcM CoreExpr
zonkCoreExpr (Var v)
    | isCoVar v
    = Coercion <$> zonkCoVarOcc v
    | otherwise
    = Var <$> zonkIdOcc v
zonkCoreExpr (Lit l)
    = return $ Lit l
zonkCoreExpr (Coercion co)
    = Coercion <$> zonkCoToCo co
zonkCoreExpr (Type ty)
    = Type <$> zonkTcTypeToTypeX ty

zonkCoreExpr (Cast e co)
    = Cast <$> zonkCoreExpr e <*> zonkCoToCo co
zonkCoreExpr (Tick t e)
    = Tick t <$> zonkCoreExpr e -- Do we need to zonk in ticks?

zonkCoreExpr (App e1 e2)
    = App <$> zonkCoreExpr e1 <*> zonkCoreExpr e2
zonkCoreExpr (Lam v e)
    = runZonkBndrT (zonkCoreBndrX v) $ \ v' ->
      Lam v' <$> zonkCoreExpr e
zonkCoreExpr (Let bind e)
    = runZonkBndrT (zonkCoreBind bind) $ \ bind' ->
      Let bind' <$> zonkCoreExpr e
zonkCoreExpr (Case scrut b ty alts)
    = do { scrut' <- zonkCoreExpr scrut
         ; ty' <- zonkTcTypeToTypeX ty
         ; runZonkBndrT (zonkIdBndrX b) $ \ b' ->
      do { alts' <- mapM zonkCoreAlt alts
         ; return $ Case scrut' b' ty' alts' } }

zonkCoreAlt :: CoreAlt -> ZonkTcM CoreAlt
zonkCoreAlt (Alt dc bndrs rhs)
    = runZonkBndrT (zonkCoreBndrsX bndrs) $ \ bndrs' ->
      do { rhs' <- zonkCoreExpr rhs
         ; return $ Alt dc bndrs' rhs' }

zonkCoreBind :: CoreBind -> ZonkBndrTcM CoreBind
zonkCoreBind (NonRec v e)
    = do { (v',e') <- noBinders $ zonkCorePair (v,e)
         ; extendIdZonkEnv v'
         ; return (NonRec v' e') }
zonkCoreBind (Rec pairs)
    = do pairs' <- mfix go
         return $ Rec pairs'
  where
    go new_pairs = do
      extendIdZonkEnvRec (map fst new_pairs)
      noBinders $ mapM zonkCorePair pairs

zonkCorePair :: (CoreBndr, CoreExpr) -> ZonkTcM (CoreBndr, CoreExpr)
zonkCorePair (v,e) =
  do { v' <- zonkIdBndr v
     ; e' <- zonkCoreExpr e
     ; return (v',e') }

zonkEvTypeable :: EvTypeable -> ZonkTcM EvTypeable
zonkEvTypeable (EvTypeableTyCon tycon e)
  = do { e'  <- mapM zonkEvTerm e
       ; return $ EvTypeableTyCon tycon e' }
zonkEvTypeable (EvTypeableTyApp t1 t2)
  = do { t1' <- zonkEvTerm t1
       ; t2' <- zonkEvTerm t2
       ; return (EvTypeableTyApp t1' t2') }
zonkEvTypeable (EvTypeableTrFun tm t1 t2)
  = do { tm' <- zonkEvTerm tm
       ; t1' <- zonkEvTerm t1
       ; t2' <- zonkEvTerm t2
       ; return (EvTypeableTrFun tm' t1' t2') }
zonkEvTypeable (EvTypeableTyLit t1)
  = do { t1' <- zonkEvTerm t1
       ; return (EvTypeableTyLit t1') }

zonkTcEvBinds_s :: [TcEvBinds] -> ZonkBndrTcM [TcEvBinds]
zonkTcEvBinds_s bs = do { bs' <- traverse zonk_tc_ev_binds bs
                        ; return ([EvBinds (unionManyBags bs')]) }

zonkTcEvBinds :: TcEvBinds -> ZonkBndrTcM TcEvBinds
zonkTcEvBinds bs = do { bs' <- zonk_tc_ev_binds bs
                      ; return (EvBinds bs') }

zonk_tc_ev_binds :: TcEvBinds -> ZonkBndrTcM (Bag EvBind)
zonk_tc_ev_binds (TcEvBinds var) = zonkEvBindsVar var
zonk_tc_ev_binds (EvBinds bs)    = zonkEvBinds bs

zonkEvBindsVar :: EvBindsVar -> ZonkBndrTcM (Bag EvBind)
zonkEvBindsVar (EvBindsVar { ebv_binds = ref })
  = do { bs <- readTcRef ref
       ; zonkEvBinds (evBindMapBinds bs) }
zonkEvBindsVar (CoEvBindsVar {}) = return emptyBag

zonkEvBinds :: Bag EvBind -> ZonkBndrTcM (Bag EvBind)
zonkEvBinds binds
  = {-# SCC "zonkEvBinds" #-}
    mfix $ \ new_binds ->
  do { extendIdZonkEnvRec (collect_ev_bndrs new_binds)
     ; noBinders $ mapBagM zonkEvBind binds }
  where
    collect_ev_bndrs :: Bag EvBind -> [EvVar]
    collect_ev_bndrs = foldr add []
    add (EvBind { eb_lhs = var }) vars = var : vars

zonkEvBind :: EvBind -> ZonkTcM EvBind
zonkEvBind bind@(EvBind { eb_lhs = var, eb_rhs = term })
  = do { var'  <- {-# SCC "zonkEvBndr" #-} zonkEvBndr var

         -- Optimise the common case of Refl coercions
         -- See Note [Optimise coercion zonking]
         -- This has a very big effect on some programs (eg #5030)

       ; term' <- case getEqPredTys_maybe (idType var') of
           Just (r, ty1, ty2) | ty1 `eqType` ty2
                  -> return (evCoercion (mkReflCo r ty1))
           _other -> zonkEvTerm term

       ; return (bind { eb_lhs = var', eb_rhs = term' }) }

{- Note [Optimise coercion zonking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When optimising evidence binds we may come across situations where
a coercion looks like
      cv = ReflCo ty
or    cv1 = cv2
where the type 'ty' is big.  In such cases it is a waste of time to zonk both
  * The variable on the LHS
  * The coercion on the RHS
Rather, we can zonk the variable, and if its type is (ty ~ ty), we can just
use Refl on the right, ignoring the actual coercion on the RHS.

This can have a very big effect, because the constraint solver sometimes does go
to a lot of effort to prove Refl!  (Eg when solving  10+3 = 10+3; cf #5030)
-}

zonkTcMethInfoToMethInfoX :: TcMethInfo -> ZonkTcM MethInfo
zonkTcMethInfoToMethInfoX (name, ty, gdm_spec)
  = do { ty' <- zonkTcTypeToTypeX ty
       ; gdm_spec' <- zonk_gdm gdm_spec
       ; return (name, ty', gdm_spec') }
  where
    zonk_gdm :: Maybe (DefMethSpec (SrcSpan, TcType))
             -> ZonkTcM (Maybe (DefMethSpec (SrcSpan, Type)))
    zonk_gdm Nothing = return Nothing
    zonk_gdm (Just VanillaDM) = return (Just VanillaDM)
    zonk_gdm (Just (GenericDM (loc, ty)))
      = do { ty' <- zonkTcTypeToTypeX ty
           ; return (Just (GenericDM (loc, ty'))) }

---------------------------------------
{- Note [Zonking the LHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also GHC.HsToCore.Binds Note [Free tyvars on rule LHS]

We need to gather the type variables mentioned on the LHS so we can
quantify over them.  Example:
  data T a = C

  foo :: T a -> Int
  foo C = 1

  {-# RULES "myrule"  foo C = 1 #-}

After type checking the LHS becomes (foo alpha (C alpha)) and we do
not want to zap the unbound meta-tyvar 'alpha' to Any, because that
limits the applicability of the rule.  Instead, we want to quantify
over it!

We do this in two stages.

* During zonking, we skolemise the TcTyVar 'alpha' to TyVar 'a'.  We
  do this by using zonkTvSkolemising as the UnboundTyVarZonker in the
  ZonkEnv.  (This is in fact the whole reason that the ZonkEnv has a
  UnboundTyVarZonker.)

* In GHC.HsToCore.Binds, we quantify over it.  See GHC.HsToCore.Binds
  Note [Free tyvars on rule LHS]

Quantifying here is awkward because (a) the data type is big and (b)
finding the free type vars of an expression is necessarily monadic
operation. (consider /\a -> f @ b, where b is side-effected to a)
-}

{-
************************************************************************
*                                                                      *
             Checking for coercion holes
*                                                                      *
************************************************************************
-}

-- | Is a coercion hole filled in?
isFilledCoercionHole :: CoercionHole -> TcM Bool
isFilledCoercionHole (CoercionHole { ch_ref = ref })
  = isJust <$> readTcRef ref

-- | Retrieve the contents of a coercion hole. Panics if the hole
-- is unfilled
unpackCoercionHole :: CoercionHole -> TcM Coercion
unpackCoercionHole hole
  = do { contents <- unpackCoercionHole_maybe hole
       ; case contents of
           Just co -> return co
           Nothing -> pprPanic "Unfilled coercion hole" (ppr hole) }

-- | Retrieve the contents of a coercion hole, if it is filled
unpackCoercionHole_maybe :: CoercionHole -> TcM (Maybe Coercion)
unpackCoercionHole_maybe (CoercionHole { ch_ref = ref }) = readTcRef ref

zonkCtRewriterSet :: Ct -> TcM Ct
zonkCtRewriterSet ct
  | isGivenCt ct
  = return ct
  | otherwise
  = case ct of
      CEqCan eq@(EqCt { eq_ev = ev })       -> do { ev' <- zonkCtEvRewriterSet ev
                                                  ; return (CEqCan (eq { eq_ev = ev' })) }
      CIrredCan ir@(IrredCt { ir_ev = ev }) -> do { ev' <- zonkCtEvRewriterSet ev
                                                  ; return (CIrredCan (ir { ir_ev = ev' })) }
      CDictCan di@(DictCt { di_ev = ev })   -> do { ev' <- zonkCtEvRewriterSet ev
                                                  ; return (CDictCan (di { di_ev = ev' })) }
      CQuantCan {}     -> return ct
      CNonCanonical ev -> do { ev' <- zonkCtEvRewriterSet ev
                             ; return (CNonCanonical ev') }

zonkCtEvRewriterSet :: CtEvidence -> TcM CtEvidence
zonkCtEvRewriterSet ev@(CtGiven {})
  = return ev
zonkCtEvRewriterSet ev@(CtWanted { ctev_rewriters = rewriters })
  = do { rewriters' <- zonkRewriterSet rewriters
       ; return (ev { ctev_rewriters = rewriters' }) }

-- | Check whether any coercion hole in a RewriterSet is still unsolved.
-- Does this by recursively looking through filled coercion holes until
-- one is found that is not yet filled in, at which point this aborts.
zonkRewriterSet :: RewriterSet -> TcM RewriterSet
zonkRewriterSet (RewriterSet set)
  = nonDetStrictFoldUniqSet go (return emptyRewriterSet) set
     -- this does not introduce non-determinism, because the only
     -- monadic action is to read, and the combining function is
     -- commutative
  where
    go :: CoercionHole -> TcM RewriterSet -> TcM RewriterSet
    go hole m_acc = unionRewriterSet <$> check_hole hole <*> m_acc

    check_hole :: CoercionHole -> TcM RewriterSet
    check_hole hole = do { m_co <- unpackCoercionHole_maybe hole
                         ; case m_co of
                             Nothing -> return (unitRewriterSet hole)
                             Just co -> unUCHM (check_co co) }

    check_ty :: Type -> UnfilledCoercionHoleMonoid
    check_co :: Coercion -> UnfilledCoercionHoleMonoid
    (check_ty, _, check_co, _) = foldTyCo folder ()

    folder :: TyCoFolder () UnfilledCoercionHoleMonoid
    folder = TyCoFolder { tcf_view  = noView
                        , tcf_tyvar = \ _ tv -> check_ty (tyVarKind tv)
                        , tcf_covar = \ _ cv -> check_ty (varType cv)
                        , tcf_hole  = \ _ -> UCHM . check_hole
                        , tcf_tycobinder = \ _ _ _ -> () }

newtype UnfilledCoercionHoleMonoid = UCHM { unUCHM :: TcM RewriterSet }

instance Semigroup UnfilledCoercionHoleMonoid where
  UCHM l <> UCHM r = UCHM (unionRewriterSet <$> l <*> r)

instance Monoid UnfilledCoercionHoleMonoid where
  mempty = UCHM (return emptyRewriterSet)
