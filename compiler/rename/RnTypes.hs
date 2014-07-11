{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

\section[RnSource]{Main pass of renamer}
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RnTypes (
        -- Type related stuff
        rnHsType, rnLHsType, rnLHsTypes, rnContext,
        rnHsKind, rnLHsKind, rnLHsMaybeKind,
        rnHsSigType, rnLHsInstType, rnConDeclFields,
        newTyVarNameRn, rnLHsTypeWithWildCards,
        rnHsSigTypeWithWildCards, rnLTyVar, collectWildCards,

        -- Precence related stuff
        mkOpAppRn, mkNegAppRn, mkOpFormRn, mkConOpPatRn,
        checkPrecMatch, checkSectionPrec,

        -- Binding related stuff
        warnContextQuantification, warnUnusedForAlls,
        bindSigTyVarsFV, bindHsTyVars, rnHsBndrSig, rnLHsTyVarBndr,
        extractHsTyRdrTyVars, extractHsTysRdrTyVars, extractTyVarBndrNames,
        extractRdrKindSigVars, extractDataDefnKindVars,
        filterInScope
  ) where

import {-# SOURCE #-} RnSplice( rnSpliceType )

import DynFlags
import HsSyn
import RnHsDoc          ( rnLHsDoc, rnMbLHsDoc )
import RnEnv
import TcRnMonad
import RdrName
import PrelNames
import TysPrim          ( funTyConName )
import Name
import SrcLoc
import NameSet

import Util
import BasicTypes       ( compareFixity, funTyFixity, negateFixity,
                          Fixity(..), FixityDirection(..) )
import Outputable
import FastString
import Maybes
import Data.List        ( nub, nubBy, deleteFirstsBy )
import qualified Data.Set as Set
import Control.Monad    ( unless, when )

#if __GLASGOW_HASKELL__ < 709
import Data.Monoid      ( mappend, mempty, mconcat )
#endif

#include "HsVersions.h"

{-
These type renamers are in a separate module, rather than in (say) RnSource,
to break several loop.

*********************************************************
*                                                      *
\subsection{Renaming types}
*                                                      *
*********************************************************
-}

rnHsSigType :: SDoc -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
        -- rnHsSigType is used for source-language type signatures,
        -- which use *implicit* universal quantification.
rnHsSigType doc_str ty = rnLHsType (TypeSigCtx doc_str) ty

rnLHsInstType :: SDoc -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
-- Rename the type in an instance or standalone deriving decl
rnLHsInstType doc_str ty
  = do { (ty', fvs) <- rnLHsType (GenericCtx doc_str) ty
       ; unless good_inst_ty (addErrAt (getLoc ty) (badInstTy ty))
       ; return (ty', fvs) }
  where
    good_inst_ty
      | Just (_, _, L _ cls, _) <-
                        splitLHsInstDeclTy_maybe (flattenTopLevelLHsForAllTy ty)
      , isTcOcc (rdrNameOcc cls) = True
      | otherwise                = False

badInstTy :: LHsType RdrName -> SDoc
badInstTy ty = ptext (sLit "Malformed instance:") <+> ppr ty

{-
rnHsType is here because we call it from loadInstDecl, and I didn't
want a gratuitous knot.

Note [Context quantification]
-----------------------------
Variables in type signatures are implicitly quantified
when (1) they are in a type signature not beginning
with "forall" or (2) in any qualified type T => R.
We are phasing out (2) since it leads to inconsistencies
(Trac #4426):

data A = A (a -> a)           is an error
data A = A (Eq a => a -> a)   binds "a"
data A = A (Eq a => a -> b)   binds "a" and "b"
data A = A (() => a -> b)     binds "a" and "b"
f :: forall a. a -> b         is an error
f :: forall a. () => a -> b   is an error
f :: forall a. a -> (() => b) binds "a" and "b"

The -fwarn-context-quantification flag warns about
this situation. See rnHsTyKi for case HsForAllTy Qualified.
-}

rnLHsTyKi  :: Bool --  True <=> renaming a type, False <=> a kind
           -> HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsTyKi isType doc (L loc ty)
  = setSrcSpan loc $
    do { (ty', fvs) <- rnHsTyKi isType doc ty
       ; return (L loc ty', fvs) }

rnLHsType  :: HsDocContext -> LHsType RdrName -> RnM (LHsType Name, FreeVars)
rnLHsType = rnLHsTyKi True

rnLHsKind  :: HsDocContext -> LHsKind RdrName -> RnM (LHsKind Name, FreeVars)
rnLHsKind = rnLHsTyKi False

rnLHsMaybeKind  :: HsDocContext -> Maybe (LHsKind RdrName)
                -> RnM (Maybe (LHsKind Name), FreeVars)
rnLHsMaybeKind _ Nothing
  = return (Nothing, emptyFVs)
rnLHsMaybeKind doc (Just kind)
  = do { (kind', fvs) <- rnLHsKind doc kind
       ; return (Just kind', fvs) }

rnHsType  :: HsDocContext -> HsType RdrName -> RnM (HsType Name, FreeVars)
rnHsType = rnHsTyKi True
rnHsKind  :: HsDocContext -> HsKind RdrName -> RnM (HsKind Name, FreeVars)
rnHsKind = rnHsTyKi False

rnHsTyKi :: Bool -> HsDocContext -> HsType RdrName -> RnM (HsType Name, FreeVars)

rnHsTyKi isType doc ty@HsForAllTy{}
  = rnHsTyKiForAll isType doc (flattenTopLevelHsForAllTy ty)

rnHsTyKi isType _ (HsTyVar rdr_name)
  = do { name <- rnTyVar isType rdr_name
       ; return (HsTyVar name, unitFV name) }

-- If we see (forall a . ty), without foralls on, the forall will give
-- a sensible error message, but we don't want to complain about the dot too
-- Hence the jiggery pokery with ty1
rnHsTyKi isType doc ty@(HsOpTy ty1 (wrapper, L loc op) ty2)
  = ASSERT( isType ) setSrcSpan loc $
    do  { ops_ok <- xoptM Opt_TypeOperators
        ; op' <- if ops_ok
                 then rnTyVar isType op
                 else do { addErr (opTyErr op ty)
                         ; return (mkUnboundName op) }  -- Avoid double complaint
        ; let l_op' = L loc op'
        ; fix <- lookupTyFixityRn l_op'
        ; (ty1', fvs1) <- rnLHsType doc ty1
        ; (ty2', fvs2) <- rnLHsType doc ty2
        ; res_ty <- mkHsOpTyRn (\t1 t2 -> HsOpTy t1 (wrapper, l_op') t2)
                               op' fix ty1' ty2'
        ; return (res_ty, (fvs1 `plusFV` fvs2) `addOneFV` op') }

rnHsTyKi isType doc (HsParTy ty)
  = do { (ty', fvs) <- rnLHsTyKi isType doc ty
       ; return (HsParTy ty', fvs) }

rnHsTyKi isType doc (HsBangTy b ty)
  = ASSERT( isType )
    do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsBangTy b ty', fvs) }

rnHsTyKi _ doc ty@(HsRecTy flds)
  = do { addErr (hang (ptext (sLit "Record syntax is illegal here:"))
                    2 (ppr ty))
       ; (flds', fvs) <- rnConDeclFields doc flds
       ; return (HsRecTy flds', fvs) }

rnHsTyKi isType doc (HsFunTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi isType doc ty1
        -- Might find a for-all as the arg of a function type
       ; (ty2', fvs2) <- rnLHsTyKi isType doc ty2
        -- Or as the result.  This happens when reading Prelude.hi
        -- when we find return :: forall m. Monad m -> forall a. a -> m a

        -- Check for fixity rearrangements
       ; res_ty <- if isType
                   then mkHsOpTyRn HsFunTy funTyConName funTyFixity ty1' ty2'
                   else return (HsFunTy ty1' ty2')
       ; return (res_ty, fvs1 `plusFV` fvs2) }

rnHsTyKi isType doc listTy@(HsListTy ty)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; unless (data_kinds || isType) (addErr (dataKindsErr isType listTy))
       ; (ty', fvs) <- rnLHsTyKi isType doc ty
       ; return (HsListTy ty', fvs) }

rnHsTyKi isType doc (HsKindSig ty k)
  = ASSERT( isType )
    do { kind_sigs_ok <- xoptM Opt_KindSignatures
       ; unless kind_sigs_ok (badSigErr False doc ty)
       ; (ty', fvs1) <- rnLHsType doc ty
       ; (k', fvs2) <- rnLHsKind doc k
       ; return (HsKindSig ty' k', fvs1 `plusFV` fvs2) }

rnHsTyKi isType doc (HsPArrTy ty)
  = ASSERT( isType )
    do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsPArrTy ty', fvs) }

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsTyKi isType doc tupleTy@(HsTupleTy tup_con tys)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; unless (data_kinds || isType) (addErr (dataKindsErr isType tupleTy))
       ; (tys', fvs) <- mapFvRn (rnLHsTyKi isType doc) tys
       ; return (HsTupleTy tup_con tys', fvs) }

-- Ensure that a type-level integer is nonnegative (#8306, #8412)
rnHsTyKi isType _ tyLit@(HsTyLit t)
  = do { data_kinds <- xoptM Opt_DataKinds
       ; unless data_kinds (addErr (dataKindsErr isType tyLit))
       ; when (negLit t) (addErr negLitErr)
       ; return (HsTyLit t, emptyFVs) }
  where
    negLit (HsStrTy _ _) = False
    negLit (HsNumTy _ i) = i < 0
    negLitErr = ptext (sLit "Illegal literal in type (type literals must not be negative):") <+> ppr tyLit

rnHsTyKi isType doc (HsAppTy ty1 ty2)
  = do { (ty1', fvs1) <- rnLHsTyKi isType doc ty1
       ; (ty2', fvs2) <- rnLHsTyKi isType doc ty2
       ; return (HsAppTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi isType doc (HsIParamTy n ty)
  = ASSERT( isType )
    do { (ty', fvs) <- rnLHsType doc ty
       ; return (HsIParamTy n ty', fvs) }

rnHsTyKi isType doc (HsEqTy ty1 ty2)
  = ASSERT( isType )
    do { (ty1', fvs1) <- rnLHsType doc ty1
       ; (ty2', fvs2) <- rnLHsType doc ty2
       ; return (HsEqTy ty1' ty2', fvs1 `plusFV` fvs2) }

rnHsTyKi isType _ (HsSpliceTy sp k)
  = ASSERT( isType )
    rnSpliceType sp k

rnHsTyKi isType doc (HsDocTy ty haddock_doc)
  = ASSERT( isType )
    do { (ty', fvs) <- rnLHsType doc ty
       ; haddock_doc' <- rnLHsDoc haddock_doc
       ; return (HsDocTy ty' haddock_doc', fvs) }

rnHsTyKi isType _ (HsCoreTy ty)
  = ASSERT( isType )
    return (HsCoreTy ty, emptyFVs)
    -- The emptyFVs probably isn't quite right
    -- but I don't think it matters

rnHsTyKi _ _ (HsWrapTy {})
  = panic "rnHsTyKi"

rnHsTyKi isType doc ty@(HsExplicitListTy k tys)
  = ASSERT( isType )
    do { data_kinds <- xoptM Opt_DataKinds
       ; unless data_kinds (addErr (dataKindsErr isType ty))
       ; (tys', fvs) <- rnLHsTypes doc tys
       ; return (HsExplicitListTy k tys', fvs) }

rnHsTyKi isType doc ty@(HsExplicitTupleTy kis tys)
  = ASSERT( isType )
    do { data_kinds <- xoptM Opt_DataKinds
       ; unless data_kinds (addErr (dataKindsErr isType ty))
       ; (tys', fvs) <- rnLHsTypes doc tys
       ; return (HsExplicitTupleTy kis tys', fvs) }

rnHsTyKi isType _doc (HsWildCardTy (AnonWildCard PlaceHolder))
  = ASSERT( isType )
    do { loc <- getSrcSpanM
       ; uniq <- newUnique
       ; let name = mkInternalName uniq (mkTyVarOcc "_") loc
       ; return (HsWildCardTy (AnonWildCard name), emptyFVs) }
         -- emptyFVs: this occurrence does not refer to a
         --           binding, so don't treat it as a free variable

rnHsTyKi isType doc (HsWildCardTy (NamedWildCard rdr_name))
  = ASSERT( isType )
    do { not_in_scope <- isNothing `fmap` lookupOccRn_maybe rdr_name
       ; when not_in_scope $
         -- When the named wild card is not in scope, it means it shouldn't be
         -- there in the first place, i.e. rnHsSigTypeWithWildCards wasn't
         -- used, so fail.
         failWith $ text "Unexpected wild card:" <+> quotes (ppr rdr_name) $$
                    docOfHsDocContext doc
       ; name <- rnTyVar isType rdr_name
       ; return (HsWildCardTy (NamedWildCard name), emptyFVs) }
         -- emptyFVs: this occurrence does not refer to a
         --           binding, so don't treat it as a free variable

--------------
rnHsTyKiForAll :: Bool -> HsDocContext -> HsType RdrName
               -> RnM (HsType Name, FreeVars)
rnHsTyKiForAll isType doc (HsForAllTy Implicit extra _ lctxt@(L _ ctxt) ty)
  = ASSERT( isType ) do
        -- Implicit quantifiction in source code (no kinds on tyvars)
        -- Given the signature  C => T  we universally quantify
        -- over FV(T) \ {in-scope-tyvars}
    rdr_env <- getLocalRdrEnv
    loc <- getSrcSpanM
    let
        (forall_kvs, forall_tvs) = filterInScope rdr_env $
                                   extractHsTysRdrTyVars (ty:ctxt)
           -- In for-all types we don't bring in scope
           -- kind variables mentioned in kind signatures
           -- (Well, not yet anyway....)
           --    f :: Int -> T (a::k)    -- Not allowed

           -- The filterInScope is to ensure that we don't quantify over
           -- type variables that are in scope; when GlasgowExts is off,
           -- there usually won't be any, except for class signatures:
           --   class C a where { op :: a -> a }
        tyvar_bndrs = userHsTyVarBndrs loc forall_tvs

    rnForAll doc Implicit extra forall_kvs (mkHsQTvs tyvar_bndrs) lctxt ty

rnHsTyKiForAll isType doc
               fulltype@(HsForAllTy Qualified extra _ lctxt@(L _ ctxt) ty)
  = ASSERT( isType ) do
    rdr_env <- getLocalRdrEnv
    loc <- getSrcSpanM
    let
        (forall_kvs, forall_tvs) = filterInScope rdr_env $
                                   extractHsTysRdrTyVars (ty:ctxt)
        tyvar_bndrs = userHsTyVarBndrs loc forall_tvs
        in_type_doc = ptext (sLit "In the type") <+> quotes (ppr fulltype)

    -- See Note [Context quantification]
    warnContextQuantification (in_type_doc $$ docOfHsDocContext doc) tyvar_bndrs
    rnForAll doc Implicit extra forall_kvs (mkHsQTvs tyvar_bndrs) lctxt ty

rnHsTyKiForAll isType doc
               ty@(HsForAllTy Explicit extra forall_tyvars lctxt@(L _ ctxt) tau)
  = ASSERT( isType ) do {      -- Explicit quantification.
         -- Check that the forall'd tyvars are actually
         -- mentioned in the type, and produce a warning if not
         let (kvs, mentioned) = extractHsTysRdrTyVars (tau:ctxt)
             in_type_doc = ptext (sLit "In the type") <+> quotes (ppr ty)
       ; warnUnusedForAlls (in_type_doc $$ docOfHsDocContext doc)
                           forall_tyvars mentioned
       ; traceRn (text "rnHsTyKiForAll:Exlicit" <+> vcat
            [ppr forall_tyvars, ppr lctxt,ppr tau ])
       ; rnForAll doc Explicit extra kvs forall_tyvars lctxt tau }

-- The following should never happen but keeps the completeness checker happy
rnHsTyKiForAll isType doc ty = rnHsTyKi isType doc ty
--------------
rnTyVar :: Bool -> RdrName -> RnM Name
rnTyVar is_type rdr_name
  | is_type   = lookupTypeOccRn rdr_name
  | otherwise = lookupKindOccRn rdr_name

rnLTyVar :: Bool -> Located RdrName -> RnM (Located Name)
rnLTyVar is_type (L loc rdr_name) = do
  tyvar' <- rnTyVar is_type rdr_name
  return (L loc tyvar')

--------------
rnLHsTypes :: HsDocContext -> [LHsType RdrName]
           -> RnM ([LHsType Name], FreeVars)
rnLHsTypes doc tys = mapFvRn (rnLHsType doc) tys

rnForAll :: HsDocContext -> HsExplicitFlag
         -> Maybe SrcSpan           -- Location of an extra-constraints wildcard
         -> [RdrName]               -- Kind variables
         -> LHsTyVarBndrs RdrName   -- Type variables
         -> LHsContext RdrName -> LHsType RdrName
         -> RnM (HsType Name, FreeVars)

rnForAll doc exp extra kvs forall_tyvars ctxt ty
  | null kvs, null (hsQTvBndrs forall_tyvars), null (unLoc ctxt), isNothing extra
  = rnHsType doc (unLoc ty)
        -- One reason for this case is that a type like Int#
        -- starts off as (HsForAllTy Implicit Nothing [] Int), in case
        -- there is some quantification.  Now that we have quantified
        -- and discovered there are no type variables, it's nicer to turn
        -- it into plain Int.  If it were Int# instead of Int, we'd actually
        -- get an error, because the body of a genuine for-all is
        -- of kind *.

  | otherwise
  = bindHsTyVars doc Nothing kvs forall_tyvars $ \ new_tyvars ->
    do { (new_ctxt, fvs1) <- rnContext doc ctxt
       ; (new_ty, fvs2) <- rnLHsType doc ty
       ; return (HsForAllTy exp extra new_tyvars new_ctxt new_ty, fvs1 `plusFV` fvs2) }
        -- Retain the same implicit/explicit flag as before
        -- so that we can later print it correctly

---------------
bindSigTyVarsFV :: [Name]
                -> RnM (a, FreeVars)
                -> RnM (a, FreeVars)
-- Used just before renaming the defn of a function
-- with a separate type signature, to bring its tyvars into scope
-- With no -XScopedTypeVariables, this is a no-op
bindSigTyVarsFV tvs thing_inside
  = do  { scoped_tyvars <- xoptM Opt_ScopedTypeVariables
        ; if not scoped_tyvars then
                thing_inside
          else
                bindLocalNamesFV tvs thing_inside }

---------------
bindHsTyVars :: HsDocContext
             -> Maybe a                 -- Just _  => an associated type decl
             -> [RdrName]               -- Kind variables from scope
             -> LHsTyVarBndrs RdrName   -- Type variables
             -> (LHsTyVarBndrs Name -> RnM (b, FreeVars))
             -> RnM (b, FreeVars)
-- (a) Bring kind variables into scope
--     both (i)  passed in (kv_bndrs)
--     and  (ii) mentioned in the kinds of tv_bndrs
-- (b) Bring type variables into scope
bindHsTyVars doc mb_assoc kv_bndrs tv_bndrs thing_inside
  = do { rdr_env <- getLocalRdrEnv
       ; let tvs = hsQTvBndrs tv_bndrs
             kvs_from_tv_bndrs = [ kv | L _ (KindedTyVar _ kind) <- tvs
                                 , let (_, kvs) = extractHsTyRdrTyVars kind
                                 , kv <- kvs ]
             all_kvs' = nub (kv_bndrs ++ kvs_from_tv_bndrs)
             all_kvs  = filterOut (`elemLocalRdrEnv` rdr_env) all_kvs'

             overlap_kvs = [ kv | kv <- all_kvs, any ((==) kv . hsLTyVarName) tvs ]
                -- These variables appear both as kind and type variables
                -- in the same declaration; eg  type family  T (x :: *) (y :: x)
                -- We disallow this: too confusing!

       ; poly_kind <- xoptM Opt_PolyKinds
       ; unless (poly_kind || null all_kvs)
                (addErr (badKindBndrs doc all_kvs))
       ; unless (null overlap_kvs)
                (addErr (overlappingKindVars doc overlap_kvs))

       ; loc <- getSrcSpanM
       ; kv_names <- mapM (newLocalBndrRn . L loc) all_kvs
       ; bindLocalNamesFV kv_names $
    do { let tv_names_w_loc = hsLTyVarLocNames tv_bndrs

       -- Check for duplicate or shadowed tyvar bindrs
       ; checkDupRdrNames tv_names_w_loc
       ; when (isNothing mb_assoc) (checkShadowedRdrNames tv_names_w_loc)

       ; (tv_bndrs', fvs1) <- mapFvRn (rnLHsTyVarBndr doc mb_assoc rdr_env) tvs
       ; (res, fvs2) <- bindLocalNamesFV (map hsLTyVarName tv_bndrs') $
                        do { inner_rdr_env <- getLocalRdrEnv
                           ; traceRn (text "bhtv" <+> vcat
                                 [ ppr tvs, ppr kv_bndrs, ppr kvs_from_tv_bndrs
                                 , ppr $ map (`elemLocalRdrEnv` rdr_env) all_kvs'
                                 , ppr $ map (getUnique . rdrNameOcc) all_kvs'
                                 , ppr all_kvs, ppr rdr_env, ppr inner_rdr_env ])
                           ; thing_inside (HsQTvs { hsq_tvs = tv_bndrs', hsq_kvs = kv_names }) }
       ; return (res, fvs1 `plusFV` fvs2) } }

rnLHsTyVarBndr :: HsDocContext -> Maybe a -> LocalRdrEnv
               -> LHsTyVarBndr RdrName -> RnM (LHsTyVarBndr Name, FreeVars)
rnLHsTyVarBndr _ mb_assoc rdr_env (L loc (UserTyVar rdr))
  = do { nm <- newTyVarNameRn mb_assoc rdr_env loc rdr
       ; return (L loc (UserTyVar nm), emptyFVs) }
rnLHsTyVarBndr doc mb_assoc rdr_env (L loc (KindedTyVar (L lv rdr) kind))
  = do { sig_ok <- xoptM Opt_KindSignatures
       ; unless sig_ok (badSigErr False doc kind)
       ; nm <- newTyVarNameRn mb_assoc rdr_env loc rdr
       ; (kind', fvs) <- rnLHsKind doc kind
       ; return (L loc (KindedTyVar (L lv nm) kind'), fvs) }

newTyVarNameRn :: Maybe a -> LocalRdrEnv -> SrcSpan -> RdrName -> RnM Name
newTyVarNameRn mb_assoc rdr_env loc rdr
  | Just _ <- mb_assoc    -- Use the same Name as the parent class decl
  , Just n <- lookupLocalRdrEnv rdr_env rdr
  = return n
  | otherwise
  = newLocalBndrRn (L loc rdr)

--------------------------------
rnHsBndrSig :: HsDocContext
            -> HsWithBndrs RdrName (LHsType RdrName)
            -> (HsWithBndrs Name (LHsType Name) -> RnM (a, FreeVars))
            -> RnM (a, FreeVars)
rnHsBndrSig doc (HsWB { hswb_cts = ty@(L loc _) }) thing_inside
  = do { sig_ok <- xoptM Opt_ScopedTypeVariables
       ; unless sig_ok (badSigErr True doc ty)
       ; rdr_env <- getLocalRdrEnv
       ; let (kv_bndrs, tv_bndrs) = filterInScope rdr_env $
                                    extractHsTyRdrTyVars ty
       ; kv_names <- newLocalBndrsRn (map (L loc) kv_bndrs)
       ; tv_names <- newLocalBndrsRn (map (L loc) tv_bndrs)
       ; bindLocalNamesFV kv_names $
         bindLocalNamesFV tv_names $
    do { (ty', fvs1, wcs) <- rnLHsTypeWithWildCards doc ty
       ; (res, fvs2) <- thing_inside (HsWB { hswb_cts = ty'
                                           , hswb_kvs = kv_names
                                           , hswb_tvs = tv_names
                                           , hswb_wcs = wcs })
       ; return (res, fvs1 `plusFV` fvs2) } }

overlappingKindVars :: HsDocContext -> [RdrName] -> SDoc
overlappingKindVars doc kvs
  = vcat [ ptext (sLit "Kind variable") <> plural kvs <+>
           ptext (sLit "also used as type variable") <> plural kvs
           <> colon <+> pprQuotedList kvs
         , docOfHsDocContext doc ]

badKindBndrs :: HsDocContext -> [RdrName] -> SDoc
badKindBndrs doc kvs
  = vcat [ hang (ptext (sLit "Unexpected kind variable") <> plural kvs
                 <+> pprQuotedList kvs)
              2 (ptext (sLit "Perhaps you intended to use PolyKinds"))
         , docOfHsDocContext doc ]

badSigErr :: Bool -> HsDocContext -> LHsType RdrName -> TcM ()
badSigErr is_type doc (L loc ty)
  = setSrcSpan loc $ addErr $
    vcat [ hang (ptext (sLit "Illegal") <+> what
                 <+> ptext (sLit "signature:") <+> quotes (ppr ty))
              2 (ptext (sLit "Perhaps you intended to use") <+> flag)
         , docOfHsDocContext doc ]
  where
    what | is_type   = ptext (sLit "type")
         | otherwise = ptext (sLit "kind")
    flag | is_type   = ptext (sLit "ScopedTypeVariables")
         | otherwise = ptext (sLit "KindSignatures")

dataKindsErr :: Bool -> HsType RdrName -> SDoc
dataKindsErr is_type thing
  = hang (ptext (sLit "Illegal") <+> what <> colon <+> quotes (ppr thing))
       2 (ptext (sLit "Perhaps you intended to use DataKinds"))
  where
    what | is_type   = ptext (sLit "type")
         | otherwise = ptext (sLit "kind")

--------------------------------
-- | Variant of @rnHsSigType@ that supports wild cards. Also returns the wild
-- cards to bind.
rnHsSigTypeWithWildCards :: SDoc -> LHsType RdrName
                         -> RnM (LHsType Name, FreeVars, [Name])
rnHsSigTypeWithWildCards doc_str ty
  = rnLHsTypeWithWildCards (TypeSigCtx doc_str) ty'
  where
    ty' = extractExtraCtsWc `fmap` flattenTopLevelLHsForAllTy ty
    -- When there is a wild card at the end of the context, remove it and add
    -- its location as the extra-constraints wild card in the HsForAllTy.
    extractExtraCtsWc (HsForAllTy flag _ bndrs (L l ctxt) ty)
      | Just (ctxt', ct) <- snocView ctxt
      , L lx (HsWildCardTy (AnonWildCard _)) <- ignoreParens ct
      = HsForAllTy flag (Just lx) bndrs (L l ctxt') ty
    extractExtraCtsWc ty = ty

-- | Variant of @rnLHsType@ that supports wild cards. The third element of the
-- tuple consists of the freshly generated names of the anonymous wild cards
-- occurring in the type, as well as the names of the named wild cards in the
-- type that are not yet in scope.
rnLHsTypeWithWildCards  :: HsDocContext -> LHsType RdrName
                        -> RnM (LHsType Name, FreeVars, [Name])
rnLHsTypeWithWildCards doc ty
  = do { checkValidPartialType doc ty
       ; rdr_env <- getLocalRdrEnv
       -- Filter out named wildcards that are already in scope
       ; let (_, wcs) = collectWildCards ty
             nwcs = [L loc n | L loc (NamedWildCard n) <- wcs
                             , not (elemLocalRdrEnv n rdr_env) ]
       ; bindLocatedLocalsRn nwcs $ \nwcs' -> do {
         (ty', fvs) <- rnLHsType doc ty
       -- Add the anonymous wildcards that have been given names during
       -- renaming
       ; let (_, wcs') = collectWildCards ty'
             awcs      = filter (isAnonWildCard . unLoc) wcs'
       ; return (ty', fvs, nwcs' ++ map (HsSyn.wildCardName . unLoc) awcs) } }

-- | Extract all wild cards from a type. The named and anonymous
-- extra-constraints wild cards are returned separately to be able to give
-- more accurate error messages.
collectWildCards
  :: Eq name => LHsType name
  -> ([Located (HsWildCardInfo name)],  -- extra-constraints wild cards
      [Located (HsWildCardInfo name)])  -- wild cards
collectWildCards lty = (extra, nubBy sameNamedWildCard wcs)
  where
    (extra, wcs) = go lty
    go (L loc ty) = case ty of
      HsAppTy ty1 ty2         -> go ty1 `mappend` go ty2
      HsFunTy ty1 ty2         -> go ty1 `mappend` go ty2
      HsListTy ty             -> go ty
      HsPArrTy ty             -> go ty
      HsTupleTy _ tys         -> gos tys
      HsOpTy ty1 _ ty2        -> go ty1 `mappend` go ty2
      HsParTy ty              -> go ty
      HsIParamTy _ ty         -> go ty
      HsEqTy ty1 ty2          -> go ty1 `mappend` go ty2
      HsKindSig ty kind       -> go ty `mappend` go kind
      HsDocTy ty _            -> go ty
      HsBangTy _ ty           -> go ty
      HsRecTy flds            -> gos $ map (cd_fld_type . unLoc) flds
      HsExplicitListTy _ tys  -> gos tys
      HsExplicitTupleTy _ tys -> gos tys
      HsWrapTy _ ty           -> go (L loc ty)
      -- Interesting cases
      HsWildCardTy wc         -> ([], [L loc wc])
      HsForAllTy _ _ _ (L _ ctxt) ty -> ctxtWcs `mappend` go ty
        where
          ctxt' = map ignoreParens ctxt
          extraWcs  = [L l wc | L l (HsWildCardTy wc) <- ctxt']
          (_, wcs) = gos ctxt'
          -- Remove extra-constraints wild cards from wcs
          ctxtWcs = (extraWcs, deleteFirstsBy sameWildCard
                               (nubBy sameWildCard wcs) extraWcs)
      -- HsQuasiQuoteTy, HsSpliceTy, HsCoreTy, HsTyLit
      _ -> mempty
    gos = mconcat . map go

-- | Check the validity of a partial type signature. The following things are
-- checked:
--
-- * Named extra-constraints wild cards aren't allowed,
-- e.g. invalid: @(Show a, _x) => a -> String@.
--
-- * There is only one extra-constraints wild card in the context and it must
-- come last, e.g. invalid: @(_, Show a) => a -> String@
-- or @(_, Show a, _) => a -> String@.
--
-- * There should be no unnamed wild cards in the context.
--
-- * An extra-constraints wild card can only occur in the top-level context.
-- This would be invalid: @(Eq a, _) => a -> (Num a, _) => a -> Bool@.
--
-- * Named wild cards occurring in the context must also occur in the monotype.
--
-- When an invalid wild card is found, we fail with an error.
checkValidPartialType :: HsDocContext -> LHsType RdrName -> RnM ()
checkValidPartialType doc lty
  = do { whenNonEmpty isNamedWildCard inExtra $ \(L loc _) ->
           failAt loc $ typeDoc $$
           text "An extra-constraints wild card cannot be named" $$
           docOfHsDocContext doc

       ; whenNonEmpty isAnonWildCard extraTopLevel $ \(L loc _) ->
           failAt loc $ typeDoc $$
           -- If there was a valid extra-constraints wild card, it should have
           -- already been removed and its location should be stored in the
           -- HsForAllTy
           (case extra of
             Just _ ->
               -- We're in a top-level context with an extracted
               -- extra-constraints wild card.
               text "Only a single extra-constraints wild card is allowed"
             _ | TypeSigCtx _ <- doc ->
               -- We're in a top-level context, but the extra-constraints wild
               -- card didn't occur at the end.
               fcat [ text "An extra-constraints wild card must occur"
                    , text "at the end of the constraints" ]
             _ ->
               -- We're not in a top-level context, so no extra-constraints
               -- wild cards are supported.
               fcat [ text "An extra-constraints wild card is only allowed"
                    , text "in the top-level context" ]) $$
           docOfHsDocContext doc

       ; whenNonEmpty isAnonWildCard inCtxt $ \(L loc _) ->
           failAt loc $ typeDoc $$
           text "Anonymous wild cards are not allowed in constraints" $$
           docOfHsDocContext doc

       ; whenNonEmpty isAnonWildCard nestedExtra $ \(L loc _) ->
           failAt loc $ typeDoc $$
           fcat [ text "An extra-constraints wild card is only allowed"
                , text "in the top-level context" ] $$
           docOfHsDocContext doc

       ; whenNonEmpty isNamedWildCard inCtxtNotInTau $ \(L loc name) ->
           failAt loc $ typeDoc $$
           fcat [ text "The named wild card" <+> quotes (ppr name) <> space
                , text "is only allowed in the constraints"
                , text "when it also occurs in the rest of the type" ] $$
           docOfHsDocContext doc }
  where
    typeDoc               = hang (text "Invalid partial type:") 2 (ppr lty)
    (extra, ctxt, tau)    = splitPartialType lty
    (inExtra,     _)      = collectWildCards lty
    (nestedExtra, inTau)  = collectWildCards tau
    (_,           inCtxt) = mconcat $ map collectWildCards ctxt
    inCtxtNotInTau        = deleteFirstsBy sameWildCard inCtxt inTau
    extraTopLevel         = deleteFirstsBy sameWildCard inExtra nestedExtra

    splitPartialType (L _ (HsForAllTy _ extra _ (L _ ctxt) ty))
      = (extra, map ignoreParens ctxt, ty)
    splitPartialType ty = (Nothing, [], ty)

    whenNonEmpty test wcs f
      = whenIsJust (listToMaybe $ filter (test . unLoc) wcs) f


{-
*********************************************************
*                                                      *
\subsection{Contexts and predicates}
*                                                      *
*********************************************************
-}

rnConDeclFields :: HsDocContext -> [LConDeclField RdrName]
                -> RnM ([LConDeclField Name], FreeVars)
rnConDeclFields doc fields = mapFvRn (rnField doc) fields

rnField :: HsDocContext -> LConDeclField RdrName
        -> RnM (LConDeclField Name, FreeVars)
rnField doc (L l (ConDeclField names ty haddock_doc))
  = do { new_names <- mapM lookupLocatedTopBndrRn names
       ; (new_ty, fvs) <- rnLHsType doc ty
       ; new_haddock_doc <- rnMbLHsDoc haddock_doc
       ; return (L l (ConDeclField new_names new_ty new_haddock_doc), fvs) }

rnContext :: HsDocContext -> LHsContext RdrName -> RnM (LHsContext Name, FreeVars)
rnContext doc (L loc cxt)
  = do { (cxt', fvs) <- rnLHsTypes doc cxt
       ; return (L loc cxt', fvs) }

{-
************************************************************************
*                                                                      *
        Fixities and precedence parsing
*                                                                      *
************************************************************************

@mkOpAppRn@ deals with operator fixities.  The argument expressions
are assumed to be already correctly arranged.  It needs the fixities
recorded in the OpApp nodes, because fixity info applies to the things
the programmer actually wrote, so you can't find it out from the Name.

Furthermore, the second argument is guaranteed not to be another
operator application.  Why? Because the parser parses all
operator appications left-associatively, EXCEPT negation, which
we need to handle specially.
Infix types are read in a *right-associative* way, so that
        a `op` b `op` c
is always read in as
        a `op` (b `op` c)

mkHsOpTyRn rearranges where necessary.  The two arguments
have already been renamed and rearranged.  It's made rather tiresome
by the presence of ->, which is a separate syntactic construct.
-}

---------------
-- Building (ty1 `op1` (ty21 `op2` ty22))
mkHsOpTyRn :: (LHsType Name -> LHsType Name -> HsType Name)
           -> Name -> Fixity -> LHsType Name -> LHsType Name
           -> RnM (HsType Name)

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsOpTy ty21 (w2, op2) ty22))
  = do  { fix2 <- lookupTyFixityRn op2
        ; mk_hs_op_ty mk1 pp_op1 fix1 ty1
                      (\t1 t2 -> HsOpTy t1 (w2, op2) t2)
                      (unLoc op2) fix2 ty21 ty22 loc2 }

mkHsOpTyRn mk1 pp_op1 fix1 ty1 (L loc2 (HsFunTy ty21 ty22))
  = mk_hs_op_ty mk1 pp_op1 fix1 ty1
                HsFunTy funTyConName funTyFixity ty21 ty22 loc2

mkHsOpTyRn mk1 _ _ ty1 ty2              -- Default case, no rearrangment
  = return (mk1 ty1 ty2)

---------------
mk_hs_op_ty :: (LHsType Name -> LHsType Name -> HsType Name)
            -> Name -> Fixity -> LHsType Name
            -> (LHsType Name -> LHsType Name -> HsType Name)
            -> Name -> Fixity -> LHsType Name -> LHsType Name -> SrcSpan
            -> RnM (HsType Name)
mk_hs_op_ty mk1 op1 fix1 ty1
            mk2 op2 fix2 ty21 ty22 loc2
  | nofix_error     = do { precParseErr (op1,fix1) (op2,fix2)
                         ; return (mk1 ty1 (L loc2 (mk2 ty21 ty22))) }
  | associate_right = return (mk1 ty1 (L loc2 (mk2 ty21 ty22)))
  | otherwise       = do { -- Rearrange to ((ty1 `op1` ty21) `op2` ty22)
                           new_ty <- mkHsOpTyRn mk1 op1 fix1 ty1 ty21
                         ; return (mk2 (noLoc new_ty) ty22) }
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2


---------------------------
mkOpAppRn :: LHsExpr Name                       -- Left operand; already rearranged
          -> LHsExpr Name -> Fixity             -- Operator and fixity
          -> LHsExpr Name                       -- Right operand (not an OpApp, but might
                                                -- be a NegApp)
          -> RnM (HsExpr Name)

-- (e11 `op1` e12) `op2` e2
mkOpAppRn e1@(L _ (OpApp e11 op1 fix1 e12)) op2 fix2 e2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (OpApp e1 op2 fix2 e2)

  | associate_right = do
    new_e <- mkOpAppRn e12 op2 fix2 e2
    return (OpApp e11 op1 fix1 (L loc' new_e))
  where
    loc'= combineLocs e12 e2
    (nofix_error, associate_right) = compareFixity fix1 fix2

---------------------------
--      (- neg_arg) `op` e2
mkOpAppRn e1@(L _ (NegApp neg_arg neg_name)) op2 fix2 e2
  | nofix_error
  = do precParseErr (negateName,negateFixity) (get_op op2,fix2)
       return (OpApp e1 op2 fix2 e2)

  | associate_right
  = do new_e <- mkOpAppRn neg_arg op2 fix2 e2
       return (NegApp (L loc' new_e) neg_name)
  where
    loc' = combineLocs neg_arg e2
    (nofix_error, associate_right) = compareFixity negateFixity fix2

---------------------------
--      e1 `op` - neg_arg
mkOpAppRn e1 op1 fix1 e2@(L _ (NegApp _ _))     -- NegApp can occur on the right
  | not associate_right                 -- We *want* right association
  = do precParseErr (get_op op1, fix1) (negateName, negateFixity)
       return (OpApp e1 op1 fix1 e2)
  where
    (_, associate_right) = compareFixity fix1 negateFixity

---------------------------
--      Default case
mkOpAppRn e1 op fix e2                  -- Default case, no rearrangment
  = ASSERT2( right_op_ok fix (unLoc e2),
             ppr e1 $$ text "---" $$ ppr op $$ text "---" $$ ppr fix $$ text "---" $$ ppr e2
    )
    return (OpApp e1 op fix e2)

----------------------------
get_op :: LHsExpr Name -> Name
-- An unbound name could be either HsVar or HsUnboundVra
-- See RnExpr.rnUnboundVar
get_op (L _ (HsVar n))          = n
get_op (L _ (HsUnboundVar occ)) = mkUnboundName (mkRdrUnqual occ)
get_op other                    = pprPanic "get_op" (ppr other)

-- Parser left-associates everything, but
-- derived instances may have correctly-associated things to
-- in the right operarand.  So we just check that the right operand is OK
right_op_ok :: Fixity -> HsExpr Name -> Bool
right_op_ok fix1 (OpApp _ _ fix2 _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok _ _
  = True

-- Parser initially makes negation bind more tightly than any other operator
-- And "deriving" code should respect this (use HsPar if not)
mkNegAppRn :: LHsExpr id -> SyntaxExpr id -> RnM (HsExpr id)
mkNegAppRn neg_arg neg_name
  = ASSERT( not_op_app (unLoc neg_arg) )
    return (NegApp neg_arg neg_name)

not_op_app :: HsExpr id -> Bool
not_op_app (OpApp _ _ _ _) = False
not_op_app _               = True

---------------------------
mkOpFormRn :: LHsCmdTop Name            -- Left operand; already rearranged
          -> LHsExpr Name -> Fixity     -- Operator and fixity
          -> LHsCmdTop Name             -- Right operand (not an infix)
          -> RnM (HsCmd Name)

-- (e11 `op1` e12) `op2` e2
mkOpFormRn a1@(L loc (HsCmdTop (L _ (HsCmdArrForm op1 (Just fix1) [a11,a12])) _ _ _))
        op2 fix2 a2
  | nofix_error
  = do precParseErr (get_op op1,fix1) (get_op op2,fix2)
       return (HsCmdArrForm op2 (Just fix2) [a1, a2])

  | associate_right
  = do new_c <- mkOpFormRn a12 op2 fix2 a2
       return (HsCmdArrForm op1 (Just fix1)
               [a11, L loc (HsCmdTop (L loc new_c)
               placeHolderType placeHolderType [])])
        -- TODO: locs are wrong
  where
    (nofix_error, associate_right) = compareFixity fix1 fix2

--      Default case
mkOpFormRn arg1 op fix arg2                     -- Default case, no rearrangment
  = return (HsCmdArrForm op (Just fix) [arg1, arg2])


--------------------------------------
mkConOpPatRn :: Located Name -> Fixity -> LPat Name -> LPat Name
             -> RnM (Pat Name)

mkConOpPatRn op2 fix2 p1@(L loc (ConPatIn op1 (InfixCon p11 p12))) p2
  = do  { fix1 <- lookupFixityRn (unLoc op1)
        ; let (nofix_error, associate_right) = compareFixity fix1 fix2

        ; if nofix_error then do
                { precParseErr (unLoc op1,fix1) (unLoc op2,fix2)
                ; return (ConPatIn op2 (InfixCon p1 p2)) }

          else if associate_right then do
                { new_p <- mkConOpPatRn op2 fix2 p12 p2
                ; return (ConPatIn op1 (InfixCon p11 (L loc new_p))) } -- XXX loc right?
          else return (ConPatIn op2 (InfixCon p1 p2)) }

mkConOpPatRn op _ p1 p2                         -- Default case, no rearrangment
  = ASSERT( not_op_pat (unLoc p2) )
    return (ConPatIn op (InfixCon p1 p2))

not_op_pat :: Pat Name -> Bool
not_op_pat (ConPatIn _ (InfixCon _ _)) = False
not_op_pat _                           = True

--------------------------------------
checkPrecMatch :: Name -> MatchGroup Name body -> RnM ()
  -- Check precedence of a function binding written infix
  --   eg  a `op` b `C` c = ...
  -- See comments with rnExpr (OpApp ...) about "deriving"

checkPrecMatch op (MG { mg_alts = ms })
  = mapM_ check ms
  where
    check (L _ (Match _ (L l1 p1 : L l2 p2 :_) _ _))
      = setSrcSpan (combineSrcSpans l1 l2) $
        do checkPrec op p1 False
           checkPrec op p2 True

    check _ = return ()
        -- This can happen.  Consider
        --      a `op` True = ...
        --      op          = ...
        -- The infix flag comes from the first binding of the group
        -- but the second eqn has no args (an error, but not discovered
        -- until the type checker).  So we don't want to crash on the
        -- second eqn.

checkPrec :: Name -> Pat Name -> Bool -> IOEnv (Env TcGblEnv TcLclEnv) ()
checkPrec op (ConPatIn op1 (InfixCon _ _)) right = do
    op_fix@(Fixity op_prec  op_dir) <- lookupFixityRn op
    op1_fix@(Fixity op1_prec op1_dir) <- lookupFixityRn (unLoc op1)
    let
        inf_ok = op1_prec > op_prec ||
                 (op1_prec == op_prec &&
                  (op1_dir == InfixR && op_dir == InfixR && right ||
                   op1_dir == InfixL && op_dir == InfixL && not right))

        info  = (op,        op_fix)
        info1 = (unLoc op1, op1_fix)
        (infol, infor) = if right then (info, info1) else (info1, info)
    unless inf_ok (precParseErr infol infor)

checkPrec _ _ _
  = return ()

-- Check precedence of (arg op) or (op arg) respectively
-- If arg is itself an operator application, then either
--   (a) its precedence must be higher than that of op
--   (b) its precedency & associativity must be the same as that of op
checkSectionPrec :: FixityDirection -> HsExpr RdrName
        -> LHsExpr Name -> LHsExpr Name -> RnM ()
checkSectionPrec direction section op arg
  = case unLoc arg of
        OpApp _ op fix _ -> go_for_it (get_op op) fix
        NegApp _ _       -> go_for_it negateName  negateFixity
        _                -> return ()
  where
    op_name = get_op op
    go_for_it arg_op arg_fix@(Fixity arg_prec assoc) = do
          op_fix@(Fixity op_prec _) <- lookupFixityRn op_name
          unless (op_prec < arg_prec
                  || (op_prec == arg_prec && direction == assoc))
                 (sectionPrecErr (op_name, op_fix)
                                 (arg_op, arg_fix) section)

-- Precedence-related error messages

precParseErr :: (Name, Fixity) -> (Name, Fixity) -> RnM ()
precParseErr op1@(n1,_) op2@(n2,_)
  | isUnboundName n1 || isUnboundName n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ hang (ptext (sLit "Precedence parsing error"))
      4 (hsep [ptext (sLit "cannot mix"), ppr_opfix op1, ptext (sLit "and"),
               ppr_opfix op2,
               ptext (sLit "in the same infix expression")])

sectionPrecErr :: (Name, Fixity) -> (Name, Fixity) -> HsExpr RdrName -> RnM ()
sectionPrecErr op@(n1,_) arg_op@(n2,_) section
  | isUnboundName n1 || isUnboundName n2
  = return ()     -- Avoid error cascade
  | otherwise
  = addErr $ vcat [ptext (sLit "The operator") <+> ppr_opfix op <+> ptext (sLit "of a section"),
         nest 4 (sep [ptext (sLit "must have lower precedence than that of the operand,"),
                      nest 2 (ptext (sLit "namely") <+> ppr_opfix arg_op)]),
         nest 4 (ptext (sLit "in the section:") <+> quotes (ppr section))]

ppr_opfix :: (Name, Fixity) -> SDoc
ppr_opfix (op, fixity) = pp_op <+> brackets (ppr fixity)
   where
     pp_op | op == negateName = ptext (sLit "prefix `-'")
           | otherwise        = quotes (ppr op)

{-
*********************************************************
*                                                      *
\subsection{Errors}
*                                                      *
*********************************************************
-}

warnUnusedForAlls :: SDoc -> LHsTyVarBndrs RdrName -> [RdrName] -> TcM ()
warnUnusedForAlls in_doc bound mentioned_rdrs
  = whenWOptM Opt_WarnUnusedMatches $
    mapM_ add_warn bound_but_not_used
  where
    bound_names        = hsLTyVarLocNames bound
    bound_but_not_used = filterOut ((`elem` mentioned_rdrs) . unLoc) bound_names

    add_warn (L loc tv)
      = addWarnAt loc $
        vcat [ ptext (sLit "Unused quantified type variable") <+> quotes (ppr tv)
             , in_doc ]

warnContextQuantification :: SDoc -> [LHsTyVarBndr RdrName] -> TcM ()
warnContextQuantification in_doc tvs
  = whenWOptM Opt_WarnContextQuantification $
    mapM_ add_warn tvs
  where
    add_warn (L loc tv)
      = addWarnAt loc $
        vcat [ ptext (sLit "Variable") <+> quotes (ppr tv) <+>
               ptext (sLit "is implicitly quantified due to a context") $$
               ptext (sLit "Use explicit forall syntax instead.") $$
               ptext (sLit "This will become an error in GHC 7.12.")
             , in_doc ]

opTyErr :: RdrName -> HsType RdrName -> SDoc
opTyErr op ty@(HsOpTy ty1 _ _)
  = hang (ptext (sLit "Illegal operator") <+> quotes (ppr op) <+> ptext (sLit "in type") <+> quotes (ppr ty))
         2 extra
  where
    extra | op == dot_tv_RDR && forall_head ty1
          = perhapsForallMsg
          | otherwise
          = ptext (sLit "Use TypeOperators to allow operators in types")

    forall_head (L _ (HsTyVar tv))   = tv == forall_tv_RDR
    forall_head (L _ (HsAppTy ty _)) = forall_head ty
    forall_head _other               = False
opTyErr _ ty = pprPanic "opTyErr: Not an op" (ppr ty)

{-
************************************************************************
*                                                                      *
      Finding the free type variables of a (HsType RdrName)
*                                                                    *
************************************************************************


Note [Kind and type-variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a type signature we may implicitly bind type variable and, more
recently, kind variables.  For example:
  *   f :: a -> a
      f = ...
    Here we need to find the free type variables of (a -> a),
    so that we know what to quantify

  *   class C (a :: k) where ...
    This binds 'k' in ..., as well as 'a'

  *   f (x :: a -> [a]) = ....
    Here we bind 'a' in ....

  *   f (x :: T a -> T (b :: k)) = ...
    Here we bind both 'a' and the kind variable 'k'

  *   type instance F (T (a :: Maybe k)) = ...a...k...
    Here we want to constrain the kind of 'a', and bind 'k'.

In general we want to walk over a type, and find
  * Its free type variables
  * The free kind variables of any kind signatures in the type

Hence we returns a pair (kind-vars, type vars)
See also Note [HsBSig binder lists] in HsTypes
-}

type FreeKiTyVars = ([RdrName], [RdrName])

filterInScope :: LocalRdrEnv -> FreeKiTyVars -> FreeKiTyVars
filterInScope rdr_env (kvs, tvs)
  = (filterOut in_scope kvs, filterOut in_scope tvs)
  where
    in_scope tv = tv `elemLocalRdrEnv` rdr_env

extractHsTyRdrTyVars :: LHsType RdrName -> FreeKiTyVars
-- extractHsTyRdrNames finds the free (kind, type) variables of a HsType
--                        or the free (sort, kind) variables of a HsKind
-- It's used when making the for-alls explicit.
-- See Note [Kind and type-variable binders]
extractHsTyRdrTyVars ty
  = case extract_lty ty ([],[]) of
     (kvs, tvs) -> (nub kvs, nub tvs)

extractHsTysRdrTyVars :: [LHsType RdrName] -> FreeKiTyVars
-- See Note [Kind and type-variable binders]
extractHsTysRdrTyVars ty
  = case extract_ltys ty ([],[]) of
     (kvs, tvs) -> (nub kvs, nub tvs)

-- Extracts variable names used in a type variable binder. Note that HsType
-- represents data and type constructors as type variables and so this function
-- will also return data and type constructors.
extractTyVarBndrNames :: LHsTyVarBndr RdrName -> Set.Set RdrName
extractTyVarBndrNames (L _ (UserTyVar name))
  = Set.singleton name
extractTyVarBndrNames (L _ (KindedTyVar (L _ name) k))
  = Set.singleton name `Set.union` (Set.fromList tvs)
                       `Set.union` (Set.fromList kvs)
    where (kvs, tvs) = extractHsTyRdrTyVars k

extractRdrKindSigVars :: LFamilyResultSig RdrName -> [RdrName]
extractRdrKindSigVars (L _ resultSig)
    | KindSig k                        <- resultSig = kindRdrNameFromSig k
    | TyVarSig (L _ (KindedTyVar _ k)) <- resultSig = kindRdrNameFromSig k
    | TyVarSig (L _ (UserTyVar _))     <- resultSig = []
    | otherwise = [] -- this can only be NoSig but pattern exhasutiveness
                     -- checker complains about "NoSig <- resultSig"
    where kindRdrNameFromSig k = nub (fst (extract_lkind k ([],[])))

extractDataDefnKindVars :: HsDataDefn RdrName -> [RdrName]
-- Get the scoped kind variables mentioned free in the constructor decls
-- Eg    data T a = T1 (S (a :: k) | forall (b::k). T2 (S b)
-- Here k should scope over the whole definition
extractDataDefnKindVars (HsDataDefn { dd_ctxt = ctxt, dd_kindSig = ksig
                                    , dd_cons = cons, dd_derivs = derivs })
  = fst $ extract_lctxt ctxt $
          extract_mb extract_lkind ksig $
          extract_mb (extract_ltys . unLoc) derivs $
          foldr (extract_con . unLoc) ([],[]) cons
  where
    extract_con (ConDecl { con_res = ResTyGADT {} }) acc = acc
    extract_con (ConDecl { con_res = ResTyH98, con_qvars = qvs
                         , con_cxt = ctxt, con_details = details }) acc
      = extract_hs_tv_bndrs qvs acc $
        extract_lctxt ctxt $
        extract_ltys (hsConDeclArgTys details) ([],[])


extract_lctxt :: LHsContext RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_lctxt ctxt = extract_ltys (unLoc ctxt)

extract_ltys :: [LHsType RdrName] -> FreeKiTyVars -> FreeKiTyVars
extract_ltys tys acc = foldr extract_lty acc tys

extract_mb :: (a -> FreeKiTyVars -> FreeKiTyVars) -> Maybe a -> FreeKiTyVars -> FreeKiTyVars
extract_mb _ Nothing  acc = acc
extract_mb f (Just x) acc = f x acc

extract_lkind :: LHsType RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_lkind kind (acc_kvs, acc_tvs) = case extract_lty kind ([], acc_kvs) of
                                          (_, res_kvs) -> (res_kvs, acc_tvs)
                                        -- Kinds shouldn't have sort signatures!

extract_lty :: LHsType RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_lty (L _ ty) acc
  = case ty of
      HsTyVar tv                -> extract_tv tv acc
      HsBangTy _ ty             -> extract_lty ty acc
      HsRecTy flds              -> foldr (extract_lty . cd_fld_type . unLoc) acc
                                         flds
      HsAppTy ty1 ty2           -> extract_lty ty1 (extract_lty ty2 acc)
      HsListTy ty               -> extract_lty ty acc
      HsPArrTy ty               -> extract_lty ty acc
      HsTupleTy _ tys           -> extract_ltys tys acc
      HsFunTy ty1 ty2           -> extract_lty ty1 (extract_lty ty2 acc)
      HsIParamTy _ ty           -> extract_lty ty acc
      HsEqTy ty1 ty2            -> extract_lty ty1 (extract_lty ty2 acc)
      HsOpTy ty1 (_, (L _ tv)) ty2 -> extract_tv tv (extract_lty ty1 (extract_lty ty2 acc))
      HsParTy ty                -> extract_lty ty acc
      HsCoreTy {}               -> acc  -- The type is closed
      HsSpliceTy {}             -> acc  -- Type splices mention no type variables
      HsDocTy ty _              -> extract_lty ty acc
      HsExplicitListTy _ tys    -> extract_ltys tys acc
      HsExplicitTupleTy _ tys   -> extract_ltys tys acc
      HsTyLit _                 -> acc
      HsWrapTy _ _              -> panic "extract_lty"
      HsKindSig ty ki           -> extract_lty ty (extract_lkind ki acc)
      HsForAllTy _ _ tvs cx ty  -> extract_hs_tv_bndrs tvs acc $
                                   extract_lctxt cx   $
                                   extract_lty ty ([],[])
      -- We deal with these separately in rnLHsTypeWithWildCards
      HsWildCardTy _            -> acc

extract_hs_tv_bndrs :: LHsTyVarBndrs RdrName -> FreeKiTyVars
                    -> FreeKiTyVars -> FreeKiTyVars
extract_hs_tv_bndrs (HsQTvs { hsq_tvs = tvs })
                    (acc_kvs, acc_tvs)   -- Note accumulator comes first
                    (body_kvs, body_tvs)
  | null tvs
  = (body_kvs ++ acc_kvs, body_tvs ++ acc_tvs)
  | otherwise
  = (acc_kvs ++ filterOut (`elem` local_kvs) body_kvs,
     acc_tvs ++ filterOut (`elem` local_tvs) body_tvs)
  where
    local_tvs = map hsLTyVarName tvs
    (_, local_kvs) = foldr extract_lty ([], []) [k | L _ (KindedTyVar _ k) <- tvs]
       -- These kind variables are bound here if not bound further out

extract_tv :: RdrName -> FreeKiTyVars -> FreeKiTyVars
extract_tv tv acc
  | isRdrTyVar tv = case acc of (kvs,tvs) -> (kvs, tv : tvs)
  | otherwise     = acc
