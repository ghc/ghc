{-
%
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998
-}

{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies, DataKinds, GADTs, TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-} -- Wrinkle in Note [Trees That Grow]

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

module GHC.Tc.Gen.Head
       ( HsExprArg(..), EValArg(..), TcPass(..), Rebuilder
       , splitHsApps
       , addArgWrap, eValArgExpr, zonkArg, isHsValArg, setSrcSpanFromArgs
       , countLeadingValArgs, isVisibleArg, pprHsExprArgTc, rebuildPrefixApps

       , tcInferAppHead, tcInferAppHead_maybe
       , tcInferId, tcCheckId
       , tcCheckRecSelId
       , disambiguateSelector, obviousSig, addAmbiguousNameErr
       , tyConOf, tyConOfET, lookupParents, fieldNotInType
       , notSelector, nonBidirectionalErr

       , exprCtxt, addExprCtxt, addFunResCtxt ) where

import {-# SOURCE #-} GHC.Tc.Gen.Expr( tcExpr, tcExprWithSig )

import GHC.Hs
import GHC.Tc.TyCl.PatSyn( patSynBuilderOcc )
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Unify
import GHC.Types.Basic
import GHC.Tc.Utils.Instantiate
import GHC.Tc.Instance.Family ( tcGetFamInstEnvs, tcLookupDataFamInst )
import GHC.Core.FamInstEnv    ( FamInstEnvs )
import GHC.Core.UsageEnv      ( unitUE )
import GHC.Rename.Env         ( addUsedGRE )
import GHC.Rename.Utils       ( addNameClashErrRn, unknownSubordinateErr )
import GHC.Tc.Utils.Env
import GHC.Tc.Gen.HsType
import GHC.Tc.Gen.Pat
import GHC.Tc.Gen.Sig( isCompleteHsSig )
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcType as TcType
import GHC.Types.Id
import GHC.Types.Id.Info
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Core.TyCon
import GHC.Core.TyCo.Rep
import GHC.Core.Type
import GHC.Tc.Types.Evidence
import GHC.Builtin.Types( multiplicityTy )
import GHC.Builtin.Names
import GHC.Builtin.Names.TH( liftStringName, liftName )
import GHC.Driver.Session
import GHC.Types.SrcLoc
import GHC.Utils.Misc
import GHC.Data.Maybe
import GHC.Utils.Outputable as Outputable
import Control.Monad

import Data.Function

#include "HsVersions.h"

import GHC.Prelude


{- *********************************************************************
*                                                                      *
              HsExprArg: auxiliary data type
*                                                                      *
********************************************************************* -}

{- Note [HsExprArg]
~~~~~~~~~~~~~~~~~~~
The data typs HsExprArg :: TcPass -> Type
is a very local type, used only within this module.

* It's really a zipper for an application chain

* It's a GHC-specific type, so using TTG only where necessary

* It is indexed by TcPass, meaning
  - HsExprArg TcpRn:
      The result of splitHsApps, which decomposes a HsExpr GhcRn

  - HsExprArg TcpInst:
      The result of tcInstFun, which instantiates the function type
      Adds EWrap nodes, the argument type in EValArg,
      and the kind-checked type in ETypeArg

  - HsExprArg TcpTc:
      The result of tcArg, which typechecks the value args
      In EValArg we now have a (LHsExpr GhcTc)

* rebuildPrefixApps is dual to splitHsApps, and zips an application
  back into a HsExpr

Note [EValArg]
~~~~~~~~~~~~~~
The data type EValArg is the payload of the EValArg constructor of
HsExprArg; i.e. a value argument of the application.  EValArg has two
forms:

* ValArg: payload is just the expression itself. Simple.

* ValArgQL: captures the results of applying quickLookArg to the
  argument in a ValArg.  When we later want to typecheck that argument
  we can just carry on from where quick-look left off.  The fields of
  ValArgQL exactly capture what is needed to complete the job.

Invariants:

1. With QL switched off, all arguments are ValArg; no ValArgQL

2. With QL switched on, tcInstFun converts some ValArgs to ValArgQL,
   under the conditions when quick-look should happen (eg the argument
   type is guarded) -- see quickLookArg

Note [splitHsApps and Rebuilder]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The key function
  splitHsApps :: HsExpr GhcRn -> (HsExpr GhcRn, [HsExprArg 'TcpRn], Rebuilder)
takes apart either an HsApp, or an infix OpApp, returning

* The "head" of the application, an expression that is often a variable

* A list of HsExprArg, the arguments

* A Rebuilder function which reconstructs the original form,
  given the head and arguments
-}

data TcPass = TcpRn     -- Arguments decomposed
            | TcpInst   -- Function instantiated
            | TcpTc     -- Typechecked

data HsExprArg (p :: TcPass)
  = -- See Note [HsExprArg]
    EValArg  { eva_loc    :: SrcSpan        -- Of the function
             , eva_arg    :: EValArg p
             , eva_arg_ty :: !(XEVAType p) }

  | ETypeArg { eva_loc   :: SrcSpan          -- Of the function
             , eva_hs_ty :: LHsWcType GhcRn  -- The type arg
             , eva_ty    :: !(XETAType p) }  -- Kind-checked type arg

  | EPrag    SrcSpan
             (HsPragE (GhcPass (XPass p)))

  | EPar     SrcSpan         -- Of the nested expr

  | EWrap    !(XEWrap p)     -- Wrapper, after instantiation

data EValArg (p :: TcPass) where  -- See Note [EValArg]
  ValArg   :: LHsExpr (GhcPass (XPass p))
           -> EValArg p
  ValArgQL :: { va_expr :: LHsExpr GhcRn        -- Original expression
                                                -- For location and error msgs
              , va_fun  :: HsExpr GhcTc         -- Function, typechecked
              , va_args :: [HsExprArg 'TcpInst] -- Args, instantiated
              , va_ty   :: TcRhoType            -- Result type
              , va_rebuild :: Rebuilder }       -- How to reassemble
           -> EValArg 'TcpInst  -- Only exists in TcpInst phase

type Rebuilder = HsExpr GhcTc -> [HsExprArg 'TcpTc]-> HsExpr GhcTc
-- See Note [splitHsApps and Rebuilder]

type family XPass p where
  XPass 'TcpRn   = 'Renamed
  XPass 'TcpInst = 'Renamed
  XPass 'TcpTc   = 'Typechecked

type family XETAType p where  -- Type arguments
  XETAType 'TcpRn = NoExtField
  XETAType _      = Type

type family XEVAType p where  -- Value arguments
  XEVAType 'TcpRn = NoExtField
  XEVAType _      = Scaled Type

type family XEWrap p where
  XEWrap 'TcpRn = NoExtCon
  XEWrap _      = HsWrapper

mkEValArg :: SrcSpan -> LHsExpr GhcRn -> HsExprArg 'TcpRn
mkEValArg l e = EValArg { eva_loc = l, eva_arg = ValArg e
                        , eva_arg_ty = noExtField }

mkETypeArg :: SrcSpan -> LHsWcType GhcRn -> HsExprArg 'TcpRn
mkETypeArg l hs_ty = ETypeArg { eva_loc = l, eva_hs_ty = hs_ty
                              , eva_ty = noExtField }

eValArgExpr :: EValArg 'TcpInst -> LHsExpr GhcRn
eValArgExpr (ValArg e)                 = e
eValArgExpr (ValArgQL { va_expr = e }) = e

addArgWrap :: HsWrapper -> [HsExprArg 'TcpInst] -> [HsExprArg 'TcpInst]
addArgWrap wrap args
 | isIdHsWrapper wrap = args
 | otherwise          = EWrap wrap : args

zonkArg :: HsExprArg 'TcpInst -> TcM (HsExprArg 'TcpInst)
zonkArg eva@(EValArg { eva_arg_ty = Scaled m ty })
  = do { ty' <- zonkTcType ty
       ; return (eva { eva_arg_ty = Scaled m ty' }) }
zonkArg arg = return arg

splitHsApps :: HsExpr GhcRn -> (HsExpr GhcRn, [HsExprArg 'TcpRn], Rebuilder)
-- See Note [splitHsApps and Rebuilder]
splitHsApps e
  = go e []
  where
    go (HsPar _     (L l fun))       args = go fun (EPar       l       : args)
    go (HsPragE _ p (L l fun))       args = go fun (EPrag      l p     : args)
    go (HsAppType _ (L l fun) hs_ty) args = go fun (mkETypeArg l hs_ty : args)
    go (HsApp _     (L l fun) arg)   args = go fun (mkEValArg  l arg   : args)

    go (OpApp fix arg1 (L l op) arg2) args
      = ( op, mkEValArg l arg1 : mkEValArg l arg2 : args
        , rebuildInfixApps fix )

    go e args = (e, args, rebuildPrefixApps)

rebuildInfixApps :: Fixity -> Rebuilder
rebuildInfixApps fix fun args
  = go fun args
  where
    go fun (EValArg { eva_arg = ValArg arg1, eva_loc = l } :
            EValArg { eva_arg = ValArg arg2 } : args)
                                 = rebuildPrefixApps (OpApp fix arg1 (L l fun) arg2) args
    go fun (EWrap wrap : args) = go (mkHsWrap wrap fun) args
    go fun args                  = rebuildPrefixApps fun args

rebuildPrefixApps :: Rebuilder
rebuildPrefixApps fun args
  = go fun args
  where
    go fun [] = fun
    go fun (EWrap wrap : args)               = go (mkHsWrap wrap fun) args
    go fun (EValArg { eva_arg = ValArg arg
                    , eva_loc = l } : args)  = go (HsApp noExtField (L l fun) arg) args
    go fun (ETypeArg { eva_hs_ty = hs_ty
                     , eva_ty  = ty
                     , eva_loc = l } : args) = go (HsAppType ty (L l fun) hs_ty) args
    go fun (EPar l : args)                   = go (HsPar noExtField (L l fun)) args
    go fun (EPrag l p : args)                = go (HsPragE noExtField p (L l fun)) args

isHsValArg :: HsExprArg id -> Bool
isHsValArg (EValArg {}) = True
isHsValArg _              = False

countLeadingValArgs :: [HsExprArg id] -> Int
countLeadingValArgs (EValArg {} : args) = 1 + countLeadingValArgs args
countLeadingValArgs (EPar {} : args)    = countLeadingValArgs args
countLeadingValArgs _                   = 0

isValArg :: HsExprArg id -> Bool
isValArg (EValArg {}) = True
isValArg _            = False

isVisibleArg :: HsExprArg id -> Bool
isVisibleArg (EValArg {})  = True
isVisibleArg (ETypeArg {}) = True
isVisibleArg _             = False

isArgPar :: HsExprArg id -> Bool
isArgPar (EPar {}) = True
isArgPar _           = False

setSrcSpanFromArgs :: [HsExprArg 'TcpRn] -> TcM a -> TcM a
setSrcSpanFromArgs [] thing_inside
  = thing_inside     -- Don't set the location twice
setSrcSpanFromArgs (arg:_) thing_inside
  = setSrcSpan (argFunLoc arg) thing_inside

argFunLoc :: HsExprArg 'TcpRn -> SrcSpan
argFunLoc (EValArg { eva_loc = l }) = l
argFunLoc (ETypeArg { eva_loc = l}) = l
argFunLoc (EPrag l _)               = l
argFunLoc (EPar l)                  = l

instance OutputableBndrId (XPass p) => Outputable (HsExprArg p) where
  ppr (EValArg { eva_arg = arg })      = text "EValArg" <+> ppr arg
  ppr (EPrag _ p)                      = text "EPrag" <+> ppr p
  ppr (ETypeArg { eva_hs_ty = hs_ty }) = char '@' <> ppr hs_ty
  ppr (EPar _)                         = text "EPar"
  ppr (EWrap _)                        = text "EWrap"
  -- ToDo: to print the wrapper properly we'll need to work harder

instance OutputableBndrId (XPass p) => Outputable (EValArg p) where
  ppr (ValArg e) = ppr e
  ppr (ValArgQL { va_fun = fun, va_args = args, va_ty = ty})
    = hang (text "ValArgQL" <+> ppr fun)
         2 (vcat [ ppr args, text "va_ty:" <+> ppr ty ])

pprHsExprArgTc :: HsExprArg 'TcpInst -> SDoc
pprHsExprArgTc (EValArg { eva_arg = tm, eva_arg_ty = ty })
  = text "EValArg" <+> hang (ppr tm) 2 (dcolon <+> ppr ty)
pprHsExprArgTc arg = ppr arg


{- *********************************************************************
*                                                                      *
                 tcInferAppHead
*                                                                      *
********************************************************************* -}

tcInferAppHead :: HsExpr GhcRn
               -> [HsExprArg 'TcpRn]
               -> TcM (HsExpr GhcTc, TcSigmaType)
-- Infer type of the head of an application
--   i.e. the 'f' in (f e1 ... en)
-- We get back a /SigmaType/ because we have special cases for
--   * A bare identifier (just look it up)
--     This case also covers a record selectro HsRecFld
--   * An expression with a type signature (e :: ty)
--
-- Note that [] and (,,) are both HsVar:
--   see Note [Empty lists] and [ExplicitTuple] in GHC.Hs.Expr
--
-- NB: 'e' cannot be HsApp, HsTyApp, HsPrag, HsPar, because those
--     cases are dealt with by splitHsApps.
--
-- See Note [Typechecking applications]
tcInferAppHead fun args
  = setSrcSpanFromArgs args $
    do { mb_tc_fun <- tcInferAppHead_maybe fun args
       ; case mb_tc_fun of
            Just (fun', fun_sigma) -> return (fun', fun_sigma)
            Nothing -> addErrCtxt (exprCtxt fun) $
                       tcInfer (tcExpr fun) }

tcInferAppHead_maybe :: HsExpr GhcRn
                     -> [HsExprArg 'TcpRn]
                     -> TcM (Maybe (HsExpr GhcTc, TcSigmaType))
-- Returns Nothing for a complicated head
tcInferAppHead_maybe fun args
  = case fun of
      HsVar _ (L _ nm)          -> Just <$> tcInferId nm
      HsUnboundVar _ occ        -> Just <$> tcInferUnboundId occ
      HsRecFld _ f              -> Just <$> tcInferRecSelId f args
      ExprWithTySig _ e hs_ty
        | isCompleteHsSig hs_ty -> addErrCtxt (exprCtxt fun) $
                                   Just <$> tcExprWithSig e hs_ty
      _                         -> return Nothing


{- *********************************************************************
*                                                                      *
                 Record selectors
*                                                                      *
********************************************************************* -}

{-
Note [Disambiguating record fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the -XDuplicateRecordFields extension is used, and the renamer
encounters a record selector or update that it cannot immediately
disambiguate (because it involves fields that belong to multiple
datatypes), it will defer resolution of the ambiguity to the
typechecker.  In this case, the `Ambiguous` constructor of
`AmbiguousFieldOcc` is used.

Consider the following definitions:

        data S = MkS { foo :: Int }
        data T = MkT { foo :: Int, bar :: Int }
        data U = MkU { bar :: Int, baz :: Int }

When the renamer sees `foo` as a selector or an update, it will not
know which parent datatype is in use.

For selectors, there are two possible ways to disambiguate:

1. Check if the pushed-in type is a function whose domain is a
   datatype, for example:

       f s = (foo :: S -> Int) s

       g :: T -> Int
       g = foo

    This is checked by `tcCheckRecSelId` when checking `HsRecFld foo`.

2. Check if the selector is applied to an argument that has a type
   signature, for example:

       h = foo (s :: S)

    This is checked by `tcApp`.


Updates are slightly more complex.  The `disambiguateRecordBinds`
function tries to determine the parent datatype in three ways:

1. Check for types that have all the fields being updated. For example:

        f x = x { foo = 3, bar = 2 }

   Here `f` must be updating `T` because neither `S` nor `U` have
   both fields. This may also discover that no possible type exists.
   For example the following will be rejected:

        f' x = x { foo = 3, baz = 3 }

2. Use the type being pushed in, if it is already a TyConApp. The
   following are valid updates to `T`:

        g :: T -> T
        g x = x { foo = 3 }

        g' x = x { foo = 3 } :: T

3. Use the type signature of the record expression, if it exists and
   is a TyConApp. Thus this is valid update to `T`:

        h x = (x :: T) { foo = 3 }


Note that we do not look up the types of variables being updated, and
no constraint-solving is performed, so for example the following will
be rejected as ambiguous:

     let bad (s :: S) = foo s

     let r :: T
         r = blah
     in r { foo = 3 }

     \r. (r { foo = 3 },  r :: T )

We could add further tests, of a more heuristic nature. For example,
rather than looking for an explicit signature, we could try to infer
the type of the argument to a selector or the record expression being
updated, in case we are lucky enough to get a TyConApp straight
away. However, it might be hard for programmers to predict whether a
particular update is sufficiently obvious for the signature to be
omitted. Moreover, this might change the behaviour of typechecker in
non-obvious ways.

See also Note [HsRecField and HsRecUpdField] in GHC.Hs.Pat.
-}

tcCheckRecSelId :: HsExpr GhcRn -> AmbiguousFieldOcc GhcRn
                -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCheckRecSelId rn_expr (Unambiguous sel_name lbl) res_ty
  = do { sel_id <- tc_rec_sel_id lbl sel_name
       ; let expr = HsRecFld noExtField (Unambiguous sel_id lbl)
       ; tcWrapResult rn_expr expr (idType sel_id) res_ty }
tcCheckRecSelId rn_expr (Ambiguous _ lbl) res_ty
  = case tcSplitFunTy_maybe =<< checkingExpType_maybe res_ty of
      Nothing       -> ambiguousSelector lbl
      Just (arg, _) -> do { sel_name <- disambiguateSelector lbl (scaledThing arg)
                          ; sel_id <- tc_rec_sel_id lbl sel_name
                          ; let expr = HsRecFld noExtField (Ambiguous sel_id lbl)
                          ; tcWrapResult rn_expr expr (idType sel_id) res_ty }

tcInferRecSelId :: AmbiguousFieldOcc GhcRn -> [HsExprArg 'TcpRn]
                -> TcM (HsExpr GhcTc, TcSigmaType)
-- Disgusting special case for ambiguous record selectors
tcInferRecSelId (Ambiguous _ lbl) args
  | arg1 : _ <- filterOut isArgPar args -- A value arg is first
  , EValArg { eva_arg = ValArg (L _ arg) } <- arg1
  , Just sig_ty <- obviousSig arg  -- A type sig on the arg disambiguates
  = do { sig_tc_ty <- tcHsSigWcType ExprSigCtxt sig_ty
       ; sel_name  <- disambiguateSelector lbl sig_tc_ty
       ; sel_id <- tc_rec_sel_id lbl sel_name
       ; let expr = HsRecFld noExtField (Ambiguous sel_id lbl)
       ; return (expr, idType sel_id) }
  | otherwise
  = ambiguousSelector lbl

tcInferRecSelId (Unambiguous sel_name lbl) _args
   = do { sel_id <- tc_rec_sel_id lbl sel_name
        ; let expr = HsRecFld noExtField (Unambiguous sel_id lbl)
        ; return (expr, idType sel_id) }

------------------------
tc_rec_sel_id :: Located RdrName -> Name -> TcM TcId
tc_rec_sel_id lbl sel_name
  = do { thing <- tcLookup sel_name
       ; case thing of
             ATcId { tct_id = id }
               -> do { check_naughty occ id    -- Note [Local record selectors]
                     ; checkThLocalId id
                     ; tcEmitBindingUsage $ unitUE sel_name One
                     ; return id }

             AGlobal (AnId id)
               -> do { check_naughty occ id
                     ; return id }
                    -- A global cannot possibly be ill-staged
                    -- nor does it need the 'lifting' treatment
                    -- hence no checkTh stuff here

             _ -> failWithTc $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    occ = rdrNameOcc (unLoc lbl)

check_naughty :: OccName -> TcId -> TcM ()
check_naughty lbl id
  | isNaughtyRecordSelector id = failWithTc (naughtyRecordSel lbl)
  | otherwise                  = return ()

------------------------
-- Given a RdrName that refers to multiple record fields, and the type
-- of its argument, try to determine the name of the selector that is
-- meant.
disambiguateSelector :: Located RdrName -> Type -> TcM Name
disambiguateSelector lr@(L _ rdr) parent_type
 = do { fam_inst_envs <- tcGetFamInstEnvs
      ; case tyConOf fam_inst_envs parent_type of
          Nothing -> ambiguousSelector lr
          Just p  ->
            do { xs <- lookupParents rdr
               ; let parent = RecSelData p
               ; case lookup parent xs of
                   Just gre -> do { addUsedGRE True gre
                                  ; return (gre_name gre) }
                   Nothing  -> failWithTc (fieldNotInType parent rdr) } }

-- This field name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then give up.
ambiguousSelector :: Located RdrName -> TcM a
ambiguousSelector (L _ rdr)
  = do { addAmbiguousNameErr rdr
       ; failM }

-- | This name really is ambiguous, so add a suitable "ambiguous
-- occurrence" error, then continue
addAmbiguousNameErr :: RdrName -> TcM ()
addAmbiguousNameErr rdr
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; setErrCtxt [] $ addNameClashErrRn rdr gres}

-- A type signature on the argument of an ambiguous record selector or
-- the record expression in an update must be "obvious", i.e. the
-- outermost constructor ignoring parentheses.
obviousSig :: HsExpr GhcRn -> Maybe (LHsSigWcType GhcRn)
obviousSig (ExprWithTySig _ _ ty) = Just ty
obviousSig (HsPar _ p)          = obviousSig (unLoc p)
obviousSig _                    = Nothing

-- Extract the outermost TyCon of a type, if there is one; for
-- data families this is the representation tycon (because that's
-- where the fields live).
tyConOf :: FamInstEnvs -> TcSigmaType -> Maybe TyCon
tyConOf fam_inst_envs ty0
  = case tcSplitTyConApp_maybe ty of
      Just (tc, tys) -> Just (fstOf3 (tcLookupDataFamInst fam_inst_envs tc tys))
      Nothing        -> Nothing
  where
    (_, _, ty) = tcSplitSigmaTy ty0

-- Variant of tyConOf that works for ExpTypes
tyConOfET :: FamInstEnvs -> ExpRhoType -> Maybe TyCon
tyConOfET fam_inst_envs ty0 = tyConOf fam_inst_envs =<< checkingExpType_maybe ty0


-- For an ambiguous record field, find all the candidate record
-- selectors (as GlobalRdrElts) and their parents.
lookupParents :: RdrName -> RnM [(RecSelParent, GlobalRdrElt)]
lookupParents rdr
  = do { env <- getGlobalRdrEnv
       ; let gres = lookupGRE_RdrName rdr env
       ; mapM lookupParent gres }
  where
    lookupParent :: GlobalRdrElt -> RnM (RecSelParent, GlobalRdrElt)
    lookupParent gre = do { id <- tcLookupId (gre_name gre)
                          ; if isRecordSelector id
                              then return (recordSelectorTyCon id, gre)
                              else failWithTc (notSelector (gre_name gre)) }


fieldNotInType :: RecSelParent -> RdrName -> SDoc
fieldNotInType p rdr
  = unknownSubordinateErr (text "field of type" <+> quotes (ppr p)) rdr

notSelector :: Name -> SDoc
notSelector field
  = hsep [quotes (ppr field), text "is not a record selector"]

naughtyRecordSel :: OccName -> SDoc
naughtyRecordSel lbl
  = text "Cannot use record selector" <+> quotes (ppr lbl) <+>
    text "as a function due to escaped type variables" $$
    text "Probable fix: use pattern-matching syntax instead"


{- *********************************************************************
*                                                                      *
                Unbound (wildcard) Ids
*                                                                      *
********************************************************************* -}

tcInferUnboundId :: OccName -> TcM (HsExpr GhcTc, TcRhoType)
-- Typecheck an occurrence of an unbound Id
--
-- Some of these started life as a true expression hole "_".
-- Others might simply be variables that accidentally have no binding site
tcInferUnboundId occ
 = do { ty <- newOpenFlexiTyVarTy  -- Allow Int# etc (#12531)
      ; name <- newSysName occ
      ; let ev = mkLocalId name Many ty
      ; emitNewExprHole occ ev ty
      ; return (HsUnboundVar ev occ, ty) }


{- *********************************************************************
*                                                                      *
                 tcInferId, tcCheckId
*                                                                      *
********************************************************************* -}

tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTc)
tcCheckId name res_ty
  = do { (expr, actual_res_ty) <- tcInferId name
       ; traceTc "tcCheckId" (vcat [ppr name, ppr actual_res_ty, ppr res_ty])
       ; addFunResCtxt expr [] actual_res_ty res_ty $
         tcWrapResultO (OccurrenceOf name) (HsVar noExtField (noLoc name)) expr
                                           actual_res_ty res_ty }

------------------------
tcInferId :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Look up an occurrence of an Id
-- Do not instantiate its type
tcInferId id_name
  | id_name `hasKey` assertIdKey
  = do { dflags <- getDynFlags
       ; if gopt Opt_IgnoreAsserts dflags
         then tc_infer_id id_name
         else tc_infer_assert id_name }

  | otherwise
  = do { (expr, ty) <- tc_infer_id id_name
       ; traceTc "tcInferId" (ppr id_name <+> dcolon <+> ppr ty)
       ; return (expr, ty) }

tc_infer_assert :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
-- Deal with an occurrence of 'assert'
-- See Note [Adding the implicit parameter to 'assert']
tc_infer_assert assert_name
  = do { assert_error_id <- tcLookupId assertErrorName
       ; (wrap, id_rho) <- topInstantiate (OccurrenceOf assert_name)
                                          (idType assert_error_id)
       ; return (mkHsWrap wrap (HsVar noExtField (noLoc assert_error_id)), id_rho)
       }

tc_infer_id :: Name -> TcM (HsExpr GhcTc, TcSigmaType)
tc_infer_id id_name
 = do { thing <- tcLookup id_name
      ; case thing of
             ATcId { tct_id = id }
               -> do { check_naughty occ id
                           -- See Note [HsVar: naughty record selectors]
                     ; checkThLocalId id
                     ; tcEmitBindingUsage $ unitUE id_name One
                     ; return_id id }

             AGlobal (AnId id)
               -> do { check_naughty occ id
                           -- See Note [HsVar: naughty record selectors]
                     ; return_id id }
                    -- A global cannot possibly be ill-staged
                    -- nor does it need the 'lifting' treatment
                    -- hence no checkTh stuff here

             AGlobal (AConLike cl) -> case cl of
                 RealDataCon con -> return_data_con con
                 PatSynCon ps
                   | Just (expr, ty) <- patSynBuilderOcc ps
                   -> return (expr, ty)
                   | otherwise
                   -> nonBidirectionalErr id_name

             _ -> failWithTc $
                  ppr thing <+> text "used where a value identifier was expected" }
  where
    occ = nameOccName id_name

    return_id id = return (HsVar noExtField (noLoc id), idType id)

    return_data_con con
      = do { let tvs = dataConUserTyVarBinders con
                 theta = dataConOtherTheta con
                 args = dataConOrigArgTys con
                 res = dataConOrigResTy con

           -- See Note [Linear fields generalization]
           ; mul_vars <- newFlexiTyVarTys (length args) multiplicityTy
           ; let scaleArgs args' = zipWithEqual "return_data_con" combine mul_vars args'
                 combine var (Scaled One ty) = Scaled var ty
                 combine _   scaled_ty       = scaled_ty
                   -- The combine function implements the fact that, as
                   -- described in Note [Linear fields generalization], if a
                   -- field is not linear (last line) it isn't made polymorphic.

                 etaWrapper arg_tys = foldr (\scaled_ty wr -> WpFun WpHole wr scaled_ty empty) WpHole arg_tys

           -- See Note [Instantiating stupid theta]
           ; let shouldInstantiate = (not (null (dataConStupidTheta con)) ||
                                      isKindLevPoly (tyConResKind (dataConTyCon con)))
           ; case shouldInstantiate of
               True -> do { (subst, tvs') <- newMetaTyVars (binderVars tvs)
                           ; let tys'   = mkTyVarTys tvs'
                                 theta' = substTheta subst theta
                                 args'  = substScaledTys subst args
                                 res'   = substTy subst res
                           ; wrap <- instCall (OccurrenceOf id_name) tys' theta'
                           ; let scaled_arg_tys = scaleArgs args'
                                 eta_wrap = etaWrapper scaled_arg_tys
                           ; addDataConStupidTheta con tys'
                           ; return ( mkHsWrap (eta_wrap <.> wrap)
                                               (HsConLikeOut noExtField (RealDataCon con))
                                    , mkVisFunTys scaled_arg_tys res')
                           }
               False -> let scaled_arg_tys = scaleArgs args
                            wrap1 = mkWpTyApps (mkTyVarTys $ binderVars tvs)
                            eta_wrap = etaWrapper (map unrestricted theta ++ scaled_arg_tys)
                            wrap2 = mkWpTyLams $ binderVars tvs
                        in return ( mkHsWrap (wrap2 <.> eta_wrap <.> wrap1)
                                             (HsConLikeOut noExtField (RealDataCon con))
                                  , mkInvisForAllTys tvs $ mkInvisFunTysMany theta $ mkVisFunTys scaled_arg_tys res)
           }


nonBidirectionalErr :: Outputable name => name -> TcM a
nonBidirectionalErr name = failWithTc $
    text "non-bidirectional pattern synonym"
    <+> quotes (ppr name) <+> text "used in an expression"

{- Note [HsVar: naughty record selectors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
All record selectors should really be HsRecFld (ambiguous or
unambiguous), but currently not all of them are: see #18452.  So we
need to check for naughty record selectors in tc_infer_id, as well as
in tc_rec_sel_id.

Remove this code when fixing #18452.

Note [Linear fields generalization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
As per Note [Polymorphisation of linear fields], linear field of data
constructors get a polymorphic type when the data constructor is used as a term.

    Just :: forall {p} a. a #p-> Maybe a

This rule is known only to the typechecker: Just keeps its linear type in Core.

In order to desugar this generalised typing rule, we simply eta-expand:

    \a (x # p :: a) -> Just @a x

has the appropriate type. We insert these eta-expansion with WpFun wrappers.

A small hitch: if the constructor is levity-polymorphic (unboxed tuples, sums,
certain newtypes with -XUnliftedNewtypes) then this strategy produces

    \r1 r2 a b (x # p :: a) (y # q :: b) -> (# a, b #)

Which has type

    forall r1 r2 a b. a #p-> b #q-> (# a, b #)

Which violates the levity-polymorphism restriction see Note [Levity polymorphism
checking] in DsMonad.

So we really must instantiate r1 and r2 rather than quantify over them.  For
simplicity, we just instantiate the entire type, as described in Note
[Instantiating stupid theta]. It breaks visible type application with unboxed
tuples, sums and levity-polymorphic newtypes, but this doesn't appear to be used
anywhere.

A better plan: let's force all representation variable to be *inferred*, so that
they are not subject to visible type applications. Then we can instantiate
inferred argument eagerly.

Note [Adding the implicit parameter to 'assert']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The typechecker transforms (assert e1 e2) to (assertError e1 e2).
This isn't really the Right Thing because there's no way to "undo"
if you want to see the original source code in the typechecker
output.  We'll have fix this in due course, when we care more about
being able to reconstruct the exact original program.


Note [Instantiating stupid theta]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Normally, when we infer the type of an Id, we don't instantiate,
because we wish to allow for visible type application later on.
But if a datacon has a stupid theta, we're a bit stuck. We need
to emit the stupid theta constraints with instantiated types. It's
difficult to defer this to the lazy instantiation, because a stupid
theta has no spot to put it in a type. So we just instantiate eagerly
in this case. Thus, users cannot use visible type application with
a data constructor sporting a stupid theta. I won't feel so bad for
the users that complain.
-}

{-
************************************************************************
*                                                                      *
                 Template Haskell checks
*                                                                      *
************************************************************************
-}

checkThLocalId :: Id -> TcM ()
-- The renamer has already done checkWellStaged,
--   in RnSplice.checkThLocalName, so don't repeat that here.
-- Here we just add constraints for cross-stage lifting
checkThLocalId id
  = do  { mb_local_use <- getStageAndBindLevel (idName id)
        ; case mb_local_use of
             Just (top_lvl, bind_lvl, use_stage)
                | thLevel use_stage > bind_lvl
                -> checkCrossStageLifting top_lvl id use_stage
             _  -> return ()   -- Not a locally-bound thing, or
                               -- no cross-stage link
    }

--------------------------------------
checkCrossStageLifting :: TopLevelFlag -> Id -> ThStage -> TcM ()
-- If we are inside typed brackets, and (use_lvl > bind_lvl)
-- we must check whether there's a cross-stage lift to do
-- Examples   \x -> [|| x ||]
--            [|| map ||]
--
-- This is similar to checkCrossStageLifting in GHC.Rename.Splice, but
-- this code is applied to *typed* brackets.

checkCrossStageLifting top_lvl id (Brack _ (TcPending ps_var lie_var q))
  | isTopLevel top_lvl
  = when (isExternalName id_name) (keepAlive id_name)
    -- See Note [Keeping things alive for Template Haskell] in GHC.Rename.Splice

  | otherwise
  =     -- Nested identifiers, such as 'x' in
        -- E.g. \x -> [|| h x ||]
        -- We must behave as if the reference to x was
        --      h $(lift x)
        -- We use 'x' itself as the splice proxy, used by
        -- the desugarer to stitch it all back together.
        -- If 'x' occurs many times we may get many identical
        -- bindings of the same splice proxy, but that doesn't
        -- matter, although it's a mite untidy.
    do  { let id_ty = idType id
        ; checkTc (isTauTy id_ty) (polySpliceErr id)
               -- If x is polymorphic, its occurrence sites might
               -- have different instantiations, so we can't use plain
               -- 'x' as the splice proxy name.  I don't know how to
               -- solve this, and it's probably unimportant, so I'm
               -- just going to flag an error for now

        ; lift <- if isStringTy id_ty then
                     do { sid <- tcLookupId GHC.Builtin.Names.TH.liftStringName
                                     -- See Note [Lifting strings]
                        ; return (HsVar noExtField (noLoc sid)) }
                  else
                     setConstraintVar lie_var   $
                          -- Put the 'lift' constraint into the right LIE
                     newMethodFromName (OccurrenceOf id_name)
                                       GHC.Builtin.Names.TH.liftName
                                       [getRuntimeRep id_ty, id_ty]

                   -- Update the pending splices
        ; ps <- readMutVar ps_var
        ; let pending_splice = PendingTcSplice id_name
                                 (nlHsApp (mkLHsWrap (applyQuoteWrapper q) (noLoc lift))
                                          (nlHsVar id))
        ; writeMutVar ps_var (pending_splice : ps)

        ; return () }
  where
    id_name = idName id

checkCrossStageLifting _ _ _ = return ()

polySpliceErr :: Id -> SDoc
polySpliceErr id
  = text "Can't splice the polymorphic local variable" <+> quotes (ppr id)

{-
Note [Lifting strings]
~~~~~~~~~~~~~~~~~~~~~~
If we see $(... [| s |] ...) where s::String, we don't want to
generate a mass of Cons (CharL 'x') (Cons (CharL 'y') ...)) etc.
So this conditional short-circuits the lifting mechanism to generate
(liftString "xy") in that case.  I didn't want to use overlapping instances
for the Lift class in TH.Syntax, because that can lead to overlapping-instance
errors in a polymorphic situation.

If this check fails (which isn't impossible) we get another chance; see
Note [Converting strings] in Convert.hs

Local record selectors
~~~~~~~~~~~~~~~~~~~~~~
Record selectors for TyCons in this module are ordinary local bindings,
which show up as ATcIds rather than AGlobals.  So we need to check for
naughtiness in both branches.  c.f. TcTyClsBindings.mkAuxBinds.
-}


{- *********************************************************************
*                                                                      *
         Error reporting for function result mis-matches
*                                                                      *
********************************************************************* -}

addFunResCtxt :: HsExpr GhcTc -> [HsExprArg 'TcpTc]
              -> TcType -> ExpRhoType
              -> TcM a -> TcM a
-- When we have a mis-match in the return type of a function
-- try to give a helpful message about too many/few arguments
addFunResCtxt fun args fun_res_ty env_ty
  = addLandmarkErrCtxtM (\env -> (env, ) <$> mk_msg)
      -- NB: use a landmark error context, so that an empty context
      -- doesn't suppress some more useful context
  where
    mk_msg
      = do { mb_env_ty <- readExpType_maybe env_ty
                     -- by the time the message is rendered, the ExpType
                     -- will be filled in (except if we're debugging)
           ; fun_res' <- zonkTcType fun_res_ty
           ; env'     <- case mb_env_ty of
                           Just env_ty -> zonkTcType env_ty
                           Nothing     ->
                             do { dumping <- doptM Opt_D_dump_tc_trace
                                ; MASSERT( dumping )
                                ; newFlexiTyVarTy liftedTypeKind }
           ; let -- See Note [Splitting nested sigma types in mismatched
                 --           function types]
                 (_, _, fun_tau) = tcSplitNestedSigmaTys fun_res'
                 -- No need to call tcSplitNestedSigmaTys here, since env_ty is
                 -- an ExpRhoTy, i.e., it's already instantiated.
                 (_, _, env_tau) = tcSplitSigmaTy env'
                 (args_fun, res_fun) = tcSplitFunTys fun_tau
                 (args_env, res_env) = tcSplitFunTys env_tau
                 n_fun = length args_fun
                 n_env = length args_env
                 info  | -- Check for too few args
                         --  fun_tau = a -> b, res_tau = Int
                         n_fun > n_env
                       , not_fun res_env
                       = text "Probable cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too few arguments"

                       | -- Check for too many args
                         -- fun_tau = a -> Int,   res_tau = a -> b -> c -> d
                         -- The final guard suppresses the message when there
                         -- aren't enough args to drop; eg. the call is (f e1)
                         n_fun < n_env
                       , not_fun res_fun
                       , (n_fun + count isValArg args) >= n_env
                          -- Never suggest that a naked variable is
                                           -- applied to too many args!
                       = text "Possible cause:" <+> quotes (ppr fun)
                         <+> text "is applied to too many arguments"

                       | otherwise
                       = Outputable.empty

           ; return info }
      where
        not_fun ty   -- ty is definitely not an arrow type,
                     -- and cannot conceivably become one
          = case tcSplitTyConApp_maybe ty of
              Just (tc, _) -> isAlgTyCon tc
              Nothing      -> False

{-
Note [Splitting nested sigma types in mismatched function types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When one applies a function to too few arguments, GHC tries to determine this
fact if possible so that it may give a helpful error message. It accomplishes
this by checking if the type of the applied function has more argument types
than supplied arguments.

Previously, GHC computed the number of argument types through tcSplitSigmaTy.
This is incorrect in the face of nested foralls, however! This caused Trac
#13311, for instance:

  f :: forall a. (Monoid a) => forall b. (Monoid b) => Maybe a -> Maybe b

If one uses `f` like so:

  do { f; putChar 'a' }

Then tcSplitSigmaTy will decompose the type of `f` into:

  Tyvars: [a]
  Context: (Monoid a)
  Argument types: []
  Return type: forall b. Monoid b => Maybe a -> Maybe b

That is, it will conclude that there are *no* argument types, and since `f`
was given no arguments, it won't print a helpful error message. On the other
hand, tcSplitNestedSigmaTys correctly decomposes `f`'s type down to:

  Tyvars: [a, b]
  Context: (Monoid a, Monoid b)
  Argument types: [Maybe a]
  Return type: Maybe b

So now GHC recognizes that `f` has one more argument type than it was actually
provided.
-}


{- *********************************************************************
*                                                                      *
             Misc utility functions
*                                                                      *
********************************************************************* -}

addExprCtxt :: LHsExpr GhcRn -> TcRn a -> TcRn a
addExprCtxt (L _ e) thing_inside
   | HsUnboundVar {} <- e = thing_inside
   | otherwise            = addErrCtxt (exprCtxt e) thing_inside
   -- The HsUnboundVar special case addresses situations like
   --    f x = _
   -- when we don't want to say "In the expression: _",
   -- because it is mentioned in the error message itself

exprCtxt :: HsExpr GhcRn -> SDoc
exprCtxt expr = hang (text "In the expression:") 2 (ppr (stripParensHsExpr expr))

