{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


This module converts Template Haskell syntax into Hs syntax
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}

module GHC.ThToHs
   ( convertToHsExpr
   , convertToPat
   , convertToHsDecls
   , convertToHsType
   , thRdrNameGuesses
   )
where

import GhcPrelude

import GHC.Hs as Hs
import PrelNames
import RdrName
import qualified Name
import Module
import RdrHsSyn
import OccName
import SrcLoc
import Type
import qualified Coercion ( Role(..) )
import TysWiredIn
import BasicTypes as Hs
import ForeignCall
import Unique
import ErrUtils
import Bag
import Lexeme
import Util
import FastString
import Outputable
import MonadUtils ( foldrM )

import qualified Data.ByteString as BS
import Control.Monad( unless, ap )

import Data.Maybe( catMaybes, isNothing )
import Language.Haskell.TH as TH hiding (sigP)
import Language.Haskell.TH.Syntax as TH
import Foreign.ForeignPtr
import Foreign.Ptr
import System.IO.Unsafe

-------------------------------------------------------------------
--              The external interface

convertToHsDecls :: Origin -> SrcSpan -> [TH.Dec] -> Either MsgDoc [LHsDecl GhcPs]
convertToHsDecls origin loc ds = initCvt origin loc (fmap catMaybes (mapM cvt_dec ds))
  where
    cvt_dec d = wrapMsg "declaration" d (cvtDec d)

convertToHsExpr :: Origin -> SrcSpan -> TH.Exp -> Either MsgDoc (LHsExpr GhcPs)
convertToHsExpr origin loc e
  = initCvt origin loc $ wrapMsg "expression" e $ cvtl e

convertToPat :: Origin -> SrcSpan -> TH.Pat -> Either MsgDoc (LPat GhcPs)
convertToPat origin loc p
  = initCvt origin loc $ wrapMsg "pattern" p $ cvtPat p

convertToHsType :: Origin -> SrcSpan -> TH.Type -> Either MsgDoc (LHsType GhcPs)
convertToHsType origin loc t
  = initCvt origin loc $ wrapMsg "type" t $ cvtType t

-------------------------------------------------------------------
newtype CvtM a = CvtM { unCvtM :: Origin -> SrcSpan -> Either MsgDoc (SrcSpan, a) }
    deriving (Functor)
        -- Push down the Origin (that is configurable by
        -- -fenable-th-splice-warnings) and source location;
        -- Can fail, with a single error message

-- NB: If the conversion succeeds with (Right x), there should
--     be no exception values hiding in x
-- Reason: so a (head []) in TH code doesn't subsequently
--         make GHC crash when it tries to walk the generated tree

-- Use the loc everywhere, for lack of anything better
-- In particular, we want it on binding locations, so that variables bound in
-- the spliced-in declarations get a location that at least relates to the splice point

instance Applicative CvtM where
    pure x = CvtM $ \_ loc -> Right (loc,x)
    (<*>) = ap

instance Monad CvtM where
  (CvtM m) >>= k = CvtM $ \origin loc -> case m origin loc of
    Left err -> Left err
    Right (loc',v) -> unCvtM (k v) origin loc'

initCvt :: Origin -> SrcSpan -> CvtM a -> Either MsgDoc a
initCvt origin loc (CvtM m) = fmap snd (m origin loc)

force :: a -> CvtM ()
force a = a `seq` return ()

failWith :: MsgDoc -> CvtM a
failWith m = CvtM (\_ _ -> Left m)

getOrigin :: CvtM Origin
getOrigin = CvtM (\origin loc -> Right (loc,origin))

getL :: CvtM SrcSpan
getL = CvtM (\_ loc -> Right (loc,loc))

setL :: SrcSpan -> CvtM ()
setL loc = CvtM (\_ _ -> Right (loc, ()))

returnL :: a -> CvtM (Located a)
returnL x = CvtM (\_ loc -> Right (loc, L loc x))

returnJustL :: a -> CvtM (Maybe (Located a))
returnJustL = fmap Just . returnL

wrapParL :: (Located a -> a) -> a -> CvtM a
wrapParL add_par x = CvtM (\_ loc -> Right (loc, add_par (L loc x)))

wrapMsg :: (Show a, TH.Ppr a) => String -> a -> CvtM b -> CvtM b
-- E.g  wrapMsg "declaration" dec thing
wrapMsg what item (CvtM m)
  = CvtM $ \origin loc -> case m origin loc of
      Left err -> Left (err $$ getPprStyle msg)
      Right v  -> Right v
  where
        -- Show the item in pretty syntax normally,
        -- but with all its constructors if you say -dppr-debug
    msg sty = hang (text "When splicing a TH" <+> text what <> colon)
                 2 (if debugStyle sty
                    then text (show item)
                    else text (pprint item))

wrapL :: CvtM a -> CvtM (Located a)
wrapL (CvtM m) = CvtM $ \origin loc -> case m origin loc of
  Left err -> Left err
  Right (loc', v) -> Right (loc', L loc v)

-------------------------------------------------------------------
cvtDecs :: [TH.Dec] -> CvtM [LHsDecl GhcPs]
cvtDecs = fmap catMaybes . mapM cvtDec

cvtDec :: TH.Dec -> CvtM (Maybe (LHsDecl GhcPs))
cvtDec (TH.ValD pat body ds)
  | TH.VarP s <- pat
  = do  { s' <- vNameL s
        ; cl' <- cvtClause (mkPrefixFunRhs s') (Clause [] body ds)
        ; th_origin <- getOrigin
        ; returnJustL $ Hs.ValD noExtField $ mkFunBind th_origin s' [cl'] }

  | otherwise
  = do  { pat' <- cvtPat pat
        ; body' <- cvtGuard body
        ; ds' <- cvtLocalDecs (text "a where clause") ds
        ; returnJustL $ Hs.ValD noExtField $
          PatBind { pat_lhs = pat'
                  , pat_rhs = GRHSs noExtField body' (noLoc ds')
                  , pat_ext = noExtField
                  , pat_ticks = ([],[]) } }

cvtDec (TH.FunD nm cls)
  | null cls
  = failWith (text "Function binding for"
                 <+> quotes (text (TH.pprint nm))
                 <+> text "has no equations")
  | otherwise
  = do  { nm' <- vNameL nm
        ; cls' <- mapM (cvtClause (mkPrefixFunRhs nm')) cls
        ; th_origin <- getOrigin
        ; returnJustL $ Hs.ValD noExtField $ mkFunBind th_origin nm' cls' }

cvtDec (TH.SigD nm typ)
  = do  { nm' <- vNameL nm
        ; ty' <- cvtType typ
        ; returnJustL $ Hs.SigD noExtField
                                    (TypeSig noExtField [nm'] (mkLHsSigWcType ty')) }

cvtDec (TH.KiSigD nm ki)
  = do  { nm' <- tconNameL nm
        ; ki' <- cvtType ki
        ; let sig' = StandaloneKindSig noExtField nm' (mkLHsSigType ki')
        ; returnJustL $ Hs.KindSigD noExtField sig' }

cvtDec (TH.InfixD fx nm)
  -- Fixity signatures are allowed for variables, constructors, and types
  -- the renamer automatically looks for types during renaming, even when
  -- the RdrName says it's a variable or a constructor. So, just assume
  -- it's a variable or constructor and proceed.
  = do { nm' <- vcNameL nm
       ; returnJustL (Hs.SigD noExtField (FixSig noExtField
                                      (FixitySig noExtField [nm'] (cvtFixity fx)))) }

cvtDec (PragmaD prag)
  = cvtPragmaD prag

cvtDec (TySynD tc tvs rhs)
  = do  { (_, tc', tvs') <- cvt_tycl_hdr [] tc tvs
        ; rhs' <- cvtType rhs
        ; returnJustL $ TyClD noExtField $
          SynDecl { tcdSExt = noExtField, tcdLName = tc', tcdTyVars = tvs'
                  , tcdFixity = Prefix
                  , tcdRhs = rhs' } }

cvtDec (DataD ctxt tc tvs ksig constrs derivs)
  = do  { let isGadtCon (GadtC    _ _ _) = True
              isGadtCon (RecGadtC _ _ _) = True
              isGadtCon (ForallC  _ _ c) = isGadtCon c
              isGadtCon _                = False
              isGadtDecl  = all isGadtCon constrs
              isH98Decl   = all (not . isGadtCon) constrs
        ; unless (isGadtDecl || isH98Decl)
                 (failWith (text "Cannot mix GADT constructors with Haskell 98"
                        <+> text "constructors"))
        ; unless (isNothing ksig || isGadtDecl)
                 (failWith (text "Kind signatures are only allowed on GADTs"))
        ; (ctxt', tc', tvs') <- cvt_tycl_hdr ctxt tc tvs
        ; ksig' <- cvtKind `traverse` ksig
        ; cons' <- mapM cvtConstr constrs
        ; derivs' <- cvtDerivs derivs
        ; let defn = HsDataDefn { dd_ext = noExtField
                                , dd_ND = DataType, dd_cType = Nothing
                                , dd_ctxt = ctxt'
                                , dd_kindSig = ksig'
                                , dd_cons = cons', dd_derivs = derivs' }
        ; returnJustL $ TyClD noExtField $
          DataDecl { tcdDExt = noExtField
                   , tcdLName = tc', tcdTyVars = tvs'
                   , tcdFixity = Prefix
                   , tcdDataDefn = defn } }

cvtDec (NewtypeD ctxt tc tvs ksig constr derivs)
  = do  { (ctxt', tc', tvs') <- cvt_tycl_hdr ctxt tc tvs
        ; ksig' <- cvtKind `traverse` ksig
        ; con' <- cvtConstr constr
        ; derivs' <- cvtDerivs derivs
        ; let defn = HsDataDefn { dd_ext = noExtField
                                , dd_ND = NewType, dd_cType = Nothing
                                , dd_ctxt = ctxt'
                                , dd_kindSig = ksig'
                                , dd_cons = [con']
                                , dd_derivs = derivs' }
        ; returnJustL $ TyClD noExtField $
          DataDecl { tcdDExt = noExtField
                   , tcdLName = tc', tcdTyVars = tvs'
                   , tcdFixity = Prefix
                   , tcdDataDefn = defn } }

cvtDec (ClassD ctxt cl tvs fds decs)
  = do  { (cxt', tc', tvs') <- cvt_tycl_hdr ctxt cl tvs
        ; fds'  <- mapM cvt_fundep fds
        ; (binds', sigs', fams', at_defs', adts') <- cvt_ci_decs (text "a class declaration") decs
        ; unless (null adts')
            (failWith $ (text "Default data instance declarations"
                     <+> text "are not allowed:")
                   $$ (Outputable.ppr adts'))
        ; returnJustL $ TyClD noExtField $
          ClassDecl { tcdCExt = noExtField
                    , tcdCtxt = cxt', tcdLName = tc', tcdTyVars = tvs'
                    , tcdFixity = Prefix
                    , tcdFDs = fds', tcdSigs = Hs.mkClassOpSigs sigs'
                    , tcdMeths = binds'
                    , tcdATs = fams', tcdATDefs = at_defs', tcdDocs = [] }
                              -- no docs in TH ^^
        }

cvtDec (InstanceD o ctxt ty decs)
  = do  { let doc = text "an instance declaration"
        ; (binds', sigs', fams', ats', adts') <- cvt_ci_decs doc decs
        ; unless (null fams') (failWith (mkBadDecMsg doc fams'))
        ; ctxt' <- cvtContext funPrec ctxt
        ; (L loc ty') <- cvtType ty
        ; let inst_ty' = mkHsQualTy ctxt loc ctxt' $ L loc ty'
        ; returnJustL $ InstD noExtField $ ClsInstD noExtField $
          ClsInstDecl { cid_ext = noExtField, cid_poly_ty = mkLHsSigType inst_ty'
                      , cid_binds = binds'
                      , cid_sigs = Hs.mkClassOpSigs sigs'
                      , cid_tyfam_insts = ats', cid_datafam_insts = adts'
                      , cid_overlap_mode = fmap (L loc . overlap) o } }
  where
  overlap pragma =
    case pragma of
      TH.Overlaps      -> Hs.Overlaps     (SourceText "OVERLAPS")
      TH.Overlappable  -> Hs.Overlappable (SourceText "OVERLAPPABLE")
      TH.Overlapping   -> Hs.Overlapping  (SourceText "OVERLAPPING")
      TH.Incoherent    -> Hs.Incoherent   (SourceText "INCOHERENT")




cvtDec (ForeignD ford)
  = do { ford' <- cvtForD ford
       ; returnJustL $ ForD noExtField ford' }

cvtDec (DataFamilyD tc tvs kind)
  = do { (_, tc', tvs') <- cvt_tycl_hdr [] tc tvs
       ; result <- cvtMaybeKindToFamilyResultSig kind
       ; returnJustL $ TyClD noExtField $ FamDecl noExtField $
         FamilyDecl noExtField DataFamily tc' tvs' Prefix result Nothing }

cvtDec (DataInstD ctxt bndrs tys ksig constrs derivs)
  = do { (ctxt', tc', bndrs', typats') <- cvt_datainst_hdr ctxt bndrs tys
       ; ksig' <- cvtKind `traverse` ksig
       ; cons' <- mapM cvtConstr constrs
       ; derivs' <- cvtDerivs derivs
       ; let defn = HsDataDefn { dd_ext = noExtField
                               , dd_ND = DataType, dd_cType = Nothing
                               , dd_ctxt = ctxt'
                               , dd_kindSig = ksig'
                               , dd_cons = cons', dd_derivs = derivs' }

       ; returnJustL $ InstD noExtField $ DataFamInstD
           { dfid_ext = noExtField
           , dfid_inst = DataFamInstDecl { dfid_eqn = mkHsImplicitBndrs $
                           FamEqn { feqn_ext = noExtField
                                  , feqn_tycon = tc'
                                  , feqn_bndrs = bndrs'
                                  , feqn_pats = typats'
                                  , feqn_rhs = defn
                                  , feqn_fixity = Prefix } }}}

cvtDec (NewtypeInstD ctxt bndrs tys ksig constr derivs)
  = do { (ctxt', tc', bndrs', typats') <- cvt_datainst_hdr ctxt bndrs tys
       ; ksig' <- cvtKind `traverse` ksig
       ; con' <- cvtConstr constr
       ; derivs' <- cvtDerivs derivs
       ; let defn = HsDataDefn { dd_ext = noExtField
                               , dd_ND = NewType, dd_cType = Nothing
                               , dd_ctxt = ctxt'
                               , dd_kindSig = ksig'
                               , dd_cons = [con'], dd_derivs = derivs' }
       ; returnJustL $ InstD noExtField $ DataFamInstD
           { dfid_ext = noExtField
           , dfid_inst = DataFamInstDecl { dfid_eqn = mkHsImplicitBndrs $
                           FamEqn { feqn_ext = noExtField
                                  , feqn_tycon = tc'
                                  , feqn_bndrs = bndrs'
                                  , feqn_pats = typats'
                                  , feqn_rhs = defn
                                  , feqn_fixity = Prefix } }}}

cvtDec (TySynInstD eqn)
  = do  { (L _ eqn') <- cvtTySynEqn eqn
        ; returnJustL $ InstD noExtField $ TyFamInstD
            { tfid_ext = noExtField
            , tfid_inst = TyFamInstDecl { tfid_eqn = eqn' } } }

cvtDec (OpenTypeFamilyD head)
  = do { (tc', tyvars', result', injectivity') <- cvt_tyfam_head head
       ; returnJustL $ TyClD noExtField $ FamDecl noExtField $
         FamilyDecl noExtField OpenTypeFamily tc' tyvars' Prefix result' injectivity'
       }

cvtDec (ClosedTypeFamilyD head eqns)
  = do { (tc', tyvars', result', injectivity') <- cvt_tyfam_head head
       ; eqns' <- mapM cvtTySynEqn eqns
       ; returnJustL $ TyClD noExtField $ FamDecl noExtField $
         FamilyDecl noExtField (ClosedTypeFamily (Just eqns')) tc' tyvars' Prefix
                           result' injectivity' }

cvtDec (TH.RoleAnnotD tc roles)
  = do { tc' <- tconNameL tc
       ; let roles' = map (noLoc . cvtRole) roles
       ; returnJustL $ Hs.RoleAnnotD noExtField (RoleAnnotDecl noExtField tc' roles') }

cvtDec (TH.StandaloneDerivD ds cxt ty)
  = do { cxt' <- cvtContext funPrec cxt
       ; ds'  <- traverse cvtDerivStrategy ds
       ; (L loc ty') <- cvtType ty
       ; let inst_ty' = mkHsQualTy cxt loc cxt' $ L loc ty'
       ; returnJustL $ DerivD noExtField $
         DerivDecl { deriv_ext =noExtField
                   , deriv_strategy = ds'
                   , deriv_type = mkLHsSigWcType inst_ty'
                   , deriv_overlap_mode = Nothing } }

cvtDec (TH.DefaultSigD nm typ)
  = do { nm' <- vNameL nm
       ; ty' <- cvtType typ
       ; returnJustL $ Hs.SigD noExtField
                     $ ClassOpSig noExtField True [nm'] (mkLHsSigType ty')}

cvtDec (TH.PatSynD nm args dir pat)
  = do { nm'   <- cNameL nm
       ; args' <- cvtArgs args
       ; dir'  <- cvtDir nm' dir
       ; pat'  <- cvtPat pat
       ; returnJustL $ Hs.ValD noExtField $ PatSynBind noExtField $
           PSB noExtField nm' args' pat' dir' }
  where
    cvtArgs (TH.PrefixPatSyn args) = Hs.PrefixCon <$> mapM vNameL args
    cvtArgs (TH.InfixPatSyn a1 a2) = Hs.InfixCon <$> vNameL a1 <*> vNameL a2
    cvtArgs (TH.RecordPatSyn sels)
      = do { sels' <- mapM vNameL sels
           ; vars' <- mapM (vNameL . mkNameS . nameBase) sels
           ; return $ Hs.RecCon $ zipWith RecordPatSynField sels' vars' }

    cvtDir _ Unidir          = return Unidirectional
    cvtDir _ ImplBidir       = return ImplicitBidirectional
    cvtDir n (ExplBidir cls) =
      do { ms <- mapM (cvtClause (mkPrefixFunRhs n)) cls
         ; th_origin <- getOrigin
         ; return $ ExplicitBidirectional $ mkMatchGroup th_origin ms }

cvtDec (TH.PatSynSigD nm ty)
  = do { nm' <- cNameL nm
       ; ty' <- cvtPatSynSigTy ty
       ; returnJustL $ Hs.SigD noExtField $ PatSynSig noExtField [nm'] (mkLHsSigType ty')}

-- Implicit parameter bindings are handled in cvtLocalDecs and
-- cvtImplicitParamBind. They are not allowed in any other scope, so
-- reaching this case indicates an error.
cvtDec (TH.ImplicitParamBindD _ _)
  = failWith (text "Implicit parameter binding only allowed in let or where")

----------------
cvtTySynEqn :: TySynEqn -> CvtM (LTyFamInstEqn GhcPs)
cvtTySynEqn (TySynEqn mb_bndrs lhs rhs)
  = do { mb_bndrs' <- traverse (mapM cvt_tv) mb_bndrs
       ; (head_ty, args) <- split_ty_app lhs
       ; case head_ty of
           ConT nm -> do { nm' <- tconNameL nm
                         ; rhs' <- cvtType rhs
                         ; let args' = map wrap_tyarg args
                         ; returnL $ mkHsImplicitBndrs
                            $ FamEqn { feqn_ext    = noExtField
                                     , feqn_tycon  = nm'
                                     , feqn_bndrs  = mb_bndrs'
                                     , feqn_pats   = args'
                                     , feqn_fixity = Prefix
                                     , feqn_rhs    = rhs' } }
           InfixT t1 nm t2 -> do { nm' <- tconNameL nm
                                 ; args' <- mapM cvtType [t1,t2]
                                 ; rhs' <- cvtType rhs
                                 ; returnL $ mkHsImplicitBndrs
                                      $ FamEqn { feqn_ext    = noExtField
                                               , feqn_tycon  = nm'
                                               , feqn_bndrs  = mb_bndrs'
                                               , feqn_pats   =
                                                (map HsValArg args') ++ args
                                               , feqn_fixity = Hs.Infix
                                               , feqn_rhs    = rhs' } }
           _ -> failWith $ text "Invalid type family instance LHS:"
                          <+> text (show lhs)
        }

----------------
cvt_ci_decs :: MsgDoc -> [TH.Dec]
            -> CvtM (LHsBinds GhcPs,
                     [LSig GhcPs],
                     [LFamilyDecl GhcPs],
                     [LTyFamInstDecl GhcPs],
                     [LDataFamInstDecl GhcPs])
-- Convert the declarations inside a class or instance decl
-- ie signatures, bindings, and associated types
cvt_ci_decs doc decs
  = do  { decs' <- cvtDecs decs
        ; let (ats', bind_sig_decs') = partitionWith is_tyfam_inst decs'
        ; let (adts', no_ats')       = partitionWith is_datafam_inst bind_sig_decs'
        ; let (sigs', prob_binds')   = partitionWith is_sig no_ats'
        ; let (binds', prob_fams')   = partitionWith is_bind prob_binds'
        ; let (fams', bads)          = partitionWith is_fam_decl prob_fams'
        ; unless (null bads) (failWith (mkBadDecMsg doc bads))
        ; return (listToBag binds', sigs', fams', ats', adts') }

----------------
cvt_tycl_hdr :: TH.Cxt -> TH.Name -> [TH.TyVarBndr]
             -> CvtM ( LHsContext GhcPs
                     , Located RdrName
                     , LHsQTyVars GhcPs)
cvt_tycl_hdr cxt tc tvs
  = do { cxt' <- cvtContext funPrec cxt
       ; tc'  <- tconNameL tc
       ; tvs' <- cvtTvs tvs
       ; return (cxt', tc', tvs')
       }

cvt_datainst_hdr :: TH.Cxt -> Maybe [TH.TyVarBndr] -> TH.Type
               -> CvtM ( LHsContext GhcPs
                       , Located RdrName
                       , Maybe [LHsTyVarBndr GhcPs]
                       , HsTyPats GhcPs)
cvt_datainst_hdr cxt bndrs tys
  = do { cxt' <- cvtContext funPrec cxt
       ; bndrs' <- traverse (mapM cvt_tv) bndrs
       ; (head_ty, args) <- split_ty_app tys
       ; case head_ty of
          ConT nm -> do { nm' <- tconNameL nm
                        ; let args' = map wrap_tyarg args
                        ; return (cxt', nm', bndrs', args') }
          InfixT t1 nm t2 -> do { nm' <- tconNameL nm
                                ; args' <- mapM cvtType [t1,t2]
                                ; return (cxt', nm', bndrs',
                                         ((map HsValArg args') ++ args)) }
          _ -> failWith $ text "Invalid type instance header:"
                          <+> text (show tys) }

----------------
cvt_tyfam_head :: TypeFamilyHead
               -> CvtM ( Located RdrName
                       , LHsQTyVars GhcPs
                       , Hs.LFamilyResultSig GhcPs
                       , Maybe (Hs.LInjectivityAnn GhcPs))

cvt_tyfam_head (TypeFamilyHead tc tyvars result injectivity)
  = do {(_, tc', tyvars') <- cvt_tycl_hdr [] tc tyvars
       ; result' <- cvtFamilyResultSig result
       ; injectivity' <- traverse cvtInjectivityAnnotation injectivity
       ; return (tc', tyvars', result', injectivity') }

-------------------------------------------------------------------
--              Partitioning declarations
-------------------------------------------------------------------

is_fam_decl :: LHsDecl GhcPs -> Either (LFamilyDecl GhcPs) (LHsDecl GhcPs)
is_fam_decl (L loc (TyClD _ (FamDecl { tcdFam = d }))) = Left (L loc d)
is_fam_decl decl = Right decl

is_tyfam_inst :: LHsDecl GhcPs -> Either (LTyFamInstDecl GhcPs) (LHsDecl GhcPs)
is_tyfam_inst (L loc (Hs.InstD _ (TyFamInstD { tfid_inst = d })))
  = Left (L loc d)
is_tyfam_inst decl
  = Right decl

is_datafam_inst :: LHsDecl GhcPs
                -> Either (LDataFamInstDecl GhcPs) (LHsDecl GhcPs)
is_datafam_inst (L loc (Hs.InstD  _ (DataFamInstD { dfid_inst = d })))
  = Left (L loc d)
is_datafam_inst decl
  = Right decl

is_sig :: LHsDecl GhcPs -> Either (LSig GhcPs) (LHsDecl GhcPs)
is_sig (L loc (Hs.SigD _ sig)) = Left (L loc sig)
is_sig decl                    = Right decl

is_bind :: LHsDecl GhcPs -> Either (LHsBind GhcPs) (LHsDecl GhcPs)
is_bind (L loc (Hs.ValD _ bind)) = Left (L loc bind)
is_bind decl                     = Right decl

is_ip_bind :: TH.Dec -> Either (String, TH.Exp) TH.Dec
is_ip_bind (TH.ImplicitParamBindD n e) = Left (n, e)
is_ip_bind decl             = Right decl

mkBadDecMsg :: Outputable a => MsgDoc -> [a] -> MsgDoc
mkBadDecMsg doc bads
  = sep [ text "Illegal declaration(s) in" <+> doc <> colon
        , nest 2 (vcat (map Outputable.ppr bads)) ]

---------------------------------------------------
--      Data types
---------------------------------------------------

cvtConstr :: TH.Con -> CvtM (LConDecl GhcPs)

cvtConstr (NormalC c strtys)
  = do  { c'   <- cNameL c
        ; tys' <- mapM cvt_arg strtys
        ; returnL $ mkConDeclH98 c' Nothing Nothing (PrefixCon tys') }

cvtConstr (RecC c varstrtys)
  = do  { c'    <- cNameL c
        ; args' <- mapM cvt_id_arg varstrtys
        ; returnL $ mkConDeclH98 c' Nothing Nothing
                                   (RecCon (noLoc args')) }

cvtConstr (InfixC st1 c st2)
  = do  { c'   <- cNameL c
        ; st1' <- cvt_arg st1
        ; st2' <- cvt_arg st2
        ; returnL $ mkConDeclH98 c' Nothing Nothing (InfixCon st1' st2') }

cvtConstr (ForallC tvs ctxt con)
  = do  { tvs'      <- cvtTvs tvs
        ; ctxt'     <- cvtContext funPrec ctxt
        ; L _ con'  <- cvtConstr con
        ; returnL $ add_forall tvs' ctxt' con' }
  where
    add_cxt lcxt         Nothing           = Just lcxt
    add_cxt (L loc cxt1) (Just (L _ cxt2))
      = Just (L loc (cxt1 ++ cxt2))

    add_forall tvs' cxt' con@(ConDeclGADT { con_qvars = qvars, con_mb_cxt = cxt })
      = con { con_forall = noLoc $ not (null all_tvs)
            , con_qvars  = mkHsQTvs all_tvs
            , con_mb_cxt = add_cxt cxt' cxt }
      where
        all_tvs = hsQTvExplicit tvs' ++ hsQTvExplicit qvars

    add_forall tvs' cxt' con@(ConDeclH98 { con_ex_tvs = ex_tvs, con_mb_cxt = cxt })
      = con { con_forall = noLoc $ not (null all_tvs)
            , con_ex_tvs = all_tvs
            , con_mb_cxt = add_cxt cxt' cxt }
      where
        all_tvs = hsQTvExplicit tvs' ++ ex_tvs

    add_forall _ _ (XConDecl nec) = noExtCon nec

cvtConstr (GadtC [] _strtys _ty)
  = failWith (text "GadtC must have at least one constructor name")

cvtConstr (GadtC c strtys ty)
  = do  { c'      <- mapM cNameL c
        ; args    <- mapM cvt_arg strtys
        ; L _ ty' <- cvtType ty
        ; c_ty    <- mk_arr_apps args ty'
        ; returnL $ fst $ mkGadtDecl c' c_ty}

cvtConstr (RecGadtC [] _varstrtys _ty)
  = failWith (text "RecGadtC must have at least one constructor name")

cvtConstr (RecGadtC c varstrtys ty)
  = do  { c'       <- mapM cNameL c
        ; ty'      <- cvtType ty
        ; rec_flds <- mapM cvt_id_arg varstrtys
        ; let rec_ty = noLoc (HsFunTy noExtField
                                           (noLoc $ HsRecTy noExtField rec_flds) ty')
        ; returnL $ fst $ mkGadtDecl c' rec_ty }

cvtSrcUnpackedness :: TH.SourceUnpackedness -> SrcUnpackedness
cvtSrcUnpackedness NoSourceUnpackedness = NoSrcUnpack
cvtSrcUnpackedness SourceNoUnpack       = SrcNoUnpack
cvtSrcUnpackedness SourceUnpack         = SrcUnpack

cvtSrcStrictness :: TH.SourceStrictness -> SrcStrictness
cvtSrcStrictness NoSourceStrictness = NoSrcStrict
cvtSrcStrictness SourceLazy         = SrcLazy
cvtSrcStrictness SourceStrict       = SrcStrict

cvt_arg :: (TH.Bang, TH.Type) -> CvtM (LHsType GhcPs)
cvt_arg (Bang su ss, ty)
  = do { ty'' <- cvtType ty
       ; let ty' = parenthesizeHsType appPrec ty''
             su' = cvtSrcUnpackedness su
             ss' = cvtSrcStrictness ss
       ; returnL $ HsBangTy noExtField (HsSrcBang NoSourceText su' ss') ty' }

cvt_id_arg :: (TH.Name, TH.Bang, TH.Type) -> CvtM (LConDeclField GhcPs)
cvt_id_arg (i, str, ty)
  = do  { L li i' <- vNameL i
        ; ty' <- cvt_arg (str,ty)
        ; return $ noLoc (ConDeclField
                          { cd_fld_ext = noExtField
                          , cd_fld_names
                              = [L li $ FieldOcc noExtField (L li i')]
                          , cd_fld_type =  ty'
                          , cd_fld_doc = Nothing}) }

cvtDerivs :: [TH.DerivClause] -> CvtM (HsDeriving GhcPs)
cvtDerivs cs = do { cs' <- mapM cvtDerivClause cs
                  ; returnL cs' }

cvt_fundep :: FunDep -> CvtM (LHsFunDep GhcPs)
cvt_fundep (FunDep xs ys) = do { xs' <- mapM tNameL xs
                               ; ys' <- mapM tNameL ys
                               ; returnL (xs', ys') }


------------------------------------------
--      Foreign declarations
------------------------------------------

cvtForD :: Foreign -> CvtM (ForeignDecl GhcPs)
cvtForD (ImportF callconv safety from nm ty)
  -- the prim and javascript calling conventions do not support headers
  -- and are inserted verbatim, analogous to mkImport in RdrHsSyn
  | callconv == TH.Prim || callconv == TH.JavaScript
  = mk_imp (CImport (noLoc (cvt_conv callconv)) (noLoc safety') Nothing
                    (CFunction (StaticTarget (SourceText from)
                                             (mkFastString from) Nothing
                                             True))
                    (noLoc $ quotedSourceText from))
  | Just impspec <- parseCImport (noLoc (cvt_conv callconv)) (noLoc safety')
                                 (mkFastString (TH.nameBase nm))
                                 from (noLoc $ quotedSourceText from)
  = mk_imp impspec
  | otherwise
  = failWith $ text (show from) <+> text "is not a valid ccall impent"
  where
    mk_imp impspec
      = do { nm' <- vNameL nm
           ; ty' <- cvtType ty
           ; return (ForeignImport { fd_i_ext = noExtField
                                   , fd_name = nm'
                                   , fd_sig_ty = mkLHsSigType ty'
                                   , fd_fi = impspec })
           }
    safety' = case safety of
                     Unsafe     -> PlayRisky
                     Safe       -> PlaySafe
                     Interruptible -> PlayInterruptible

cvtForD (ExportF callconv as nm ty)
  = do  { nm' <- vNameL nm
        ; ty' <- cvtType ty
        ; let e = CExport (noLoc (CExportStatic (SourceText as)
                                                (mkFastString as)
                                                (cvt_conv callconv)))
                                                (noLoc (SourceText as))
        ; return $ ForeignExport { fd_e_ext = noExtField
                                 , fd_name = nm'
                                 , fd_sig_ty = mkLHsSigType ty'
                                 , fd_fe = e } }

cvt_conv :: TH.Callconv -> CCallConv
cvt_conv TH.CCall      = CCallConv
cvt_conv TH.StdCall    = StdCallConv
cvt_conv TH.CApi       = CApiConv
cvt_conv TH.Prim       = PrimCallConv
cvt_conv TH.JavaScript = JavaScriptCallConv

------------------------------------------
--              Pragmas
------------------------------------------

cvtPragmaD :: Pragma -> CvtM (Maybe (LHsDecl GhcPs))
cvtPragmaD (InlineP nm inline rm phases)
  = do { nm' <- vNameL nm
       ; let dflt = dfltActivation inline
       ; let src TH.NoInline  = "{-# NOINLINE"
             src TH.Inline    = "{-# INLINE"
             src TH.Inlinable = "{-# INLINABLE"
       ; let ip   = InlinePragma { inl_src    = SourceText $ src inline
                                 , inl_inline = cvtInline inline
                                 , inl_rule   = cvtRuleMatch rm
                                 , inl_act    = cvtPhases phases dflt
                                 , inl_sat    = Nothing }
       ; returnJustL $ Hs.SigD noExtField $ InlineSig noExtField nm' ip }

cvtPragmaD (SpecialiseP nm ty inline phases)
  = do { nm' <- vNameL nm
       ; ty' <- cvtType ty
       ; let src TH.NoInline  = "{-# SPECIALISE NOINLINE"
             src TH.Inline    = "{-# SPECIALISE INLINE"
             src TH.Inlinable = "{-# SPECIALISE INLINE"
       ; let (inline', dflt,srcText) = case inline of
               Just inline1 -> (cvtInline inline1, dfltActivation inline1,
                                src inline1)
               Nothing      -> (NoUserInline,   AlwaysActive,
                                "{-# SPECIALISE")
       ; let ip = InlinePragma { inl_src    = SourceText srcText
                               , inl_inline = inline'
                               , inl_rule   = Hs.FunLike
                               , inl_act    = cvtPhases phases dflt
                               , inl_sat    = Nothing }
       ; returnJustL $ Hs.SigD noExtField $ SpecSig noExtField nm' [mkLHsSigType ty'] ip }

cvtPragmaD (SpecialiseInstP ty)
  = do { ty' <- cvtType ty
       ; returnJustL $ Hs.SigD noExtField $
         SpecInstSig noExtField (SourceText "{-# SPECIALISE") (mkLHsSigType ty') }

cvtPragmaD (RuleP nm ty_bndrs tm_bndrs lhs rhs phases)
  = do { let nm' = mkFastString nm
       ; let act = cvtPhases phases AlwaysActive
       ; ty_bndrs' <- traverse (mapM cvt_tv) ty_bndrs
       ; tm_bndrs' <- mapM cvtRuleBndr tm_bndrs
       ; lhs'   <- cvtl lhs
       ; rhs'   <- cvtl rhs
       ; returnJustL $ Hs.RuleD noExtField
            $ HsRules { rds_ext = noExtField
                      , rds_src = SourceText "{-# RULES"
                      , rds_rules = [noLoc $
                          HsRule { rd_ext  = noExtField
                                 , rd_name = (noLoc (quotedSourceText nm,nm'))
                                 , rd_act  = act
                                 , rd_tyvs = ty_bndrs'
                                 , rd_tmvs = tm_bndrs'
                                 , rd_lhs  = lhs'
                                 , rd_rhs  = rhs' }] }

          }

cvtPragmaD (AnnP target exp)
  = do { exp' <- cvtl exp
       ; target' <- case target of
         ModuleAnnotation  -> return ModuleAnnProvenance
         TypeAnnotation n  -> do
           n' <- tconName n
           return (TypeAnnProvenance  (noLoc n'))
         ValueAnnotation n -> do
           n' <- vcName n
           return (ValueAnnProvenance (noLoc n'))
       ; returnJustL $ Hs.AnnD noExtField
                     $ HsAnnotation noExtField (SourceText "{-# ANN") target' exp'
       }

cvtPragmaD (LineP line file)
  = do { setL (srcLocSpan (mkSrcLoc (fsLit file) line 1))
       ; return Nothing
       }
cvtPragmaD (CompleteP cls mty)
  = do { cls' <- noLoc <$> mapM cNameL cls
       ; mty'  <- traverse tconNameL mty
       ; returnJustL $ Hs.SigD noExtField
                   $ CompleteMatchSig noExtField NoSourceText cls' mty' }

dfltActivation :: TH.Inline -> Activation
dfltActivation TH.NoInline = NeverActive
dfltActivation _           = AlwaysActive

cvtInline :: TH.Inline -> Hs.InlineSpec
cvtInline TH.NoInline  = Hs.NoInline
cvtInline TH.Inline    = Hs.Inline
cvtInline TH.Inlinable = Hs.Inlinable

cvtRuleMatch :: TH.RuleMatch -> RuleMatchInfo
cvtRuleMatch TH.ConLike = Hs.ConLike
cvtRuleMatch TH.FunLike = Hs.FunLike

cvtPhases :: TH.Phases -> Activation -> Activation
cvtPhases AllPhases       dflt = dflt
cvtPhases (FromPhase i)   _    = ActiveAfter NoSourceText i
cvtPhases (BeforePhase i) _    = ActiveBefore NoSourceText i

cvtRuleBndr :: TH.RuleBndr -> CvtM (Hs.LRuleBndr GhcPs)
cvtRuleBndr (RuleVar n)
  = do { n' <- vNameL n
       ; return $ noLoc $ Hs.RuleBndr noExtField n' }
cvtRuleBndr (TypedRuleVar n ty)
  = do { n'  <- vNameL n
       ; ty' <- cvtType ty
       ; return $ noLoc $ Hs.RuleBndrSig noExtField n' $ mkLHsSigWcType ty' }

---------------------------------------------------
--              Declarations
---------------------------------------------------

cvtLocalDecs :: MsgDoc -> [TH.Dec] -> CvtM (HsLocalBinds GhcPs)
cvtLocalDecs doc ds
  = case partitionWith is_ip_bind ds of
      ([], []) -> return (EmptyLocalBinds noExtField)
      ([], _) -> do
        ds' <- cvtDecs ds
        let (binds, prob_sigs) = partitionWith is_bind ds'
        let (sigs, bads) = partitionWith is_sig prob_sigs
        unless (null bads) (failWith (mkBadDecMsg doc bads))
        return (HsValBinds noExtField (ValBinds noExtField (listToBag binds) sigs))
      (ip_binds, []) -> do
        binds <- mapM (uncurry cvtImplicitParamBind) ip_binds
        return (HsIPBinds noExtField (IPBinds noExtField binds))
      ((_:_), (_:_)) ->
        failWith (text "Implicit parameters mixed with other bindings")

cvtClause :: HsMatchContext GhcPs
          -> TH.Clause -> CvtM (Hs.LMatch GhcPs (LHsExpr GhcPs))
cvtClause ctxt (Clause ps body wheres)
  = do  { ps' <- cvtPats ps
        ; let pps = map (parenthesizePat appPrec) ps'
        ; g'  <- cvtGuard body
        ; ds' <- cvtLocalDecs (text "a where clause") wheres
        ; returnL $ Hs.Match noExtField ctxt pps (GRHSs noExtField g' (noLoc ds')) }

cvtImplicitParamBind :: String -> TH.Exp -> CvtM (LIPBind GhcPs)
cvtImplicitParamBind n e = do
    n' <- wrapL (ipName n)
    e' <- cvtl e
    returnL (IPBind noExtField (Left n') e')

-------------------------------------------------------------------
--              Expressions
-------------------------------------------------------------------

cvtl :: TH.Exp -> CvtM (LHsExpr GhcPs)
cvtl e = wrapL (cvt e)
  where
    cvt (VarE s)        = do { s' <- vName s; return $ HsVar noExtField (noLoc s') }
    cvt (ConE s)        = do { s' <- cName s; return $ HsVar noExtField (noLoc s') }
    cvt (LitE l)
      | overloadedLit l = go cvtOverLit (HsOverLit noExtField)
                             (hsOverLitNeedsParens appPrec)
      | otherwise       = go cvtLit (HsLit noExtField)
                             (hsLitNeedsParens appPrec)
      where
        go :: (Lit -> CvtM (l GhcPs))
           -> (l GhcPs -> HsExpr GhcPs)
           -> (l GhcPs -> Bool)
           -> CvtM (HsExpr GhcPs)
        go cvt_lit mk_expr is_compound_lit = do
          l' <- cvt_lit l
          let e' = mk_expr l'
          return $ if is_compound_lit l' then HsPar noExtField (noLoc e') else e'
    cvt (AppE x@(LamE _ _) y) = do { x' <- cvtl x; y' <- cvtl y
                                   ; return $ HsApp noExtField (mkLHsPar x')
                                                          (mkLHsPar y')}
    cvt (AppE x y)            = do { x' <- cvtl x; y' <- cvtl y
                                   ; return $ HsApp noExtField (mkLHsPar x')
                                                          (mkLHsPar y')}
    cvt (AppTypeE e t) = do { e' <- cvtl e
                            ; t' <- cvtType t
                            ; let tp = parenthesizeHsType appPrec t'
                            ; return $ HsAppType noExtField e'
                                     $ mkHsWildCardBndrs tp }
    cvt (LamE [] e)    = cvt e -- Degenerate case. We convert the body as its
                               -- own expression to avoid pretty-printing
                               -- oddities that can result from zero-argument
                               -- lambda expressions. See #13856.
    cvt (LamE ps e)    = do { ps' <- cvtPats ps; e' <- cvtl e
                            ; let pats = map (parenthesizePat appPrec) ps'
                            ; th_origin <- getOrigin
                            ; return $ HsLam noExtField (mkMatchGroup th_origin
                                             [mkSimpleMatch LambdaExpr
                                             pats e'])}
    cvt (LamCaseE ms)  = do { ms' <- mapM (cvtMatch CaseAlt) ms
                            ; th_origin <- getOrigin
                            ; return $ HsLamCase noExtField
                                                   (mkMatchGroup th_origin ms')
                            }
    cvt (TupE es)        = cvt_tup es Boxed
    cvt (UnboxedTupE es) = cvt_tup es Unboxed
    cvt (UnboxedSumE e alt arity) = do { e' <- cvtl e
                                       ; unboxedSumChecks alt arity
                                       ; return $ ExplicitSum noExtField
                                                                   alt arity e'}
    cvt (CondE x y z)  = do { x' <- cvtl x; y' <- cvtl y; z' <- cvtl z;
                            ; return $ mkHsIf x' y' z' }
    cvt (MultiIfE alts)
      | null alts      = failWith (text "Multi-way if-expression with no alternatives")
      | otherwise      = do { alts' <- mapM cvtpair alts
                            ; return $ HsMultiIf noExtField alts' }
    cvt (LetE ds e)    = do { ds' <- cvtLocalDecs (text "a let expression") ds
                            ; e' <- cvtl e; return $ HsLet noExtField (noLoc ds') e'}
    cvt (CaseE e ms)   = do { e' <- cvtl e; ms' <- mapM (cvtMatch CaseAlt) ms
                            ; th_origin <- getOrigin
                            ; return $ HsCase noExtField e'
                                                 (mkMatchGroup th_origin ms') }
    cvt (DoE ss)       = cvtHsDo DoExpr ss
    cvt (MDoE ss)      = cvtHsDo MDoExpr ss
    cvt (CompE ss)     = cvtHsDo ListComp ss
    cvt (ArithSeqE dd) = do { dd' <- cvtDD dd
                            ; return $ ArithSeq noExtField Nothing dd' }
    cvt (ListE xs)
      | Just s <- allCharLs xs       = do { l' <- cvtLit (StringL s)
                                          ; return (HsLit noExtField l') }
             -- Note [Converting strings]
      | otherwise       = do { xs' <- mapM cvtl xs
                             ; return $ ExplicitList noExtField Nothing xs'
                             }

    -- Infix expressions
    cvt (InfixE (Just x) s (Just y)) = ensureValidOpExp s $
      do { x' <- cvtl x
         ; s' <- cvtl s
         ; y' <- cvtl y
         ; let px = parenthesizeHsExpr opPrec x'
               py = parenthesizeHsExpr opPrec y'
         ; wrapParL (HsPar noExtField)
           $ OpApp noExtField px s' py }
           -- Parenthesise both arguments and result,
           -- to ensure this operator application does
           -- does not get re-associated
           -- See Note [Operator association]
    cvt (InfixE Nothing  s (Just y)) = ensureValidOpExp s $
                                       do { s' <- cvtl s; y' <- cvtl y
                                          ; wrapParL (HsPar noExtField) $
                                                          SectionR noExtField s' y' }
                                            -- See Note [Sections in HsSyn] in GHC.Hs.Expr
    cvt (InfixE (Just x) s Nothing ) = ensureValidOpExp s $
                                       do { x' <- cvtl x; s' <- cvtl s
                                          ; wrapParL (HsPar noExtField) $
                                                          SectionL noExtField x' s' }

    cvt (InfixE Nothing  s Nothing ) = ensureValidOpExp s $
                                       do { s' <- cvtl s
                                          ; return $ HsPar noExtField s' }
                                       -- Can I indicate this is an infix thing?
                                       -- Note [Dropping constructors]

    cvt (UInfixE x s y)  = ensureValidOpExp s $
                           do { x' <- cvtl x
                              ; let x'' = case unLoc x' of
                                            OpApp {} -> x'
                                            _ -> mkLHsPar x'
                              ; cvtOpApp x'' s y } --  Note [Converting UInfix]

    cvt (ParensE e)      = do { e' <- cvtl e; return $ HsPar noExtField e' }
    cvt (SigE e t)       = do { e' <- cvtl e; t' <- cvtType t
                              ; let pe = parenthesizeHsExpr sigPrec e'
                              ; return $ ExprWithTySig noExtField pe (mkLHsSigWcType t') }
    cvt (RecConE c flds) = do { c' <- cNameL c
                              ; flds' <- mapM (cvtFld (mkFieldOcc . noLoc)) flds
                              ; return $ mkRdrRecordCon c' (HsRecFields flds' Nothing) }
    cvt (RecUpdE e flds) = do { e' <- cvtl e
                              ; flds'
                                  <- mapM (cvtFld (mkAmbiguousFieldOcc . noLoc))
                                           flds
                              ; return $ mkRdrRecordUpd e' flds' }
    cvt (StaticE e)      = fmap (HsStatic noExtField) $ cvtl e
    cvt (UnboundVarE s)  = do -- Use of 'vcName' here instead of 'vName' is
                              -- important, because UnboundVarE may contain
                              -- constructor names - see #14627.
                              { s' <- vcName s
                              ; return $ HsVar noExtField (noLoc s') }
    cvt (LabelE s)       = do { return $ HsOverLabel noExtField Nothing (fsLit s) }
    cvt (ImplicitParamVarE n) = do { n' <- ipName n; return $ HsIPVar noExtField n' }

{- | #16895 Ensure an infix expression's operator is a variable/constructor.
Consider this example:

  $(uInfixE [|1|] [|id id|] [|2|])

This infix expression is obviously ill-formed so we use this helper function
to reject such programs outright.

The constructors `ensureValidOpExp` permits should be in sync with `pprInfixExp`
in Language.Haskell.TH.Ppr from the template-haskell library.
-}
ensureValidOpExp :: TH.Exp -> CvtM a -> CvtM a
ensureValidOpExp (VarE _n) m = m
ensureValidOpExp (ConE _n) m = m
ensureValidOpExp (UnboundVarE _n) m = m
ensureValidOpExp _e _m =
    failWith (text "Non-variable expression is not allowed in an infix expression")

{- Note [Dropping constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we drop constructors from the input, we must insert parentheses around the
argument. For example:

  UInfixE x * (AppE (InfixE (Just y) + Nothing) z)

If we convert the InfixE expression to an operator section but don't insert
parentheses, the above expression would be reassociated to

  OpApp (OpApp x * y) + z

which we don't want.
-}

cvtFld :: (RdrName -> t) -> (TH.Name, TH.Exp)
       -> CvtM (LHsRecField' t (LHsExpr GhcPs))
cvtFld f (v,e)
  = do  { v' <- vNameL v; e' <- cvtl e
        ; return (noLoc $ HsRecField { hsRecFieldLbl = fmap f v'
                                     , hsRecFieldArg = e'
                                     , hsRecPun      = False}) }

cvtDD :: Range -> CvtM (ArithSeqInfo GhcPs)
cvtDD (FromR x)           = do { x' <- cvtl x; return $ From x' }
cvtDD (FromThenR x y)     = do { x' <- cvtl x; y' <- cvtl y; return $ FromThen x' y' }
cvtDD (FromToR x y)       = do { x' <- cvtl x; y' <- cvtl y; return $ FromTo x' y' }
cvtDD (FromThenToR x y z) = do { x' <- cvtl x; y' <- cvtl y; z' <- cvtl z; return $ FromThenTo x' y' z' }

cvt_tup :: [Maybe Exp] -> Boxity -> CvtM (HsExpr GhcPs)
cvt_tup es boxity = do { let cvtl_maybe Nothing  = return missingTupArg
                             cvtl_maybe (Just e) = fmap (Present noExtField) (cvtl e)
                       ; es' <- mapM cvtl_maybe es
                       ; return $ ExplicitTuple
                                    noExtField
                                    (map noLoc es')
                                    boxity }

{- Note [Operator association]
We must be quite careful about adding parens:
  * Infix (UInfix ...) op arg      Needs parens round the first arg
  * Infix (Infix ...) op arg       Needs parens round the first arg
  * UInfix (UInfix ...) op arg     No parens for first arg
  * UInfix (Infix ...) op arg      Needs parens round first arg


Note [Converting UInfix]
~~~~~~~~~~~~~~~~~~~~~~~~
When converting @UInfixE@, @UInfixP@, and @UInfixT@ values, we want to readjust
the trees to reflect the fixities of the underlying operators:

  UInfixE x * (UInfixE y + z) ---> (x * y) + z

This is done by the renamer (see @mkOppAppRn@, @mkConOppPatRn@, and
@mkHsOpTyRn@ in GHC.Rename.Types), which expects that the input will be completely
right-biased for types and left-biased for everything else. So we left-bias the
trees of @UInfixP@ and @UInfixE@ and right-bias the trees of @UInfixT@.

Sample input:

  UInfixE
   (UInfixE x op1 y)
   op2
   (UInfixE z op3 w)

Sample output:

  OpApp
    (OpApp
      (OpApp x op1 y)
      op2
      z)
    op3
    w

The functions @cvtOpApp@, @cvtOpAppP@, and @cvtOpAppT@ are responsible for this
biasing.
-}

{- | @cvtOpApp x op y@ converts @op@ and @y@ and produces the operator application @x `op` y@.
The produced tree of infix expressions will be left-biased, provided @x@ is.

We can see that @cvtOpApp@ is correct as follows. The inductive hypothesis
is that @cvtOpApp x op y@ is left-biased, provided @x@ is. It is clear that
this holds for both branches (of @cvtOpApp@), provided we assume it holds for
the recursive calls to @cvtOpApp@.

When we call @cvtOpApp@ from @cvtl@, the first argument will always be left-biased
since we have already run @cvtl@ on it.
-}
cvtOpApp :: LHsExpr GhcPs -> TH.Exp -> TH.Exp -> CvtM (HsExpr GhcPs)
cvtOpApp x op1 (UInfixE y op2 z)
  = do { l <- wrapL $ cvtOpApp x op1 y
       ; cvtOpApp l op2 z }
cvtOpApp x op y
  = do { op' <- cvtl op
       ; y' <- cvtl y
       ; return (OpApp noExtField x op' y') }

-------------------------------------
--      Do notation and statements
-------------------------------------

cvtHsDo :: HsStmtContext GhcRn -> [TH.Stmt] -> CvtM (HsExpr GhcPs)
cvtHsDo do_or_lc stmts
  | null stmts = failWith (text "Empty stmt list in do-block")
  | otherwise
  = do  { stmts' <- cvtStmts stmts
        ; let Just (stmts'', last') = snocView stmts'

        ; last'' <- case last' of
                    (L loc (BodyStmt _ body _ _))
                      -> return (L loc (mkLastStmt body))
                    _ -> failWith (bad_last last')

        ; return $ HsDo noExtField do_or_lc (noLoc (stmts'' ++ [last''])) }
  where
    bad_last stmt = vcat [ text "Illegal last statement of" <+> pprAStmtContext do_or_lc <> colon
                         , nest 2 $ Outputable.ppr stmt
                         , text "(It should be an expression.)" ]

cvtStmts :: [TH.Stmt] -> CvtM [Hs.LStmt GhcPs (LHsExpr GhcPs)]
cvtStmts = mapM cvtStmt

cvtStmt :: TH.Stmt -> CvtM (Hs.LStmt GhcPs (LHsExpr GhcPs))
cvtStmt (NoBindS e)    = do { e' <- cvtl e; returnL $ mkBodyStmt e' }
cvtStmt (TH.BindS p e) = do { p' <- cvtPat p; e' <- cvtl e; returnL $ mkBindStmt p' e' }
cvtStmt (TH.LetS ds)   = do { ds' <- cvtLocalDecs (text "a let binding") ds
                            ; returnL $ LetStmt noExtField (noLoc ds') }
cvtStmt (TH.ParS dss)  = do { dss' <- mapM cvt_one dss
                            ; returnL $ ParStmt noExtField dss' noExpr noSyntaxExpr }
  where
    cvt_one ds = do { ds' <- cvtStmts ds
                    ; return (ParStmtBlock noExtField ds' undefined noSyntaxExpr) }
cvtStmt (TH.RecS ss) = do { ss' <- mapM cvtStmt ss; returnL (mkRecStmt ss') }

cvtMatch :: HsMatchContext GhcPs
         -> TH.Match -> CvtM (Hs.LMatch GhcPs (LHsExpr GhcPs))
cvtMatch ctxt (TH.Match p body decs)
  = do  { p' <- cvtPat p
        ; let lp = case p' of
                     (L loc SigPat{}) -> L loc (ParPat noExtField p') -- #14875
                     _                -> p'
        ; g' <- cvtGuard body
        ; decs' <- cvtLocalDecs (text "a where clause") decs
        ; returnL $ Hs.Match noExtField ctxt [lp] (GRHSs noExtField g' (noLoc decs')) }

cvtGuard :: TH.Body -> CvtM [LGRHS GhcPs (LHsExpr GhcPs)]
cvtGuard (GuardedB pairs) = mapM cvtpair pairs
cvtGuard (NormalB e)      = do { e' <- cvtl e
                               ; g' <- returnL $ GRHS noExtField [] e'; return [g'] }

cvtpair :: (TH.Guard, TH.Exp) -> CvtM (LGRHS GhcPs (LHsExpr GhcPs))
cvtpair (NormalG ge,rhs) = do { ge' <- cvtl ge; rhs' <- cvtl rhs
                              ; g' <- returnL $ mkBodyStmt ge'
                              ; returnL $ GRHS noExtField [g'] rhs' }
cvtpair (PatG gs,rhs)    = do { gs' <- cvtStmts gs; rhs' <- cvtl rhs
                              ; returnL $ GRHS noExtField gs' rhs' }

cvtOverLit :: Lit -> CvtM (HsOverLit GhcPs)
cvtOverLit (IntegerL i)
  = do { force i; return $ mkHsIntegral   (mkIntegralLit i) }
cvtOverLit (RationalL r)
  = do { force r; return $ mkHsFractional (mkFractionalLit r) }
cvtOverLit (StringL s)
  = do { let { s' = mkFastString s }
       ; force s'
       ; return $ mkHsIsString (quotedSourceText s) s'
       }
cvtOverLit _ = panic "Convert.cvtOverLit: Unexpected overloaded literal"
-- An Integer is like an (overloaded) '3' in a Haskell source program
-- Similarly 3.5 for fractionals

{- Note [Converting strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we get (ListE [CharL 'x', CharL 'y']) we'd like to convert to
a string literal for "xy".  Of course, we might hope to get
(LitE (StringL "xy")), but not always, and allCharLs fails quickly
if it isn't a literal string
-}

allCharLs :: [TH.Exp] -> Maybe String
-- Note [Converting strings]
-- NB: only fire up this setup for a non-empty list, else
--     there's a danger of returning "" for [] :: [Int]!
allCharLs xs
  = case xs of
      LitE (CharL c) : ys -> go [c] ys
      _                   -> Nothing
  where
    go cs []                    = Just (reverse cs)
    go cs (LitE (CharL c) : ys) = go (c:cs) ys
    go _  _                     = Nothing

cvtLit :: Lit -> CvtM (HsLit GhcPs)
cvtLit (IntPrimL i)    = do { force i; return $ HsIntPrim NoSourceText i }
cvtLit (WordPrimL w)   = do { force w; return $ HsWordPrim NoSourceText w }
cvtLit (FloatPrimL f)
  = do { force f; return $ HsFloatPrim noExtField (mkFractionalLit f) }
cvtLit (DoublePrimL f)
  = do { force f; return $ HsDoublePrim noExtField (mkFractionalLit f) }
cvtLit (CharL c)       = do { force c; return $ HsChar NoSourceText c }
cvtLit (CharPrimL c)   = do { force c; return $ HsCharPrim NoSourceText c }
cvtLit (StringL s)     = do { let { s' = mkFastString s }
                            ; force s'
                            ; return $ HsString (quotedSourceText s) s' }
cvtLit (StringPrimL s) = do { let { s' = BS.pack s }
                            ; force s'
                            ; return $ HsStringPrim NoSourceText s' }
cvtLit (BytesPrimL (Bytes fptr off sz)) = do
  let bs = unsafePerformIO $ withForeignPtr fptr $ \ptr ->
             BS.packCStringLen (ptr `plusPtr` fromIntegral off, fromIntegral sz)
  force bs
  return $ HsStringPrim NoSourceText bs
cvtLit _ = panic "Convert.cvtLit: Unexpected literal"
        -- cvtLit should not be called on IntegerL, RationalL
        -- That precondition is established right here in
        -- Convert.hs, hence panic

quotedSourceText :: String -> SourceText
quotedSourceText s = SourceText $ "\"" ++ s ++ "\""

cvtPats :: [TH.Pat] -> CvtM [Hs.LPat GhcPs]
cvtPats pats = mapM cvtPat pats

cvtPat :: TH.Pat -> CvtM (Hs.LPat GhcPs)
cvtPat pat = wrapL (cvtp pat)

cvtp :: TH.Pat -> CvtM (Hs.Pat GhcPs)
cvtp (TH.LitP l)
  | overloadedLit l    = do { l' <- cvtOverLit l
                            ; return (mkNPat (noLoc l') Nothing) }
                                  -- Not right for negative patterns;
                                  -- need to think about that!
  | otherwise          = do { l' <- cvtLit l; return $ Hs.LitPat noExtField l' }
cvtp (TH.VarP s)       = do { s' <- vName s
                            ; return $ Hs.VarPat noExtField (noLoc s') }
cvtp (TupP ps)         = do { ps' <- cvtPats ps
                            ; return $ TuplePat noExtField ps' Boxed }
cvtp (UnboxedTupP ps)  = do { ps' <- cvtPats ps
                            ; return $ TuplePat noExtField ps' Unboxed }
cvtp (UnboxedSumP p alt arity)
                       = do { p' <- cvtPat p
                            ; unboxedSumChecks alt arity
                            ; return $ SumPat noExtField p' alt arity }
cvtp (ConP s ps)       = do { s' <- cNameL s; ps' <- cvtPats ps
                            ; let pps = map (parenthesizePat appPrec) ps'
                            ; return $ ConPat s' (PrefixCon pps) NoExtField }
cvtp (InfixP p1 s p2)  = do { s' <- cNameL s; p1' <- cvtPat p1; p2' <- cvtPat p2
                            ; wrapParL (ParPat noExtField) $
                              ConPat s'
                                (InfixCon (parenthesizePat opPrec p1')
                                          (parenthesizePat opPrec p2'))
                                NoExtField
                            }
                            -- See Note [Operator association]
cvtp (UInfixP p1 s p2) = do { p1' <- cvtPat p1; cvtOpAppP p1' s p2 } -- Note [Converting UInfix]
cvtp (ParensP p)       = do { p' <- cvtPat p;
                            ; case unLoc p' of  -- may be wrapped ConPatIn
                                ParPat {} -> return $ unLoc p'
                                _         -> return $ ParPat noExtField p' }
cvtp (TildeP p)        = do { p' <- cvtPat p; return $ LazyPat noExtField p' }
cvtp (BangP p)         = do { p' <- cvtPat p; return $ BangPat noExtField p' }
cvtp (TH.AsP s p)      = do { s' <- vNameL s; p' <- cvtPat p
                            ; return $ AsPat noExtField s' p' }
cvtp TH.WildP          = return $ WildPat noExtField
cvtp (RecP c fs)       = do { c' <- cNameL c; fs' <- mapM cvtPatFld fs
                            ; return $ ConPat c'
                                (Hs.RecCon $ HsRecFields fs' Nothing)
                                NoExtField
                            }
cvtp (ListP ps)        = do { ps' <- cvtPats ps
                            ; return
                                   $ ListPat noExtField ps'}
cvtp (SigP p t)        = do { p' <- cvtPat p; t' <- cvtType t
                            ; return $ SigPat noExtField p' (mkLHsSigWcType t') }
cvtp (ViewP e p)       = do { e' <- cvtl e; p' <- cvtPat p
                            ; return $ ViewPat noExtField e' p'}

cvtPatFld :: (TH.Name, TH.Pat) -> CvtM (LHsRecField GhcPs (LPat GhcPs))
cvtPatFld (s,p)
  = do  { L ls s' <- vNameL s
        ; p' <- cvtPat p
        ; return (noLoc $ HsRecField { hsRecFieldLbl
                                         = L ls $ mkFieldOcc (L ls s')
                                     , hsRecFieldArg = p'
                                     , hsRecPun      = False}) }

{- | @cvtOpAppP x op y@ converts @op@ and @y@ and produces the operator application @x `op` y@.
The produced tree of infix patterns will be left-biased, provided @x@ is.

See the @cvtOpApp@ documentation for how this function works.
-}
cvtOpAppP :: Hs.LPat GhcPs -> TH.Name -> TH.Pat -> CvtM (Hs.Pat GhcPs)
cvtOpAppP x op1 (UInfixP y op2 z)
  = do { l <- wrapL $ cvtOpAppP x op1 y
       ; cvtOpAppP l op2 z }
cvtOpAppP x op y
  = do { op' <- cNameL op
       ; y' <- cvtPat y
       ; return $ ConPat op' (InfixCon x y') NoExtField
       }

-----------------------------------------------------------
--      Types and type variables

cvtTvs :: [TH.TyVarBndr] -> CvtM (LHsQTyVars GhcPs)
cvtTvs tvs = do { tvs' <- mapM cvt_tv tvs; return (mkHsQTvs tvs') }

cvt_tv :: TH.TyVarBndr -> CvtM (LHsTyVarBndr GhcPs)
cvt_tv (TH.PlainTV nm)
  = do { nm' <- tNameL nm
       ; returnL $ UserTyVar noExtField nm' }
cvt_tv (TH.KindedTV nm ki)
  = do { nm' <- tNameL nm
       ; ki' <- cvtKind ki
       ; returnL $ KindedTyVar noExtField nm' ki' }

cvtRole :: TH.Role -> Maybe Coercion.Role
cvtRole TH.NominalR          = Just Coercion.Nominal
cvtRole TH.RepresentationalR = Just Coercion.Representational
cvtRole TH.PhantomR          = Just Coercion.Phantom
cvtRole TH.InferR            = Nothing

cvtContext :: PprPrec -> TH.Cxt -> CvtM (LHsContext GhcPs)
cvtContext p tys = do { preds' <- mapM cvtPred tys
                      ; parenthesizeHsContext p <$> returnL preds' }

cvtPred :: TH.Pred -> CvtM (LHsType GhcPs)
cvtPred = cvtType

cvtDerivClause :: TH.DerivClause
               -> CvtM (LHsDerivingClause GhcPs)
cvtDerivClause (TH.DerivClause ds ctxt)
  = do { ctxt' <- fmap (map mkLHsSigType) <$> cvtContext appPrec ctxt
       ; ds'   <- traverse cvtDerivStrategy ds
       ; returnL $ HsDerivingClause noExtField ds' ctxt' }

cvtDerivStrategy :: TH.DerivStrategy -> CvtM (Hs.LDerivStrategy GhcPs)
cvtDerivStrategy TH.StockStrategy    = returnL Hs.StockStrategy
cvtDerivStrategy TH.AnyclassStrategy = returnL Hs.AnyclassStrategy
cvtDerivStrategy TH.NewtypeStrategy  = returnL Hs.NewtypeStrategy
cvtDerivStrategy (TH.ViaStrategy ty) = do
  ty' <- cvtType ty
  returnL $ Hs.ViaStrategy (mkLHsSigType ty')

cvtType :: TH.Type -> CvtM (LHsType GhcPs)
cvtType = cvtTypeKind "type"

cvtTypeKind :: String -> TH.Type -> CvtM (LHsType GhcPs)
cvtTypeKind ty_str ty
  = do { (head_ty, tys') <- split_ty_app ty
       ; let m_normals = mapM extract_normal tys'
                                where extract_normal (HsValArg ty) = Just ty
                                      extract_normal _ = Nothing

       ; case head_ty of
           TupleT n
            | Just normals <- m_normals
            , normals `lengthIs` n         -- Saturated
            -> returnL (HsTupleTy noExtField HsBoxedOrConstraintTuple normals)
            | otherwise
            -> mk_apps
               (HsTyVar noExtField NotPromoted (noLoc (getRdrName (tupleTyCon Boxed n))))
               tys'
           UnboxedTupleT n
             | Just normals <- m_normals
             , normals `lengthIs` n               -- Saturated
             -> returnL (HsTupleTy noExtField HsUnboxedTuple normals)
             | otherwise
             -> mk_apps
                (HsTyVar noExtField NotPromoted (noLoc (getRdrName (tupleTyCon Unboxed n))))
                tys'
           UnboxedSumT n
             | n < 2
            -> failWith $
                   vcat [ text "Illegal sum arity:" <+> text (show n)
                        , nest 2 $
                            text "Sums must have an arity of at least 2" ]
             | Just normals <- m_normals
             , normals `lengthIs` n -- Saturated
             -> returnL (HsSumTy noExtField normals)
             | otherwise
             -> mk_apps
                (HsTyVar noExtField NotPromoted (noLoc (getRdrName (sumTyCon n))))
                tys'
           ArrowT
             | Just normals <- m_normals
             , [x',y'] <- normals -> do
                 x'' <- case unLoc x' of
                          HsFunTy{}    -> returnL (HsParTy noExtField x')
                          HsForAllTy{} -> returnL (HsParTy noExtField x') -- #14646
                          HsQualTy{}   -> returnL (HsParTy noExtField x') -- #15324
                          _            -> return $
                                          parenthesizeHsType sigPrec x'
                 let y'' = parenthesizeHsType sigPrec y'
                 returnL (HsFunTy noExtField x'' y'')
             | otherwise
             -> mk_apps
                (HsTyVar noExtField NotPromoted (noLoc (getRdrName funTyCon)))
                tys'
           ListT
             | Just normals <- m_normals
             , [x'] <- normals -> do
                returnL (HsListTy noExtField x')
             | otherwise
             -> mk_apps
                (HsTyVar noExtField NotPromoted (noLoc (getRdrName listTyCon)))
                tys'

           VarT nm -> do { nm' <- tNameL nm
                         ; mk_apps (HsTyVar noExtField NotPromoted nm') tys' }
           ConT nm -> do { nm' <- tconName nm
                         ; let prom = name_promotedness nm'
                         ; mk_apps (HsTyVar noExtField prom (noLoc nm')) tys'}

           ForallT tvs cxt ty
             | null tys'
             -> do { tvs' <- cvtTvs tvs
                   ; cxt' <- cvtContext funPrec cxt
                   ; ty'  <- cvtType ty
                   ; loc <- getL
                   ; let hs_ty  = mkHsForAllTy tvs loc ForallInvis tvs' rho_ty
                         rho_ty = mkHsQualTy cxt loc cxt' ty'

                   ; return hs_ty }

           ForallVisT tvs ty
             | null tys'
             -> do { tvs' <- cvtTvs tvs
                   ; ty'  <- cvtType ty
                   ; loc  <- getL
                   ; pure $ mkHsForAllTy tvs loc ForallVis tvs' ty' }

           SigT ty ki
             -> do { ty' <- cvtType ty
                   ; ki' <- cvtKind ki
                   ; mk_apps (HsKindSig noExtField ty' ki') tys'
                   }

           LitT lit
             -> mk_apps (HsTyLit noExtField (cvtTyLit lit)) tys'

           WildCardT
             -> mk_apps mkAnonWildCardTy tys'

           InfixT t1 s t2
             -> do { s'  <- tconName s
                   ; t1' <- cvtType t1
                   ; t2' <- cvtType t2
                   ; let prom = name_promotedness s'
                   ; mk_apps
                      (HsTyVar noExtField prom (noLoc s'))
                      ([HsValArg t1', HsValArg t2'] ++ tys')
                   }

           UInfixT t1 s t2
             -> do { t2' <- cvtType t2
                   ; t <- cvtOpAppT t1 s t2'
                   ; mk_apps (unLoc t) tys'
                   } -- Note [Converting UInfix]

           ParensT t
             -> do { t' <- cvtType t
                   ; mk_apps (HsParTy noExtField t') tys'
                   }

           PromotedT nm -> do { nm' <- cName nm
                              ; mk_apps (HsTyVar noExtField IsPromoted (noLoc nm'))
                                        tys' }
                 -- Promoted data constructor; hence cName

           PromotedTupleT n
              | Just normals <- m_normals
              , normals `lengthIs` n   -- Saturated
              -> returnL (HsExplicitTupleTy noExtField normals)
              | otherwise
              -> mk_apps
                 (HsTyVar noExtField IsPromoted (noLoc (getRdrName (tupleDataCon Boxed n))))
                 tys'

           PromotedNilT
             -> mk_apps (HsExplicitListTy noExtField IsPromoted []) tys'

           PromotedConsT  -- See Note [Representing concrete syntax in types]
                          -- in Language.Haskell.TH.Syntax
              | Just normals <- m_normals
              , [ty1, L _ (HsExplicitListTy _ ip tys2)] <- normals
              -> do
                  returnL (HsExplicitListTy noExtField ip (ty1:tys2))
              | otherwise
              -> mk_apps
                 (HsTyVar noExtField IsPromoted (noLoc (getRdrName consDataCon)))
                 tys'

           StarT
             -> mk_apps
                (HsTyVar noExtField NotPromoted (noLoc (getRdrName liftedTypeKindTyCon)))
                tys'

           ConstraintT
             -> mk_apps
                (HsTyVar noExtField NotPromoted (noLoc (getRdrName constraintKindTyCon)))
                tys'

           EqualityT
             | Just normals <- m_normals
             , [x',y'] <- normals ->
                   let px = parenthesizeHsType opPrec x'
                       py = parenthesizeHsType opPrec y'
                   in returnL (HsOpTy noExtField px (noLoc eqTyCon_RDR) py)
               -- The long-term goal is to remove the above case entirely and
               -- subsume it under the case for InfixT. See #15815, comment:6,
               -- for more details.

             | otherwise ->
                   mk_apps (HsTyVar noExtField NotPromoted
                            (noLoc eqTyCon_RDR)) tys'
           ImplicitParamT n t
             -> do { n' <- wrapL $ ipName n
                   ; t' <- cvtType t
                   ; returnL (HsIParamTy noExtField n' t')
                   }

           _ -> failWith (ptext (sLit ("Malformed " ++ ty_str)) <+> text (show ty))
    }

-- ConT/InfixT can contain both data constructor (i.e., promoted) names and
-- other (i.e, unpromoted) names, as opposed to PromotedT, which can only
-- contain data constructor names. See #15572/#17394. We use this function to
-- determine whether to mark a name as promoted/unpromoted when dealing with
-- ConT/InfixT.
name_promotedness :: RdrName -> Hs.PromotionFlag
name_promotedness nm
  | isRdrDataCon nm = IsPromoted
  | otherwise       = NotPromoted

-- | Constructs an application of a type to arguments passed in a list.
mk_apps :: HsType GhcPs -> [LHsTypeArg GhcPs] -> CvtM (LHsType GhcPs)
mk_apps head_ty type_args = do
  head_ty' <- returnL head_ty
  -- We must parenthesize the function type in case of an explicit
  -- signature. For instance, in `(Maybe :: Type -> Type) Int`, there
  -- _must_ be parentheses around `Maybe :: Type -> Type`.
  let phead_ty :: LHsType GhcPs
      phead_ty = parenthesizeHsType sigPrec head_ty'

      go :: [LHsTypeArg GhcPs] -> CvtM (LHsType GhcPs)
      go [] = pure head_ty'
      go (arg:args) =
        case arg of
          HsValArg ty  -> do p_ty <- add_parens ty
                             mk_apps (HsAppTy noExtField phead_ty p_ty) args
          HsTypeArg l ki -> do p_ki <- add_parens ki
                               mk_apps (HsAppKindTy l phead_ty p_ki) args
          HsArgPar _   -> mk_apps (HsParTy noExtField phead_ty) args

  go type_args
   where
    -- See Note [Adding parens for splices]
    add_parens lt@(L _ t)
      | hsTypeNeedsParens appPrec t = returnL (HsParTy noExtField lt)
      | otherwise                   = return lt

wrap_tyarg :: LHsTypeArg GhcPs -> LHsTypeArg GhcPs
wrap_tyarg (HsValArg ty)    = HsValArg  $ parenthesizeHsType appPrec ty
wrap_tyarg (HsTypeArg l ki) = HsTypeArg l $ parenthesizeHsType appPrec ki
wrap_tyarg ta@(HsArgPar {}) = ta -- Already parenthesized

-- ---------------------------------------------------------------------
-- Note [Adding parens for splices]
{-
The hsSyn representation of parsed source explicitly contains all the original
parens, as written in the source.

When a Template Haskell (TH) splice is evaluated, the original splice is first
renamed and type checked and then finally converted to core in DsMeta. This core
is then run in the TH engine, and the result comes back as a TH AST.

In the process, all parens are stripped out, as they are not needed.

This Convert module then converts the TH AST back to hsSyn AST.

In order to pretty-print this hsSyn AST, parens need to be adde back at certain
points so that the code is readable with its original meaning.

So scattered through Convert.hs are various points where parens are added.

See (among other closed issued) https://gitlab.haskell.org/ghc/ghc/issues/14289
-}
-- ---------------------------------------------------------------------

-- | Constructs an arrow type with a specified return type
mk_arr_apps :: [LHsType GhcPs] -> HsType GhcPs -> CvtM (LHsType GhcPs)
mk_arr_apps tys return_ty = foldrM go return_ty tys >>= returnL
    where go :: LHsType GhcPs -> HsType GhcPs -> CvtM (HsType GhcPs)
          go arg ret_ty = do { ret_ty_l <- returnL ret_ty
                             ; return (HsFunTy noExtField arg ret_ty_l) }

split_ty_app :: TH.Type -> CvtM (TH.Type, [LHsTypeArg GhcPs])
split_ty_app ty = go ty []
  where
    go (AppT f a) as' = do { a' <- cvtType a; go f (HsValArg a':as') }
    go (AppKindT ty ki) as' = do { ki' <- cvtKind ki
                                 ; go ty (HsTypeArg noSrcSpan ki':as') }
    go (ParensT t) as' = do { loc <- getL; go t (HsArgPar loc: as') }
    go f as           = return (f,as)

cvtTyLit :: TH.TyLit -> HsTyLit
cvtTyLit (TH.NumTyLit i) = HsNumTy NoSourceText i
cvtTyLit (TH.StrTyLit s) = HsStrTy NoSourceText (fsLit s)

{- | @cvtOpAppT x op y@ converts @op@ and @y@ and produces the operator
application @x `op` y@. The produced tree of infix types will be right-biased,
provided @y@ is.

See the @cvtOpApp@ documentation for how this function works.
-}
cvtOpAppT :: TH.Type -> TH.Name -> LHsType GhcPs -> CvtM (LHsType GhcPs)
cvtOpAppT (UInfixT x op2 y) op1 z
  = do { l <- cvtOpAppT y op1 z
       ; cvtOpAppT x op2 l }
cvtOpAppT x op y
  = do { op' <- tconNameL op
       ; x' <- cvtType x
       ; returnL (mkHsOpTy x' op' y) }

cvtKind :: TH.Kind -> CvtM (LHsKind GhcPs)
cvtKind = cvtTypeKind "kind"

-- | Convert Maybe Kind to a type family result signature. Used with data
-- families where naming of the result is not possible (thus only kind or no
-- signature is possible).
cvtMaybeKindToFamilyResultSig :: Maybe TH.Kind
                              -> CvtM (LFamilyResultSig GhcPs)
cvtMaybeKindToFamilyResultSig Nothing   = returnL (Hs.NoSig noExtField)
cvtMaybeKindToFamilyResultSig (Just ki) = do { ki' <- cvtKind ki
                                             ; returnL (Hs.KindSig noExtField ki') }

-- | Convert type family result signature. Used with both open and closed type
-- families.
cvtFamilyResultSig :: TH.FamilyResultSig -> CvtM (Hs.LFamilyResultSig GhcPs)
cvtFamilyResultSig TH.NoSig           = returnL (Hs.NoSig noExtField)
cvtFamilyResultSig (TH.KindSig ki)    = do { ki' <- cvtKind ki
                                           ; returnL (Hs.KindSig noExtField  ki') }
cvtFamilyResultSig (TH.TyVarSig bndr) = do { tv <- cvt_tv bndr
                                           ; returnL (Hs.TyVarSig noExtField tv) }

-- | Convert injectivity annotation of a type family.
cvtInjectivityAnnotation :: TH.InjectivityAnn
                         -> CvtM (Hs.LInjectivityAnn GhcPs)
cvtInjectivityAnnotation (TH.InjectivityAnn annLHS annRHS)
  = do { annLHS' <- tNameL annLHS
       ; annRHS' <- mapM tNameL annRHS
       ; returnL (Hs.InjectivityAnn annLHS' annRHS') }

cvtPatSynSigTy :: TH.Type -> CvtM (LHsType GhcPs)
-- pattern synonym types are of peculiar shapes, which is why we treat
-- them separately from regular types;
-- see Note [Pattern synonym type signatures and Template Haskell]
cvtPatSynSigTy (ForallT univs reqs (ForallT exis provs ty))
  | null exis, null provs = cvtType (ForallT univs reqs ty)
  | null univs, null reqs = do { l   <- getL
                               ; ty' <- cvtType (ForallT exis provs ty)
                               ; return $ L l (HsQualTy { hst_ctxt = L l []
                                                        , hst_xqual = noExtField
                                                        , hst_body = ty' }) }
  | null reqs             = do { l      <- getL
                               ; univs' <- hsQTvExplicit <$> cvtTvs univs
                               ; ty'    <- cvtType (ForallT exis provs ty)
                               ; let forTy = HsForAllTy
                                              { hst_fvf = ForallInvis
                                              , hst_bndrs = univs'
                                              , hst_xforall = noExtField
                                              , hst_body = L l cxtTy }
                                     cxtTy = HsQualTy { hst_ctxt = L l []
                                                      , hst_xqual = noExtField
                                                      , hst_body = ty' }
                               ; return $ L l forTy }
  | otherwise             = cvtType (ForallT univs reqs (ForallT exis provs ty))
cvtPatSynSigTy ty         = cvtType ty

-----------------------------------------------------------
cvtFixity :: TH.Fixity -> Hs.Fixity
cvtFixity (TH.Fixity prec dir) = Hs.Fixity NoSourceText prec (cvt_dir dir)
   where
     cvt_dir TH.InfixL = Hs.InfixL
     cvt_dir TH.InfixR = Hs.InfixR
     cvt_dir TH.InfixN = Hs.InfixN

-----------------------------------------------------------


-----------------------------------------------------------
-- some useful things

overloadedLit :: Lit -> Bool
-- True for literals that Haskell treats as overloaded
overloadedLit (IntegerL  _) = True
overloadedLit (RationalL _) = True
overloadedLit _             = False

-- Checks that are performed when converting unboxed sum expressions and
-- patterns alike.
unboxedSumChecks :: TH.SumAlt -> TH.SumArity -> CvtM ()
unboxedSumChecks alt arity
    | alt > arity
    = failWith $ text "Sum alternative"    <+> text (show alt)
             <+> text "exceeds its arity," <+> text (show arity)
    | alt <= 0
    = failWith $ vcat [ text "Illegal sum alternative:" <+> text (show alt)
                      , nest 2 $ text "Sum alternatives must start from 1" ]
    | arity < 2
    = failWith $ vcat [ text "Illegal sum arity:" <+> text (show arity)
                      , nest 2 $ text "Sums must have an arity of at least 2" ]
    | otherwise
    = return ()

-- | If passed an empty list of 'TH.TyVarBndr's, this simply returns the
-- third argument (an 'LHsType'). Otherwise, return an 'HsForAllTy'
-- using the provided 'LHsQTyVars' and 'LHsType'.
mkHsForAllTy :: [TH.TyVarBndr]
             -- ^ The original Template Haskell type variable binders
             -> SrcSpan
             -- ^ The location of the returned 'LHsType' if it needs an
             --   explicit forall
             -> ForallVisFlag
             -- ^ Whether this is @forall@ is visible (e.g., @forall a ->@)
             --   or invisible (e.g., @forall a.@)
             -> LHsQTyVars GhcPs
             -- ^ The converted type variable binders
             -> LHsType GhcPs
             -- ^ The converted rho type
             -> LHsType GhcPs
             -- ^ The complete type, quantified with a forall if necessary
mkHsForAllTy tvs loc fvf tvs' rho_ty
  | null tvs  = rho_ty
  | otherwise = L loc $ HsForAllTy { hst_fvf = fvf
                                   , hst_bndrs = hsQTvExplicit tvs'
                                   , hst_xforall = noExtField
                                   , hst_body = rho_ty }

-- | If passed an empty 'TH.Cxt', this simply returns the third argument
-- (an 'LHsType'). Otherwise, return an 'HsQualTy' using the provided
-- 'LHsContext' and 'LHsType'.

-- It's important that we don't build an HsQualTy if the context is empty,
-- as the pretty-printer for HsType _always_ prints contexts, even if
-- they're empty. See #13183.
mkHsQualTy :: TH.Cxt
           -- ^ The original Template Haskell context
           -> SrcSpan
           -- ^ The location of the returned 'LHsType' if it needs an
           --   explicit context
           -> LHsContext GhcPs
           -- ^ The converted context
           -> LHsType GhcPs
           -- ^ The converted tau type
           -> LHsType GhcPs
           -- ^ The complete type, qualified with a context if necessary
mkHsQualTy ctxt loc ctxt' ty
  | null ctxt = ty
  | otherwise = L loc $ HsQualTy { hst_xqual = noExtField
                                 , hst_ctxt  = ctxt'
                                 , hst_body  = ty }

--------------------------------------------------------------------
--      Turning Name back into RdrName
--------------------------------------------------------------------

-- variable names
vNameL, cNameL, vcNameL, tNameL, tconNameL :: TH.Name -> CvtM (Located RdrName)
vName,  cName,  vcName,  tName,  tconName  :: TH.Name -> CvtM RdrName

-- Variable names
vNameL n = wrapL (vName n)
vName n = cvtName OccName.varName n

-- Constructor function names; this is Haskell source, hence srcDataName
cNameL n = wrapL (cName n)
cName n = cvtName OccName.dataName n

-- Variable *or* constructor names; check by looking at the first char
vcNameL n = wrapL (vcName n)
vcName n = if isVarName n then vName n else cName n

-- Type variable names
tNameL n = wrapL (tName n)
tName n = cvtName OccName.tvName n

-- Type Constructor names
tconNameL n = wrapL (tconName n)
tconName n = cvtName OccName.tcClsName n

ipName :: String -> CvtM HsIPName
ipName n
  = do { unless (okVarOcc n) (failWith (badOcc OccName.varName n))
       ; return (HsIPName (fsLit n)) }

cvtName :: OccName.NameSpace -> TH.Name -> CvtM RdrName
cvtName ctxt_ns (TH.Name occ flavour)
  | not (okOcc ctxt_ns occ_str) = failWith (badOcc ctxt_ns occ_str)
  | otherwise
  = do { loc <- getL
       ; let rdr_name = thRdrName loc ctxt_ns occ_str flavour
       ; force rdr_name
       ; return rdr_name }
  where
    occ_str = TH.occString occ

okOcc :: OccName.NameSpace -> String -> Bool
okOcc ns str
  | OccName.isVarNameSpace ns     = okVarOcc str
  | OccName.isDataConNameSpace ns = okConOcc str
  | otherwise                     = okTcOcc  str

-- Determine the name space of a name in a type
--
isVarName :: TH.Name -> Bool
isVarName (TH.Name occ _)
  = case TH.occString occ of
      ""    -> False
      (c:_) -> startsVarId c || startsVarSym c

badOcc :: OccName.NameSpace -> String -> SDoc
badOcc ctxt_ns occ
  = text "Illegal" <+> pprNameSpace ctxt_ns
        <+> text "name:" <+> quotes (text occ)

thRdrName :: SrcSpan -> OccName.NameSpace -> String -> TH.NameFlavour -> RdrName
-- This turns a TH Name into a RdrName; used for both binders and occurrences
-- See Note [Binders in Template Haskell]
-- The passed-in name space tells what the context is expecting;
--      use it unless the TH name knows what name-space it comes
--      from, in which case use the latter
--
-- We pass in a SrcSpan (gotten from the monad) because this function
-- is used for *binders* and if we make an Exact Name we want it
-- to have a binding site inside it.  (cf #5434)
--
-- ToDo: we may generate silly RdrNames, by passing a name space
--       that doesn't match the string, like VarName ":+",
--       which will give confusing error messages later
--
-- The strict applications ensure that any buried exceptions get forced
thRdrName loc ctxt_ns th_occ th_name
  = case th_name of
     TH.NameG th_ns pkg mod -> thOrigRdrName th_occ th_ns pkg mod
     TH.NameQ mod  -> (mkRdrQual  $! mk_mod mod) $! occ
     TH.NameL uniq -> nameRdrName $! (((Name.mkInternalName $! mk_uniq (fromInteger uniq)) $! occ) loc)
     TH.NameU uniq -> nameRdrName $! (((Name.mkSystemNameAt $! mk_uniq (fromInteger uniq)) $! occ) loc)
     TH.NameS | Just name <- isBuiltInOcc_maybe occ -> nameRdrName $! name
              | otherwise                           -> mkRdrUnqual $! occ
              -- We check for built-in syntax here, because the TH
              -- user might have written a (NameS "(,,)"), for example
  where
    occ :: OccName.OccName
    occ = mk_occ ctxt_ns th_occ

-- Return an unqualified exact RdrName if we're dealing with built-in syntax.
-- See #13776.
thOrigRdrName :: String -> TH.NameSpace -> PkgName -> ModName -> RdrName
thOrigRdrName occ th_ns pkg mod =
  let occ' = mk_occ (mk_ghc_ns th_ns) occ
  in case isBuiltInOcc_maybe occ' of
       Just name -> nameRdrName name
       Nothing   -> (mkOrig $! (mkModule (mk_pkg pkg) (mk_mod mod))) $! occ'

thRdrNameGuesses :: TH.Name -> [RdrName]
thRdrNameGuesses (TH.Name occ flavour)
  -- This special case for NameG ensures that we don't generate duplicates in the output list
  | TH.NameG th_ns pkg mod <- flavour = [ thOrigRdrName occ_str th_ns pkg mod]
  | otherwise                         = [ thRdrName noSrcSpan gns occ_str flavour
                                        | gns <- guessed_nss]
  where
    -- guessed_ns are the name spaces guessed from looking at the TH name
    guessed_nss
      | isLexCon (mkFastString occ_str) = [OccName.tcName,  OccName.dataName]
      | otherwise                       = [OccName.varName, OccName.tvName]
    occ_str = TH.occString occ

-- The packing and unpacking is rather turgid :-(
mk_occ :: OccName.NameSpace -> String -> OccName.OccName
mk_occ ns occ = OccName.mkOccName ns occ

mk_ghc_ns :: TH.NameSpace -> OccName.NameSpace
mk_ghc_ns TH.DataName  = OccName.dataName
mk_ghc_ns TH.TcClsName = OccName.tcClsName
mk_ghc_ns TH.VarName   = OccName.varName

mk_mod :: TH.ModName -> ModuleName
mk_mod mod = mkModuleName (TH.modString mod)

mk_pkg :: TH.PkgName -> UnitId
mk_pkg pkg = stringToUnitId (TH.pkgString pkg)

mk_uniq :: Int -> Unique
mk_uniq u = mkUniqueGrimily u

{-
Note [Binders in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this TH term construction:
  do { x1 <- TH.newName "x"   -- newName :: String -> Q TH.Name
     ; x2 <- TH.newName "x"   -- Builds a NameU
     ; x3 <- TH.newName "x"

     ; let x = mkName "x"     -- mkName :: String -> TH.Name
                              -- Builds a NameS

     ; return (LamE (..pattern [x1,x2]..) $
               LamE (VarPat x3) $
               ..tuple (x1,x2,x3,x)) }

It represents the term   \[x1,x2]. \x3. (x1,x2,x3,x)

a) We don't want to complain about "x" being bound twice in
   the pattern [x1,x2]
b) We don't want x3 to shadow the x1,x2
c) We *do* want 'x' (dynamically bound with mkName) to bind
   to the innermost binding of "x", namely x3.
d) When pretty printing, we want to print a unique with x1,x2
   etc, else they'll all print as "x" which isn't very helpful

When we convert all this to HsSyn, the TH.Names are converted with
thRdrName.  To achieve (b) we want the binders to be Exact RdrNames.
Achieving (a) is a bit awkward, because
   - We must check for duplicate and shadowed names on Names,
     not RdrNames, *after* renaming.
     See Note [Collect binders only after renaming] in GHC.Hs.Utils

   - But to achieve (a) we must distinguish between the Exact
     RdrNames arising from TH and the Unqual RdrNames that would
     come from a user writing \[x,x] -> blah

So in Convert.thRdrName we translate
   TH Name                          RdrName
   --------------------------------------------------------
   NameU (arising from newName) --> Exact (Name{ System })
   NameS (arising from mkName)  --> Unqual

Notice that the NameUs generate *System* Names.  Then, when
figuring out shadowing and duplicates, we can filter out
System Names.

This use of System Names fits with other uses of System Names, eg for
temporary variables "a". Since there are lots of things called "a" we
usually want to print the name with the unique, and that is indeed
the way System Names are printed.

There's a small complication of course; see Note [Looking up Exact
RdrNames] in GHC.Rename.Env.
-}

{-
Note [Pattern synonym type signatures and Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In general, the type signature of a pattern synonym

  pattern P x1 x2 .. xn = <some-pattern>

is of the form

   forall univs. reqs => forall exis. provs => t1 -> t2 -> ... -> tn -> t

with the following parts:

   1) the (possibly empty lists of) universally quantified type
      variables `univs` and required constraints `reqs` on them.
   2) the (possibly empty lists of) existentially quantified type
      variables `exis` and the provided constraints `provs` on them.
   3) the types `t1`, `t2`, .., `tn` of the pattern synonym's arguments x1,
      x2, .., xn, respectively
   4) the type `t` of <some-pattern>, mentioning only universals from `univs`.

Due to the two forall quantifiers and constraint contexts (either of
which might be empty), pattern synonym type signatures are treated
specially in `deSugar/DsMeta.hs`, `hsSyn/Convert.hs`, and
`typecheck/TcSplice.hs`:

   (a) When desugaring a pattern synonym from HsSyn to TH.Dec in
       `deSugar/DsMeta.hs`, we represent its *full* type signature in TH, i.e.:

           ForallT univs reqs (ForallT exis provs ty)
              (where ty is the AST representation of t1 -> t2 -> ... -> tn -> t)

   (b) When converting pattern synonyms from TH.Dec to HsSyn in
       `hsSyn/Convert.hs`, we convert their TH type signatures back to an
       appropriate Haskell pattern synonym type of the form

         forall univs. reqs => forall exis. provs => t1 -> t2 -> ... -> tn -> t

       where initial empty `univs` type variables or an empty `reqs`
       constraint context are represented *explicitly* as `() =>`.

   (c) When reifying a pattern synonym in `typecheck/TcSplice.hs`, we always
       return its *full* type, i.e.:

           ForallT univs reqs (ForallT exis provs ty)
              (where ty is the AST representation of t1 -> t2 -> ... -> tn -> t)

The key point is to always represent a pattern synonym's *full* type
in cases (a) and (c) to make it clear which of the two forall
quantifiers and/or constraint contexts are specified, and which are
not. See GHC's user's guide on pattern synonyms for more information
about pattern synonym type signatures.

-}
