%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

This module converts Template Haskell syntax into HsSyn

\begin{code}
module Convert( convertToHsExpr, convertToPat, convertToHsDecls,
                convertToHsType,
                thRdrNameGuesses ) where

import HsSyn as Hs
import qualified Class
import RdrName
import qualified Name
import Module
import RdrHsSyn
import qualified OccName
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
import Util
import FastString
import Outputable

import qualified Data.ByteString as BS
import Control.Monad( unless, liftM, ap )
import Control.Applicative (Applicative(..))

import Language.Haskell.TH as TH hiding (sigP)
import Language.Haskell.TH.Syntax as TH
import GHC.Exts

-------------------------------------------------------------------
--              The external interface

convertToHsDecls :: SrcSpan -> [TH.Dec] -> Either MsgDoc [LHsDecl RdrName]
convertToHsDecls loc ds = initCvt loc (mapM cvt_dec ds)
  where
    cvt_dec d = wrapMsg "declaration" d (cvtDec d)

convertToHsExpr :: SrcSpan -> TH.Exp -> Either MsgDoc (LHsExpr RdrName)
convertToHsExpr loc e
  = initCvt loc $ wrapMsg "expression" e $ cvtl e

convertToPat :: SrcSpan -> TH.Pat -> Either MsgDoc (LPat RdrName)
convertToPat loc p
  = initCvt loc $ wrapMsg "pattern" p $ cvtPat p

convertToHsType :: SrcSpan -> TH.Type -> Either MsgDoc (LHsType RdrName)
convertToHsType loc t
  = initCvt loc $ wrapMsg "type" t $ cvtType t

-------------------------------------------------------------------
newtype CvtM a = CvtM { unCvtM :: SrcSpan -> Either MsgDoc a }
        -- Push down the source location;
        -- Can fail, with a single error message

-- NB: If the conversion succeeds with (Right x), there should
--     be no exception values hiding in x
-- Reason: so a (head []) in TH code doesn't subsequently
--         make GHC crash when it tries to walk the generated tree

-- Use the loc everywhere, for lack of anything better
-- In particular, we want it on binding locations, so that variables bound in
-- the spliced-in declarations get a location that at least relates to the splice point

instance Functor CvtM where
    fmap = liftM

instance Applicative CvtM where
    pure = return
    (<*>) = ap

instance Monad CvtM where
  return x       = CvtM $ \_   -> Right x
  (CvtM m) >>= k = CvtM $ \loc -> case m loc of
                                  Left err -> Left err
                                  Right v  -> unCvtM (k v) loc

initCvt :: SrcSpan -> CvtM a -> Either MsgDoc a
initCvt loc (CvtM m) = m loc

force :: a -> CvtM ()
force a = a `seq` return ()

failWith :: MsgDoc -> CvtM a
failWith m = CvtM (\_ -> Left m)

getL :: CvtM SrcSpan
getL = CvtM (\loc -> Right loc)

returnL :: a -> CvtM (Located a)
returnL x = CvtM (\loc -> Right (L loc x))

wrapParL :: (Located a -> a) -> a -> CvtM a
wrapParL add_par x = CvtM (\loc -> Right (add_par (L loc x)))

wrapMsg :: (Show a, TH.Ppr a) => String -> a -> CvtM b -> CvtM b
-- E.g  wrapMsg "declaration" dec thing
wrapMsg what item (CvtM m)
  = CvtM (\loc -> case m loc of
                     Left err -> Left (err $$ getPprStyle msg)
                     Right v  -> Right v)
  where
        -- Show the item in pretty syntax normally,
        -- but with all its constructors if you say -dppr-debug
    msg sty = hang (ptext (sLit "When splicing a TH") <+> text what <> colon)
                 2 (if debugStyle sty
                    then text (show item)
                    else text (pprint item))

wrapL :: CvtM a -> CvtM (Located a)
wrapL (CvtM m) = CvtM (\loc -> case m loc of
                               Left err -> Left err
                               Right v  -> Right (L loc v))

-------------------------------------------------------------------
cvtDec :: TH.Dec -> CvtM (LHsDecl RdrName)
cvtDec (TH.ValD pat body ds)
  | TH.VarP s <- pat
  = do  { s' <- vNameL s
        ; cl' <- cvtClause (Clause [] body ds)
        ; returnL $ Hs.ValD $ mkFunBind s' [cl'] }

  | otherwise
  = do  { pat' <- cvtPat pat
        ; body' <- cvtGuard body
        ; ds' <- cvtLocalDecs (ptext (sLit "a where clause")) ds
        ; returnL $ Hs.ValD $
          PatBind { pat_lhs = pat', pat_rhs = GRHSs body' ds'
                  , pat_rhs_ty = void, bind_fvs = placeHolderNames
                  , pat_ticks = (Nothing,[]) } }

cvtDec (TH.FunD nm cls)
  | null cls
  = failWith (ptext (sLit "Function binding for")
                 <+> quotes (text (TH.pprint nm))
                 <+> ptext (sLit "has no equations"))
  | otherwise
  = do  { nm' <- vNameL nm
        ; cls' <- mapM cvtClause cls
        ; returnL $ Hs.ValD $ mkFunBind nm' cls' }

cvtDec (TH.SigD nm typ)
  = do  { nm' <- vNameL nm
        ; ty' <- cvtType typ
        ; returnL $ Hs.SigD (TypeSig [nm'] ty') }

cvtDec (TH.InfixD fx nm)
  = do { nm' <- vNameL nm
       ; returnL (Hs.SigD (FixSig (FixitySig nm' (cvtFixity fx)))) }

cvtDec (PragmaD prag)
  = cvtPragmaD prag

cvtDec (TySynD tc tvs rhs)
  = do  { (_, tc', tvs') <- cvt_tycl_hdr [] tc tvs
        ; rhs' <- cvtType rhs
        ; returnL $ TyClD (SynDecl { tcdLName = tc'
                                  , tcdTyVars = tvs', tcdFVs = placeHolderNames
                                  , tcdRhs = rhs' }) }

cvtDec (DataD ctxt tc tvs constrs derivs)
  = do  { (ctxt', tc', tvs') <- cvt_tycl_hdr ctxt tc tvs
        ; cons' <- mapM cvtConstr constrs
        ; derivs' <- cvtDerivs derivs
        ; let defn = HsDataDefn { dd_ND = DataType, dd_cType = Nothing
                                , dd_ctxt = ctxt'
                                , dd_kindSig = Nothing
                                , dd_cons = cons', dd_derivs = derivs' }
        ; returnL $ TyClD (DataDecl { tcdLName = tc', tcdTyVars = tvs'
                                    , tcdDataDefn = defn, tcdFVs = placeHolderNames }) }

cvtDec (NewtypeD ctxt tc tvs constr derivs)
  = do  { (ctxt', tc', tvs') <- cvt_tycl_hdr ctxt tc tvs
        ; con' <- cvtConstr constr
        ; derivs' <- cvtDerivs derivs
        ; let defn = HsDataDefn { dd_ND = NewType, dd_cType = Nothing
                                , dd_ctxt = ctxt'
                                , dd_kindSig = Nothing
                                , dd_cons = [con'], dd_derivs = derivs' }
        ; returnL $ TyClD (DataDecl { tcdLName = tc', tcdTyVars = tvs'
                                    , tcdDataDefn = defn, tcdFVs = placeHolderNames }) }

cvtDec (ClassD ctxt cl tvs fds decs)
  = do  { (cxt', tc', tvs') <- cvt_tycl_hdr ctxt cl tvs
        ; fds'  <- mapM cvt_fundep fds
        ; (binds', sigs', fams', ats', adts') <- cvt_ci_decs (ptext (sLit "a class declaration")) decs
        ; unless (null adts')
            (failWith $ (ptext (sLit "Default data instance declarations are not allowed:"))
                   $$ (Outputable.ppr adts'))
        ; returnL $ TyClD $
          ClassDecl { tcdCtxt = cxt', tcdLName = tc', tcdTyVars = tvs'
                    , tcdFDs = fds', tcdSigs = sigs', tcdMeths = binds'
                    , tcdATs = fams', tcdATDefs = ats', tcdDocs = []
                    , tcdFVs = placeHolderNames }
                              -- no docs in TH ^^
        }

cvtDec (InstanceD ctxt ty decs)
  = do  { let doc = ptext (sLit "an instance declaration")
        ; (binds', sigs', fams', ats', adts') <- cvt_ci_decs doc decs
        ; unless (null fams') (failWith (mkBadDecMsg doc fams'))
        ; ctxt' <- cvtContext ctxt
        ; L loc ty' <- cvtType ty
        ; let inst_ty' = L loc $ mkImplicitHsForAllTy ctxt' $ L loc ty'
        ; returnL $ InstD (ClsInstD (ClsInstDecl inst_ty' binds' sigs' ats' adts')) }

cvtDec (ForeignD ford)
  = do { ford' <- cvtForD ford
       ; returnL $ ForD ford' }

cvtDec (FamilyD flav tc tvs kind)
  = do { (_, tc', tvs') <- cvt_tycl_hdr [] tc tvs
       ; kind' <- cvtMaybeKind kind
       ; returnL $ TyClD (FamDecl (FamilyDecl (cvtFamFlavour flav) tc' tvs' kind')) }
  where
    cvtFamFlavour TypeFam = OpenTypeFamily
    cvtFamFlavour DataFam = DataFamily

cvtDec (DataInstD ctxt tc tys constrs derivs)
  = do { (ctxt', tc', typats') <- cvt_tyinst_hdr ctxt tc tys
       ; cons' <- mapM cvtConstr constrs
       ; derivs' <- cvtDerivs derivs
       ; let defn = HsDataDefn { dd_ND = DataType, dd_cType = Nothing
                               , dd_ctxt = ctxt'
                               , dd_kindSig = Nothing
                               , dd_cons = cons', dd_derivs = derivs' }

       ; returnL $ InstD $ DataFamInstD
           { dfid_inst = DataFamInstDecl { dfid_tycon = tc', dfid_pats = typats'
                                         , dfid_defn = defn, dfid_fvs = placeHolderNames } }}

cvtDec (NewtypeInstD ctxt tc tys constr derivs)
  = do { (ctxt', tc', typats') <- cvt_tyinst_hdr ctxt tc tys
       ; con' <- cvtConstr constr
       ; derivs' <- cvtDerivs derivs
       ; let defn = HsDataDefn { dd_ND = NewType, dd_cType = Nothing
                               , dd_ctxt = ctxt'
                               , dd_kindSig = Nothing
                               , dd_cons = [con'], dd_derivs = derivs' }
       ; returnL $ InstD $ DataFamInstD
           { dfid_inst = DataFamInstDecl { dfid_tycon = tc', dfid_pats = typats'
                                         , dfid_defn = defn, dfid_fvs = placeHolderNames } }}

cvtDec (TySynInstD tc eqn)
  = do  { tc' <- tconNameL tc
        ; eqn' <- cvtTySynEqn tc' eqn
        ; returnL $ InstD $ TyFamInstD
            { tfid_inst = TyFamInstDecl { tfid_eqn = eqn'
                                        , tfid_fvs = placeHolderNames } } }

cvtDec (ClosedTypeFamilyD tc tyvars mkind eqns)
  | not $ null eqns
  = do { (_, tc', tvs') <- cvt_tycl_hdr [] tc tyvars
       ; mkind' <- cvtMaybeKind mkind
       ; eqns' <- mapM (cvtTySynEqn tc') eqns
       ; returnL $ TyClD (FamDecl (FamilyDecl (ClosedTypeFamily eqns') tc' tvs' mkind')) }
  | otherwise
  = failWith (ptext (sLit "Illegal empty closed type family"))

cvtDec (TH.RoleAnnotD tc roles)
  = do { tc' <- tconNameL tc
       ; let roles' = map (noLoc . cvtRole) roles
       ; return $ noLoc $ Hs.RoleAnnotD (RoleAnnotDecl tc' roles') }
----------------
cvtTySynEqn :: Located RdrName -> TySynEqn -> CvtM (LTyFamInstEqn RdrName)
cvtTySynEqn tc (TySynEqn lhs rhs)
  = do  { lhs' <- mapM cvtType lhs
        ; rhs' <- cvtType rhs
        ; returnL $ TyFamInstEqn { tfie_tycon = tc
                                 , tfie_pats = mkHsWithBndrs lhs'
                                 , tfie_rhs = rhs' } }

----------------
cvt_ci_decs :: MsgDoc -> [TH.Dec]
            -> CvtM (LHsBinds RdrName,
                     [LSig RdrName],
                     [LFamilyDecl RdrName],
                     [LTyFamInstDecl RdrName],
                     [LDataFamInstDecl RdrName])
-- Convert the declarations inside a class or instance decl
-- ie signatures, bindings, and associated types
cvt_ci_decs doc decs
  = do  { decs' <- mapM cvtDec decs
        ; let (ats', bind_sig_decs') = partitionWith is_tyfam_inst decs'
        ; let (adts', no_ats')       = partitionWith is_datafam_inst bind_sig_decs'
        ; let (sigs', prob_binds')   = partitionWith is_sig no_ats'
        ; let (binds', prob_fams')   = partitionWith is_bind prob_binds'
        ; let (fams', bads)          = partitionWith is_fam_decl prob_fams'
        ; unless (null bads) (failWith (mkBadDecMsg doc bads))
        ; return (listToBag binds', sigs', fams', ats', adts') }

----------------
cvt_tycl_hdr :: TH.Cxt -> TH.Name -> [TH.TyVarBndr]
             -> CvtM ( LHsContext RdrName
                     , Located RdrName
                     , LHsTyVarBndrs RdrName)
cvt_tycl_hdr cxt tc tvs
  = do { cxt' <- cvtContext cxt
       ; tc'  <- tconNameL tc
       ; tvs' <- cvtTvs tvs
       ; return (cxt', tc', tvs')
       }

cvt_tyinst_hdr :: TH.Cxt -> TH.Name -> [TH.Type]
               -> CvtM ( LHsContext RdrName
                       , Located RdrName
                       , HsWithBndrs [LHsType RdrName])
cvt_tyinst_hdr cxt tc tys
  = do { cxt' <- cvtContext cxt
       ; tc'  <- tconNameL tc
       ; tys' <- mapM cvtType tys
       ; return (cxt', tc', mkHsWithBndrs tys') }

-------------------------------------------------------------------
--              Partitioning declarations
-------------------------------------------------------------------

is_fam_decl :: LHsDecl RdrName -> Either (LFamilyDecl RdrName) (LHsDecl RdrName)
is_fam_decl (L loc (TyClD (FamDecl { tcdFam = d }))) = Left (L loc d)
is_fam_decl decl = Right decl

is_tyfam_inst :: LHsDecl RdrName -> Either (LTyFamInstDecl RdrName) (LHsDecl RdrName)
is_tyfam_inst (L loc (Hs.InstD (TyFamInstD { tfid_inst = d }))) = Left (L loc d)
is_tyfam_inst decl                                              = Right decl

is_datafam_inst :: LHsDecl RdrName -> Either (LDataFamInstDecl RdrName) (LHsDecl RdrName)
is_datafam_inst (L loc (Hs.InstD (DataFamInstD { dfid_inst = d }))) = Left (L loc d)
is_datafam_inst decl                                                = Right decl

is_sig :: LHsDecl RdrName -> Either (LSig RdrName) (LHsDecl RdrName)
is_sig (L loc (Hs.SigD sig)) = Left (L loc sig)
is_sig decl                  = Right decl

is_bind :: LHsDecl RdrName -> Either (LHsBind RdrName) (LHsDecl RdrName)
is_bind (L loc (Hs.ValD bind)) = Left (L loc bind)
is_bind decl                   = Right decl

mkBadDecMsg :: Outputable a => MsgDoc -> [a] -> MsgDoc
mkBadDecMsg doc bads
  = sep [ ptext (sLit "Illegal declaration(s) in") <+> doc <> colon
        , nest 2 (vcat (map Outputable.ppr bads)) ]

---------------------------------------------------
--      Data types
-- Can't handle GADTs yet
---------------------------------------------------

cvtConstr :: TH.Con -> CvtM (LConDecl RdrName)

cvtConstr (NormalC c strtys)
  = do  { c'   <- cNameL c
        ; cxt' <- returnL []
        ; tys' <- mapM cvt_arg strtys
        ; returnL $ mkSimpleConDecl c' noExistentials cxt' (PrefixCon tys') }

cvtConstr (RecC c varstrtys)
  = do  { c'    <- cNameL c
        ; cxt'  <- returnL []
        ; args' <- mapM cvt_id_arg varstrtys
        ; returnL $ mkSimpleConDecl c' noExistentials cxt' (RecCon args') }

cvtConstr (InfixC st1 c st2)
  = do  { c' <- cNameL c
        ; cxt' <- returnL []
        ; st1' <- cvt_arg st1
        ; st2' <- cvt_arg st2
        ; returnL $ mkSimpleConDecl c' noExistentials cxt' (InfixCon st1' st2') }

cvtConstr (ForallC tvs ctxt con)
  = do  { tvs'  <- cvtTvs tvs
        ; L loc ctxt' <- cvtContext ctxt
        ; L _ con' <- cvtConstr con
        ; returnL $ con' { con_qvars = mkHsQTvs (hsQTvBndrs tvs' ++ hsQTvBndrs (con_qvars con'))
                         , con_cxt = L loc (ctxt' ++ (unLoc $ con_cxt con')) } }

cvt_arg :: (TH.Strict, TH.Type) -> CvtM (LHsType RdrName)
cvt_arg (NotStrict, ty) = cvtType ty
cvt_arg (IsStrict,  ty) = do { ty' <- cvtType ty; returnL $ HsBangTy (HsUserBang Nothing     True) ty' }
cvt_arg (Unpacked,  ty) = do { ty' <- cvtType ty; returnL $ HsBangTy (HsUserBang (Just True) True) ty' }

cvt_id_arg :: (TH.Name, TH.Strict, TH.Type) -> CvtM (ConDeclField RdrName)
cvt_id_arg (i, str, ty)
  = do  { i' <- vNameL i
        ; ty' <- cvt_arg (str,ty)
        ; return (ConDeclField { cd_fld_name = i', cd_fld_type =  ty', cd_fld_doc = Nothing}) }

cvtDerivs :: [TH.Name] -> CvtM (Maybe [LHsType RdrName])
cvtDerivs [] = return Nothing
cvtDerivs cs = do { cs' <- mapM cvt_one cs
                  ; return (Just cs') }
        where
          cvt_one c = do { c' <- tconName c
                         ; returnL $ HsTyVar c' }

cvt_fundep :: FunDep -> CvtM (Located (Class.FunDep RdrName))
cvt_fundep (FunDep xs ys) = do { xs' <- mapM tName xs; ys' <- mapM tName ys; returnL (xs', ys') }

noExistentials :: [LHsTyVarBndr RdrName]
noExistentials = []

------------------------------------------
--      Foreign declarations
------------------------------------------

cvtForD :: Foreign -> CvtM (ForeignDecl RdrName)
cvtForD (ImportF callconv safety from nm ty)
  | Just impspec <- parseCImport (cvt_conv callconv) safety'
                                 (mkFastString (TH.nameBase nm)) from
  = do { nm' <- vNameL nm
       ; ty' <- cvtType ty
       ; return (ForeignImport nm' ty' noForeignImportCoercionYet impspec)
       }
  | otherwise
  = failWith $ text (show from) <+> ptext (sLit "is not a valid ccall impent")
  where
    safety' = case safety of
                     Unsafe     -> PlayRisky
                     Safe       -> PlaySafe
                     Interruptible -> PlayInterruptible

cvtForD (ExportF callconv as nm ty)
  = do  { nm' <- vNameL nm
        ; ty' <- cvtType ty
        ; let e = CExport (CExportStatic (mkFastString as) (cvt_conv callconv))
        ; return $ ForeignExport nm' ty' noForeignExportCoercionYet e }

cvt_conv :: TH.Callconv -> CCallConv
cvt_conv TH.CCall   = CCallConv
cvt_conv TH.StdCall = StdCallConv

------------------------------------------
--              Pragmas
------------------------------------------

cvtPragmaD :: Pragma -> CvtM (LHsDecl RdrName)
cvtPragmaD (InlineP nm inline rm phases)
  = do { nm' <- vNameL nm
       ; let dflt = dfltActivation inline
       ; let ip   = InlinePragma { inl_inline = cvtInline inline
                                 , inl_rule   = cvtRuleMatch rm
                                 , inl_act    = cvtPhases phases dflt
                                 , inl_sat    = Nothing }
       ; returnL $ Hs.SigD $ InlineSig nm' ip }

cvtPragmaD (SpecialiseP nm ty inline phases)
  = do { nm' <- vNameL nm
       ; ty' <- cvtType ty
       ; let (inline', dflt) = case inline of
               Just inline1 -> (cvtInline inline1, dfltActivation inline1)
               Nothing      -> (EmptyInlineSpec,   AlwaysActive)
       ; let ip = InlinePragma { inl_inline = inline'
                               , inl_rule   = Hs.FunLike
                               , inl_act    = cvtPhases phases dflt
                               , inl_sat    = Nothing }
       ; returnL $ Hs.SigD $ SpecSig nm' ty' ip }

cvtPragmaD (SpecialiseInstP ty)
  = do { ty' <- cvtType ty
       ; returnL $ Hs.SigD $ SpecInstSig ty' }

cvtPragmaD (RuleP nm bndrs lhs rhs phases)
  = do { let nm' = mkFastString nm
       ; let act = cvtPhases phases AlwaysActive
       ; bndrs' <- mapM cvtRuleBndr bndrs
       ; lhs'   <- cvtl lhs
       ; rhs'   <- cvtl rhs
       ; returnL $ Hs.RuleD $ HsRule nm' act bndrs'
                                     lhs' placeHolderNames
                                     rhs' placeHolderNames
       }

cvtPragmaD (AnnP target exp)
  = do { exp' <- cvtl exp
       ; target' <- case target of
         ModuleAnnotation  -> return ModuleAnnProvenance
         TypeAnnotation n  -> do
           n' <- tconName n
           return (TypeAnnProvenance  n')
         ValueAnnotation n -> do
           n' <- if isVarName n then vName n else cName n
           return (ValueAnnProvenance n')
       ; returnL $ Hs.AnnD $ HsAnnotation target' exp'
       }

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
cvtPhases (FromPhase i)   _    = ActiveAfter i
cvtPhases (BeforePhase i) _    = ActiveBefore i

cvtRuleBndr :: TH.RuleBndr -> CvtM (Hs.RuleBndr RdrName)
cvtRuleBndr (RuleVar n)
  = do { n' <- vNameL n
       ; return $ Hs.RuleBndr n' }
cvtRuleBndr (TypedRuleVar n ty)
  = do { n'  <- vNameL n
       ; ty' <- cvtType ty
       ; return $ Hs.RuleBndrSig n' $ mkHsWithBndrs ty' }

---------------------------------------------------
--              Declarations
---------------------------------------------------

cvtLocalDecs :: MsgDoc -> [TH.Dec] -> CvtM (HsLocalBinds RdrName)
cvtLocalDecs doc ds
  | null ds
  = return EmptyLocalBinds
  | otherwise
  = do { ds' <- mapM cvtDec ds
       ; let (binds, prob_sigs) = partitionWith is_bind ds'
       ; let (sigs, bads) = partitionWith is_sig prob_sigs
       ; unless (null bads) (failWith (mkBadDecMsg doc bads))
       ; return (HsValBinds (ValBindsIn (listToBag binds) sigs)) }

cvtClause :: TH.Clause -> CvtM (Hs.LMatch RdrName (LHsExpr RdrName))
cvtClause (Clause ps body wheres)
  = do  { ps' <- cvtPats ps
        ; g'  <- cvtGuard body
        ; ds' <- cvtLocalDecs (ptext (sLit "a where clause")) wheres
        ; returnL $ Hs.Match ps' Nothing (GRHSs g' ds') }


-------------------------------------------------------------------
--              Expressions
-------------------------------------------------------------------

cvtl :: TH.Exp -> CvtM (LHsExpr RdrName)
cvtl e = wrapL (cvt e)
  where
    cvt (VarE s)        = do { s' <- vName s; return $ HsVar s' }
    cvt (ConE s)        = do { s' <- cName s; return $ HsVar s' }
    cvt (LitE l)
      | overloadedLit l = do { l' <- cvtOverLit l; return $ HsOverLit l' }
      | otherwise       = do { l' <- cvtLit l;     return $ HsLit l' }

    cvt (AppE x y)     = do { x' <- cvtl x; y' <- cvtl y; return $ HsApp x' y' }
    cvt (LamE ps e)    = do { ps' <- cvtPats ps; e' <- cvtl e
                            ; return $ HsLam (mkMatchGroup [mkSimpleMatch ps' e']) }
    cvt (LamCaseE ms)  = do { ms' <- mapM cvtMatch ms
                            ; return $ HsLamCase placeHolderType
                                                 (mkMatchGroup ms')
                            }
    cvt (TupE [e])     = do { e' <- cvtl e; return $ HsPar e' }
                                 -- Note [Dropping constructors]
                                 -- Singleton tuples treated like nothing (just parens)
    cvt (TupE es)      = do { es' <- mapM cvtl es; return $ ExplicitTuple (map Present es') Boxed }
    cvt (UnboxedTupE es)      = do { es' <- mapM cvtl es; return $ ExplicitTuple (map Present es') Unboxed }
    cvt (CondE x y z)  = do { x' <- cvtl x; y' <- cvtl y; z' <- cvtl z;
                            ; return $ HsIf (Just noSyntaxExpr) x' y' z' }
    cvt (MultiIfE alts)
      | null alts      = failWith (ptext (sLit "Multi-way if-expression with no alternatives"))
      | otherwise      = do { alts' <- mapM cvtpair alts
                            ; return $ HsMultiIf placeHolderType alts' }
    cvt (LetE ds e)    = do { ds' <- cvtLocalDecs (ptext (sLit "a let expression")) ds
                            ; e' <- cvtl e; return $ HsLet ds' e' }
    cvt (CaseE e ms)   = do { e' <- cvtl e; ms' <- mapM cvtMatch ms
                            ; return $ HsCase e' (mkMatchGroup ms') }
    cvt (DoE ss)       = cvtHsDo DoExpr ss
    cvt (CompE ss)     = cvtHsDo ListComp ss
    cvt (ArithSeqE dd) = do { dd' <- cvtDD dd; return $ ArithSeq noPostTcExpr Nothing dd' }
    cvt (ListE xs)
      | Just s <- allCharLs xs       = do { l' <- cvtLit (StringL s); return (HsLit l') }
             -- Note [Converting strings]
      | otherwise                    = do { xs' <- mapM cvtl xs; return $ ExplicitList void Nothing xs' }

    -- Infix expressions
    cvt (InfixE (Just x) s (Just y)) = do { x' <- cvtl x; s' <- cvtl s; y' <- cvtl y
                                          ; wrapParL HsPar $
                                            OpApp (mkLHsPar x') s' undefined (mkLHsPar y') }
                                            -- Parenthesise both arguments and result,
                                            -- to ensure this operator application does
                                            -- does not get re-associated
                            -- See Note [Operator association]
    cvt (InfixE Nothing  s (Just y)) = do { s' <- cvtl s; y' <- cvtl y
                                          ; wrapParL HsPar $ SectionR s' y' }
                                            -- See Note [Sections in HsSyn] in HsExpr
    cvt (InfixE (Just x) s Nothing ) = do { x' <- cvtl x; s' <- cvtl s
                                          ; wrapParL HsPar $ SectionL x' s' }

    cvt (InfixE Nothing  s Nothing ) = do { s' <- cvtl s; return $ HsPar s' }
                                       -- Can I indicate this is an infix thing?
                                       -- Note [Dropping constructors]

    cvt (UInfixE x s y)  = do { x' <- cvtl x
                              ; let x'' = case x' of
                                            L _ (OpApp {}) -> x'
                                            _ -> mkLHsPar x'
                              ; cvtOpApp x'' s y } --  Note [Converting UInfix]

    cvt (ParensE e)      = do { e' <- cvtl e; return $ HsPar e' }
    cvt (SigE e t)       = do { e' <- cvtl e; t' <- cvtType t
                              ; return $ ExprWithTySig e' t' }
    cvt (RecConE c flds) = do { c' <- cNameL c
                              ; flds' <- mapM cvtFld flds
                              ; return $ RecordCon c' noPostTcExpr (HsRecFields flds' Nothing)}
    cvt (RecUpdE e flds) = do { e' <- cvtl e
                              ; flds' <- mapM cvtFld flds
                              ; return $ RecordUpd e' (HsRecFields flds' Nothing) [] [] [] }

{- Note [Dropping constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we drop constructors from the input (for instance, when we encounter @TupE [e]@)
we must insert parentheses around the argument. Otherwise, @UInfix@ constructors in @e@
could meet @UInfix@ constructors containing the @TupE [e]@. For example:

  UInfixE x * (TupE [UInfixE y + z])

If we drop the singleton tuple but don't insert parentheses, the @UInfixE@s would meet
and the above expression would be reassociated to

  OpApp (OpApp x * y) + z

which we don't want.
-}

cvtFld :: (TH.Name, TH.Exp) -> CvtM (HsRecField RdrName (LHsExpr RdrName))
cvtFld (v,e)
  = do  { v' <- vNameL v; e' <- cvtl e
        ; return (HsRecField { hsRecFieldId = v', hsRecFieldArg = e', hsRecPun = False}) }

cvtDD :: Range -> CvtM (ArithSeqInfo RdrName)
cvtDD (FromR x)           = do { x' <- cvtl x; return $ From x' }
cvtDD (FromThenR x y)     = do { x' <- cvtl x; y' <- cvtl y; return $ FromThen x' y' }
cvtDD (FromToR x y)       = do { x' <- cvtl x; y' <- cvtl y; return $ FromTo x' y' }
cvtDD (FromThenToR x y z) = do { x' <- cvtl x; y' <- cvtl y; z' <- cvtl z; return $ FromThenTo x' y' z' }

{- Note [Operator assocation]
We must be quite careful about adding parens:
  * Infix (UInfix ...) op arg      Needs parens round the first arg
  * Infix (Infix ...) op arg       Needs parens round the first arg
  * UInfix (UInfix ...) op arg     No parens for first arg
  * UInfix (Infix ...) op arg      Needs parens round first arg


Note [Converting UInfix]
~~~~~~~~~~~~~~~~~~~~~~~~
When converting @UInfixE@ and @UInfixP@ values, we want to readjust
the trees to reflect the fixities of the underlying operators:

  UInfixE x * (UInfixE y + z) ---> (x * y) + z

This is done by the renamer (see @mkOppAppRn@ and @mkConOppPatRn@ in
RnTypes), which expects that the input will be completely left-biased.
So we left-bias the trees  of @UInfixP@ and @UInfixE@ that we come across.

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

The functions @cvtOpApp@ and @cvtOpAppP@ are responsible for this
left-biasing.
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
cvtOpApp :: LHsExpr RdrName -> TH.Exp -> TH.Exp -> CvtM (HsExpr RdrName)
cvtOpApp x op1 (UInfixE y op2 z)
  = do { l <- wrapL $ cvtOpApp x op1 y
       ; cvtOpApp l op2 z }
cvtOpApp x op y
  = do { op' <- cvtl op
       ; y' <- cvtl y
       ; return (OpApp x op' undefined y') }

-------------------------------------
--      Do notation and statements
-------------------------------------

cvtHsDo :: HsStmtContext Name.Name -> [TH.Stmt] -> CvtM (HsExpr RdrName)
cvtHsDo do_or_lc stmts
  | null stmts = failWith (ptext (sLit "Empty stmt list in do-block"))
  | otherwise
  = do  { stmts' <- cvtStmts stmts
        ; let Just (stmts'', last') = snocView stmts'

        ; last'' <- case last' of
                    L loc (BodyStmt body _ _ _) -> return (L loc (mkLastStmt body))
                    _ -> failWith (bad_last last')

        ; return $ HsDo do_or_lc (stmts'' ++ [last'']) void }
  where
    bad_last stmt = vcat [ ptext (sLit "Illegal last statement of") <+> pprAStmtContext do_or_lc <> colon
                         , nest 2 $ Outputable.ppr stmt
                         , ptext (sLit "(It should be an expression.)") ]

cvtStmts :: [TH.Stmt] -> CvtM [Hs.LStmt RdrName (LHsExpr RdrName)]
cvtStmts = mapM cvtStmt

cvtStmt :: TH.Stmt -> CvtM (Hs.LStmt RdrName (LHsExpr RdrName))
cvtStmt (NoBindS e)    = do { e' <- cvtl e; returnL $ mkBodyStmt e' }
cvtStmt (TH.BindS p e) = do { p' <- cvtPat p; e' <- cvtl e; returnL $ mkBindStmt p' e' }
cvtStmt (TH.LetS ds)   = do { ds' <- cvtLocalDecs (ptext (sLit "a let binding")) ds
                            ; returnL $ LetStmt ds' }
cvtStmt (TH.ParS dss)  = do { dss' <- mapM cvt_one dss; returnL $ ParStmt dss' noSyntaxExpr noSyntaxExpr }
                       where
                         cvt_one ds = do { ds' <- cvtStmts ds; return (ParStmtBlock ds' undefined noSyntaxExpr) }

cvtMatch :: TH.Match -> CvtM (Hs.LMatch RdrName (LHsExpr RdrName))
cvtMatch (TH.Match p body decs)
  = do  { p' <- cvtPat p
        ; g' <- cvtGuard body
        ; decs' <- cvtLocalDecs (ptext (sLit "a where clause")) decs
        ; returnL $ Hs.Match [p'] Nothing (GRHSs g' decs') }

cvtGuard :: TH.Body -> CvtM [LGRHS RdrName (LHsExpr RdrName)]
cvtGuard (GuardedB pairs) = mapM cvtpair pairs
cvtGuard (NormalB e)      = do { e' <- cvtl e; g' <- returnL $ GRHS [] e'; return [g'] }

cvtpair :: (TH.Guard, TH.Exp) -> CvtM (LGRHS RdrName (LHsExpr RdrName))
cvtpair (NormalG ge,rhs) = do { ge' <- cvtl ge; rhs' <- cvtl rhs
                              ; g' <- returnL $ mkBodyStmt ge'
                              ; returnL $ GRHS [g'] rhs' }
cvtpair (PatG gs,rhs)    = do { gs' <- cvtStmts gs; rhs' <- cvtl rhs
                              ; returnL $ GRHS gs' rhs' }

cvtOverLit :: Lit -> CvtM (HsOverLit RdrName)
cvtOverLit (IntegerL i)
  = do { force i; return $ mkHsIntegral i placeHolderType}
cvtOverLit (RationalL r)
  = do { force r; return $ mkHsFractional (cvtFractionalLit r) placeHolderType}
cvtOverLit (StringL s)
  = do { let { s' = mkFastString s }
       ; force s'
       ; return $ mkHsIsString s' placeHolderType
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

cvtLit :: Lit -> CvtM HsLit
cvtLit (IntPrimL i)    = do { force i; return $ HsIntPrim i }
cvtLit (WordPrimL w)   = do { force w; return $ HsWordPrim w }
cvtLit (FloatPrimL f)  = do { force f; return $ HsFloatPrim (cvtFractionalLit f) }
cvtLit (DoublePrimL f) = do { force f; return $ HsDoublePrim (cvtFractionalLit f) }
cvtLit (CharL c)       = do { force c; return $ HsChar c }
cvtLit (StringL s)     = do { let { s' = mkFastString s }
                            ; force s'
                            ; return $ HsString s' }
cvtLit (StringPrimL s) = do { let { s' = BS.pack s }
                            ; force s'
                            ; return $ HsStringPrim s' }
cvtLit _ = panic "Convert.cvtLit: Unexpected literal"
        -- cvtLit should not be called on IntegerL, RationalL
        -- That precondition is established right here in
        -- Convert.lhs, hence panic

cvtPats :: [TH.Pat] -> CvtM [Hs.LPat RdrName]
cvtPats pats = mapM cvtPat pats

cvtPat :: TH.Pat -> CvtM (Hs.LPat RdrName)
cvtPat pat = wrapL (cvtp pat)

cvtp :: TH.Pat -> CvtM (Hs.Pat RdrName)
cvtp (TH.LitP l)
  | overloadedLit l    = do { l' <- cvtOverLit l
                            ; return (mkNPat l' Nothing) }
                                  -- Not right for negative patterns;
                                  -- need to think about that!
  | otherwise          = do { l' <- cvtLit l; return $ Hs.LitPat l' }
cvtp (TH.VarP s)       = do { s' <- vName s; return $ Hs.VarPat s' }
cvtp (TupP [p])        = do { p' <- cvtPat p; return $ ParPat p' } -- Note [Dropping constructors]
cvtp (TupP ps)         = do { ps' <- cvtPats ps; return $ TuplePat ps' Boxed void }
cvtp (UnboxedTupP ps)  = do { ps' <- cvtPats ps; return $ TuplePat ps' Unboxed void }
cvtp (ConP s ps)       = do { s' <- cNameL s; ps' <- cvtPats ps
                            ; return $ ConPatIn s' (PrefixCon ps') }
cvtp (InfixP p1 s p2)  = do { s' <- cNameL s; p1' <- cvtPat p1; p2' <- cvtPat p2
                            ; wrapParL ParPat $
                              ConPatIn s' (InfixCon (mkParPat p1') (mkParPat p2')) }
                            -- See Note [Operator association]
cvtp (UInfixP p1 s p2) = do { p1' <- cvtPat p1; cvtOpAppP p1' s p2 } -- Note [Converting UInfix]
cvtp (ParensP p)       = do { p' <- cvtPat p; return $ ParPat p' }
cvtp (TildeP p)        = do { p' <- cvtPat p; return $ LazyPat p' }
cvtp (BangP p)         = do { p' <- cvtPat p; return $ BangPat p' }
cvtp (TH.AsP s p)      = do { s' <- vNameL s; p' <- cvtPat p; return $ AsPat s' p' }
cvtp TH.WildP          = return $ WildPat void
cvtp (RecP c fs)       = do { c' <- cNameL c; fs' <- mapM cvtPatFld fs
                            ; return $ ConPatIn c' $ Hs.RecCon (HsRecFields fs' Nothing) }
cvtp (ListP ps)        = do { ps' <- cvtPats ps; return $ ListPat ps' void Nothing }
cvtp (SigP p t)        = do { p' <- cvtPat p; t' <- cvtType t
                            ; return $ SigPatIn p' (mkHsWithBndrs t') }
cvtp (ViewP e p)       = do { e' <- cvtl e; p' <- cvtPat p; return $ ViewPat e' p' void }

cvtPatFld :: (TH.Name, TH.Pat) -> CvtM (HsRecField RdrName (LPat RdrName))
cvtPatFld (s,p)
  = do  { s' <- vNameL s; p' <- cvtPat p
        ; return (HsRecField { hsRecFieldId = s', hsRecFieldArg = p', hsRecPun = False}) }

{- | @cvtOpAppP x op y@ converts @op@ and @y@ and produces the operator application @x `op` y@.
The produced tree of infix patterns will be left-biased, provided @x@ is.

See the @cvtOpApp@ documentation for how this function works.
-}
cvtOpAppP :: Hs.LPat RdrName -> TH.Name -> TH.Pat -> CvtM (Hs.Pat RdrName)
cvtOpAppP x op1 (UInfixP y op2 z)
  = do { l <- wrapL $ cvtOpAppP x op1 y
       ; cvtOpAppP l op2 z }
cvtOpAppP x op y
  = do { op' <- cNameL op
       ; y' <- cvtPat y
       ; return (ConPatIn op' (InfixCon x y')) }

-----------------------------------------------------------
--      Types and type variables

cvtTvs :: [TH.TyVarBndr] -> CvtM (LHsTyVarBndrs RdrName)
cvtTvs tvs = do { tvs' <- mapM cvt_tv tvs; return (mkHsQTvs tvs') }

cvt_tv :: TH.TyVarBndr -> CvtM (LHsTyVarBndr RdrName)
cvt_tv (TH.PlainTV nm)
  = do { nm' <- tName nm
       ; returnL $ UserTyVar nm' }
cvt_tv (TH.KindedTV nm ki)
  = do { nm' <- tName nm
       ; ki' <- cvtKind ki
       ; returnL $ KindedTyVar nm' ki' }

cvtRole :: TH.Role -> Maybe Coercion.Role
cvtRole TH.NominalR          = Just Coercion.Nominal
cvtRole TH.RepresentationalR = Just Coercion.Representational
cvtRole TH.PhantomR          = Just Coercion.Phantom
cvtRole TH.InferR            = Nothing

cvtContext :: TH.Cxt -> CvtM (LHsContext RdrName)
cvtContext tys = do { preds' <- mapM cvtPred tys; returnL preds' }

cvtPred :: TH.Pred -> CvtM (LHsType RdrName)
cvtPred (TH.ClassP cla tys)
  = do { cla' <- if isVarName cla then tName cla else tconName cla
       ; tys' <- mapM cvtType tys
       ; mk_apps (HsTyVar cla') tys'
       }
cvtPred (TH.EqualP ty1 ty2)
  = do { ty1' <- cvtType ty1
       ; ty2' <- cvtType ty2
       ; returnL $ HsEqTy ty1' ty2'
       }

cvtType :: TH.Type -> CvtM (LHsType RdrName)
cvtType = cvtTypeKind "type"

cvtTypeKind :: String -> TH.Type -> CvtM (LHsType RdrName)
cvtTypeKind ty_str ty
  = do { (head_ty, tys') <- split_ty_app ty
       ; case head_ty of
           TupleT n
             | length tys' == n         -- Saturated
             -> if n==1 then return (head tys') -- Singleton tuples treated
                                                -- like nothing (ie just parens)
                        else returnL (HsTupleTy HsBoxedTuple tys')
             | n == 1
             -> failWith (ptext (sLit ("Illegal 1-tuple " ++ ty_str ++ " constructor")))
             | otherwise
             -> mk_apps (HsTyVar (getRdrName (tupleTyCon BoxedTuple n))) tys'
           UnboxedTupleT n
             | length tys' == n         -- Saturated
             -> if n==1 then return (head tys') -- Singleton tuples treated
                                                -- like nothing (ie just parens)
                        else returnL (HsTupleTy HsUnboxedTuple tys')
             | otherwise
             -> mk_apps (HsTyVar (getRdrName (tupleTyCon UnboxedTuple n))) tys'
           ArrowT
             | [x',y'] <- tys' -> returnL (HsFunTy x' y')
             | otherwise       -> mk_apps (HsTyVar (getRdrName funTyCon)) tys'
           ListT
             | [x']    <- tys' -> returnL (HsListTy x')
             | otherwise       -> mk_apps (HsTyVar (getRdrName listTyCon)) tys'
           VarT nm -> do { nm' <- tName nm;    mk_apps (HsTyVar nm') tys' }
           ConT nm -> do { nm' <- tconName nm; mk_apps (HsTyVar nm') tys' }

           ForallT tvs cxt ty
             | null tys'
             -> do { tvs' <- cvtTvs tvs
                   ; cxt' <- cvtContext cxt
                   ; ty'  <- cvtType ty
                   ; returnL $ mkExplicitHsForAllTy (hsQTvBndrs tvs') cxt' ty'
                   }

           SigT ty ki
             -> do { ty' <- cvtType ty
                   ; ki' <- cvtKind ki
                   ; mk_apps (HsKindSig ty' ki') tys'
                   }

           LitT lit
             -> returnL (HsTyLit (cvtTyLit lit))

           PromotedT nm -> do { nm' <- cName nm; mk_apps (HsTyVar nm') tys' }
                 -- Promoted data constructor; hence cName

           PromotedTupleT n
             | n == 1
             -> failWith (ptext (sLit ("Illegal promoted 1-tuple " ++ ty_str)))
             | m == n   -- Saturated
             -> do  { let kis = replicate m placeHolderKind
                    ; returnL (HsExplicitTupleTy kis tys')
                    }
             where
               m = length tys'

           PromotedNilT
             -> returnL (HsExplicitListTy placeHolderKind [])

           PromotedConsT  -- See Note [Representing concrete syntax in types]
                          -- in Language.Haskell.TH.Syntax
             | [ty1, L _ (HsExplicitListTy _ tys2)] <- tys'
             -> returnL (HsExplicitListTy placeHolderKind (ty1:tys2))
             | otherwise
             -> mk_apps (HsTyVar (getRdrName consDataCon)) tys'

           StarT
             -> returnL (HsTyVar (getRdrName liftedTypeKindTyCon))

           ConstraintT
             -> returnL (HsTyVar (getRdrName constraintKindTyCon))

           _ -> failWith (ptext (sLit ("Malformed " ++ ty_str)) <+> text (show ty))
    }

mk_apps :: HsType RdrName -> [LHsType RdrName] -> CvtM (LHsType RdrName)
mk_apps head_ty []       = returnL head_ty
mk_apps head_ty (ty:tys) = do { head_ty' <- returnL head_ty
                              ; mk_apps (HsAppTy head_ty' ty) tys }

split_ty_app :: TH.Type -> CvtM (TH.Type, [LHsType RdrName])
split_ty_app ty = go ty []
  where
    go (AppT f a) as' = do { a' <- cvtType a; go f (a':as') }
    go f as           = return (f,as)

cvtTyLit :: TH.TyLit -> HsTyLit
cvtTyLit (NumTyLit i) = HsNumTy i
cvtTyLit (StrTyLit s) = HsStrTy (fsLit s)

cvtKind :: TH.Kind -> CvtM (LHsKind RdrName)
cvtKind = cvtTypeKind "kind"

cvtMaybeKind :: Maybe TH.Kind -> CvtM (Maybe (LHsKind RdrName))
cvtMaybeKind Nothing = return Nothing
cvtMaybeKind (Just ki) = do { ki' <- cvtKind ki
                            ; return (Just ki') }

-----------------------------------------------------------
cvtFixity :: TH.Fixity -> Hs.Fixity
cvtFixity (TH.Fixity prec dir) = Hs.Fixity prec (cvt_dir dir)
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

void :: Type.Type
void = placeHolderType

cvtFractionalLit :: Rational -> FractionalLit
cvtFractionalLit r = FL { fl_text = show (fromRational r :: Double), fl_value = r }

--------------------------------------------------------------------
--      Turning Name back into RdrName
--------------------------------------------------------------------

-- variable names
vNameL, cNameL, tconNameL :: TH.Name -> CvtM (Located RdrName)
vName,  cName,  tName,  tconName  :: TH.Name -> CvtM RdrName

vNameL n = wrapL (vName n)
vName n = cvtName OccName.varName n

-- Constructor function names; this is Haskell source, hence srcDataName
cNameL n = wrapL (cName n)
cName n = cvtName OccName.dataName n

-- Type variable names
tName n = cvtName OccName.tvName n

-- Type Constructor names
tconNameL n = wrapL (tconName n)
tconName n = cvtName OccName.tcClsName n

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
okOcc _  []      = False
okOcc ns str@(c:_)
  | OccName.isVarNameSpace ns     = startsVarId c || startsVarSym c
  | OccName.isDataConNameSpace ns = startsConId c || startsConSym c || str == "[]"
  | otherwise                     = startsConId c || startsConSym c ||
                                    startsVarSym c || str == "[]" || str == "->"
                                     -- allow type operators like "+"

-- Determine the name space of a name in a type
--
isVarName :: TH.Name -> Bool
isVarName (TH.Name occ _)
  = case TH.occString occ of
      ""    -> False
      (c:_) -> startsVarId c || startsVarSym c

badOcc :: OccName.NameSpace -> String -> SDoc
badOcc ctxt_ns occ
  = ptext (sLit "Illegal") <+> pprNameSpace ctxt_ns
        <+> ptext (sLit "name:") <+> quotes (text occ)

thRdrName :: SrcSpan -> OccName.NameSpace -> String -> TH.NameFlavour -> RdrName
-- This turns a TH Name into a RdrName; used for both binders and occurrences
-- See Note [Binders in Template Haskell]
-- The passed-in name space tells what the context is expecting;
--      use it unless the TH name knows what name-space it comes
--      from, in which case use the latter
--
-- We pass in a SrcSpan (gotten from the monad) because this function
-- is used for *binders* and if we make an Exact Name we want it
-- to have a binding site inside it.  (cf Trac #5434)
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
     TH.NameL uniq -> nameRdrName $! (((Name.mkInternalName $! mk_uniq uniq) $! occ) loc)
     TH.NameU uniq -> nameRdrName $! (((Name.mkSystemNameAt $! mk_uniq uniq) $! occ) loc)
     TH.NameS | Just name <- isBuiltInOcc ctxt_ns th_occ -> nameRdrName $! name
              | otherwise                                -> mkRdrUnqual $! occ
  where
    occ :: OccName.OccName
    occ = mk_occ ctxt_ns th_occ

thOrigRdrName :: String -> TH.NameSpace -> PkgName -> ModName -> RdrName
thOrigRdrName occ th_ns pkg mod = (mkOrig $! (mkModule (mk_pkg pkg) (mk_mod mod))) $! (mk_occ (mk_ghc_ns th_ns) occ)

thRdrNameGuesses :: TH.Name -> [RdrName]
thRdrNameGuesses (TH.Name occ flavour)
  -- This special case for NameG ensures that we don't generate duplicates in the output list
  | TH.NameG th_ns pkg mod <- flavour = [ thOrigRdrName occ_str th_ns pkg mod]
  | otherwise                         = [ thRdrName noSrcSpan gns occ_str flavour
                                        | gns <- guessed_nss]
  where
    -- guessed_ns are the name spaces guessed from looking at the TH name
    guessed_nss | isLexCon (mkFastString occ_str) = [OccName.tcName,  OccName.dataName]
                | otherwise                       = [OccName.varName, OccName.tvName]
    occ_str = TH.occString occ

isBuiltInOcc :: OccName.NameSpace -> String -> Maybe Name.Name
-- Built in syntax isn't "in scope" so an Unqual RdrName won't do
-- We must generate an Exact name, just as the parser does
isBuiltInOcc ctxt_ns occ
  = case occ of
        ":"              -> Just (Name.getName consDataCon)
        "[]"             -> Just (Name.getName nilDataCon)
        "()"             -> Just (tup_name 0)
        '(' : ',' : rest -> go_tuple 2 rest
        _                -> Nothing
  where
    go_tuple n ")"          = Just (tup_name n)
    go_tuple n (',' : rest) = go_tuple (n+1) rest
    go_tuple _ _            = Nothing

    tup_name n
        | OccName.isTcClsNameSpace ctxt_ns = Name.getName (tupleTyCon BoxedTuple n)
        | otherwise                        = Name.getName (tupleCon BoxedTuple n)

-- The packing and unpacking is rather turgid :-(
mk_occ :: OccName.NameSpace -> String -> OccName.OccName
mk_occ ns occ = OccName.mkOccName ns occ

mk_ghc_ns :: TH.NameSpace -> OccName.NameSpace
mk_ghc_ns TH.DataName  = OccName.dataName
mk_ghc_ns TH.TcClsName = OccName.tcClsName
mk_ghc_ns TH.VarName   = OccName.varName

mk_mod :: TH.ModName -> ModuleName
mk_mod mod = mkModuleName (TH.modString mod)

mk_pkg :: TH.PkgName -> PackageId
mk_pkg pkg = stringToPackageId (TH.pkgString pkg)

mk_uniq :: Int# -> Unique
mk_uniq u = mkUniqueGrimily (I# u)
\end{code}

Note [Binders in Template Haskell]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this TH term construction:
  do { x1 <- TH.newName "x"   -- newName :: String -> Q TH.Name
     ; x2 <- TH.newName "x"   -- Builds a NameU
     ; x3 <- TH.newName "x"

     ; let x = mkName "x"     -- mkName :: String -> TH.Name
                              -- Builds a NameL

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
     See Note [Collect binders only after renaming] in HsUtils

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
RdrNames] in RnEnv.

