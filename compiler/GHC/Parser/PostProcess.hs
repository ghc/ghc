
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- Functions over HsSyn specialised to RdrName.

module GHC.Parser.PostProcess (
        mkRdrGetField, mkRdrProjection, Fbind, -- RecordDot
        mkHsOpApp,
        mkHsIntegral, mkHsFractional, mkHsIsString,
        mkHsDo, mkMDo, mkSpliceDecl,
        mkRoleAnnotDecl,
        mkClassDecl,
        mkTyData, mkDataFamInst,
        mkTySynonym, mkTyFamInstEqn,
        mkStandaloneKindSig,
        mkTyFamInst,
        mkFamDecl,
        mkInlinePragma,
        mkOpaquePragma,
        mkPatSynMatchGroup,
        mkRecConstrOrUpdate,
        mkTyClD, mkInstD,
        mkRdrRecordCon, mkRdrRecordUpd,
        setRdrNameSpace,
        fromSpecTyVarBndr, fromSpecTyVarBndrs,
        annBinds,
        stmtsAnchor, stmtsLoc,

        cvBindGroup,
        cvBindsAndSigs,
        cvTopDecls,
        placeHolderPunRhs,

        -- Stuff to do with Foreign declarations
        mkImport,
        parseCImport,
        mkExport,
        mkExtName,    -- RdrName -> CLabelString
        mkGadtDecl,   -- [LocatedA RdrName] -> LHsType RdrName -> ConDecl RdrName
        mkConDeclH98,

        -- Bunch of functions in the parser monad for
        -- checking and constructing values
        checkImportDecl,
        checkExpBlockArguments, checkCmdBlockArguments,
        checkPrecP,           -- Int -> P Int
        checkContext,         -- HsType -> P HsContext
        checkPattern,         -- HsExp -> P HsPat
        checkPattern_details,
        incompleteDoBlock,
        ParseContext(..),
        checkMonadComp,
        checkValDef,          -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
        checkValSigLhs,
        LRuleTyTmVar, RuleTyTmVar(..),
        mkRuleBndrs, mkRuleTyVarBndrs,
        checkRuleTyVarBndrNames,
        checkRecordSyntax,
        checkEmptyGADTs,
        addFatalError, hintBangPat,
        mkBangTy,
        UnpackednessPragma(..),
        mkMultAnn,

        -- Token location
        mkTokenLocation,

        -- Help with processing exports
        ImpExpSubSpec(..),
        ImpExpQcSpec(..),
        mkModuleImpExp,
        mkTypeImpExp,
        mkImpExpSubSpec,
        checkImportSpec,

        -- Token symbols
        starSym,

        -- Warnings and errors
        warnStarIsType,
        warnPrepositiveQualifiedModule,
        failOpFewArgs,
        failNotEnabledImportQualifiedPost,
        failImportQualifiedTwice,

        SumOrTuple (..),

        -- Expression/command/pattern ambiguity resolution
        PV,
        runPV,
        ECP(ECP, unECP),
        DisambInfixOp(..),
        DisambECP(..),
        ecpFromExp,
        ecpFromCmd,
        ecpFromPat,
        ArrowParsingMode(..),
        withArrowParsingMode, withArrowParsingMode',
        setTelescopeBndrsNameSpace,
        PatBuilder,
        hsHoleExpr,

        -- Type/datacon ambiguity resolution
        DisambTD(..),
        addUnpackednessP,
        dataConBuilderCon,
        dataConBuilderDetails,
        mkUnboxedSumCon,

        -- ListTuplePuns related parsers
        mkTupleSyntaxTy,
        mkTupleSyntaxTycon,
        mkListSyntaxTy0,
        mkListSyntaxTy1,
        withCombinedComments,
        requireLTPuns,
    ) where

import GHC.Prelude
import GHC.Hs           -- Lots of it
import GHC.Core.TyCon          ( TyCon, isTupleTyCon, tyConSingleDataCon_maybe )
import GHC.Core.DataCon        ( DataCon, dataConTyCon, dataConName )
import GHC.Core.ConLike        ( ConLike(..) )
import GHC.Core.Coercion.Axiom ( Role, fsFromRole )
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Types.Basic
import GHC.Types.Error
import GHC.Types.Fixity
import GHC.Types.Hint
import GHC.Types.SourceText
import GHC.Parser.Types
import GHC.Parser.Lexer
import GHC.Parser.Errors.Types
import GHC.Utils.Lexeme ( okConOcc )
import GHC.Types.TyThing
import GHC.Core.Type    ( Specificity(..) )
import GHC.Builtin.Types( cTupleTyConName, tupleTyCon, tupleDataCon,
                          nilDataConName, nilDataConKey,
                          listTyConName, listTyConKey, sumDataCon,
                          unrestrictedFunTyCon , listTyCon_RDR, unitDataCon )
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc
import GHC.Types.Unique ( hasKey )
import GHC.Data.OrdList
import GHC.Utils.Outputable as Outputable
import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Utils.Error
import GHC.Utils.Misc
import GHC.Utils.Monad (unlessM)
import Data.Either
import Data.List        ( findIndex )
import Data.Foldable
import qualified Data.Semigroup as Semi
import GHC.Unit.Module.Warnings
import GHC.Utils.Panic
import qualified GHC.Data.Strict as Strict

import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Control.Monad
import Text.ParserCombinators.ReadP as ReadP
import Data.Char
import Data.Data       ( dataTypeOf, fromConstr, dataTypeConstrs )
import Data.Kind       ( Type )
import Data.List.NonEmpty (NonEmpty)

{- **********************************************************************

  Construction functions for Rdr stuff

  ********************************************************************* -}

-- | mkClassDecl builds a RdrClassDecl, filling in the names for tycon and
-- datacon by deriving them from the name of the class.  We fill in the names
-- for the tycon and datacon corresponding to the class, by deriving them
-- from the name of the class itself.  This saves recording the names in the
-- interface file (which would be equally good).

-- Similarly for mkConDecl, mkClassOpSig and default-method names.

--         *** See Note [The Naming story] in GHC.Hs.Decls ****

mkTyClD :: LTyClDecl (GhcPass p) -> LHsDecl (GhcPass p)
mkTyClD (L loc d) = L loc (TyClD noExtField d)

mkInstD :: LInstDecl (GhcPass p) -> LHsDecl (GhcPass p)
mkInstD (L loc d) = L loc (InstD noExtField d)

mkClassDecl :: SrcSpan
            -> Located (Maybe (LHsContext GhcPs), LHsType GhcPs)
            -> Located (a,[LHsFunDep GhcPs])
            -> OrdList (LHsDecl GhcPs)
            -> EpLayout
            -> AnnClassDecl
            -> P (LTyClDecl GhcPs)

mkClassDecl loc' (L _ (mcxt, tycl_hdr)) fds where_cls layout annsIn
  = do { (binds, sigs, ats, at_defs, _, docs) <- cvBindsAndSigs where_cls
       ; (cls, tparams, fixity, ops, cps, cs) <- checkTyClHdr True tycl_hdr
       ; tyvars <- checkTyVars (text "class") whereDots cls tparams
       ; let anns' = annsIn { acd_openp = ops, acd_closep = cps}
       ; let loc = EpAnn (spanAsAnchor loc') noAnn cs
       ; return (L loc (ClassDecl { tcdCExt = (anns', layout, NoAnnSortKey)
                                  , tcdCtxt = mcxt
                                  , tcdLName = cls, tcdTyVars = tyvars
                                  , tcdFixity = fixity
                                  , tcdFDs = snd (unLoc fds)
                                  , tcdSigs = mkClassOpSigs sigs
                                  , tcdMeths = binds
                                  , tcdATs = ats, tcdATDefs = at_defs
                                  , tcdDocs  = docs })) }

mkTyData :: SrcSpan
         -> Bool
         -> NewOrData
         -> Maybe (LocatedP CType)
         -> Located (Maybe (LHsContext GhcPs), LHsType GhcPs)
         -> Maybe (LHsKind GhcPs)
         -> [LConDecl GhcPs]
         -> Located (HsDeriving GhcPs)
         -> AnnDataDefn
         -> P (LTyClDecl GhcPs)
mkTyData loc' is_type_data new_or_data cType (L _ (mcxt, tycl_hdr))
         ksig data_cons (L _ maybe_deriv) annsIn
  = do { (tc, tparams, fixity, ops, cps, cs) <- checkTyClHdr False tycl_hdr
       ; tyvars <- checkTyVars (ppr new_or_data) equalsDots tc tparams
       ; let anns = annsIn {andd_openp = ops, andd_closep = cps}
       ; data_cons <- checkNewOrData loc' (unLoc tc) is_type_data new_or_data data_cons
       ; defn <- mkDataDefn cType mcxt ksig data_cons maybe_deriv anns
       ; !cs' <- getCommentsFor loc'
       ; let loc = EpAnn (spanAsAnchor loc') noAnn (cs' Semi.<> cs)
       ; return (L loc (DataDecl { tcdDExt = noExtField,
                                   tcdLName = tc, tcdTyVars = tyvars,
                                   tcdFixity = fixity,
                                   tcdDataDefn = defn })) }

mkDataDefn :: Maybe (LocatedP CType)
           -> Maybe (LHsContext GhcPs)
           -> Maybe (LHsKind GhcPs)
           -> DataDefnCons (LConDecl GhcPs)
           -> HsDeriving GhcPs
           -> AnnDataDefn
           -> P (HsDataDefn GhcPs)
mkDataDefn cType mcxt ksig data_cons maybe_deriv anns
  = do { checkDatatypeContext mcxt
       ; return (HsDataDefn { dd_ext = anns
                            , dd_cType = cType
                            , dd_ctxt = mcxt
                            , dd_cons = data_cons
                            , dd_kindSig = ksig
                            , dd_derivs = maybe_deriv }) }

mkTySynonym :: SrcSpan
            -> LHsType GhcPs  -- LHS
            -> LHsType GhcPs  -- RHS
            -> EpToken "type"
            -> EpToken "="
            -> P (LTyClDecl GhcPs)
mkTySynonym loc lhs rhs antype aneq
  = do { (tc, tparams, fixity, ops, cps, cs) <- checkTyClHdr False lhs
       ; tyvars <- checkTyVars (text "type") equalsDots tc tparams
       ; let anns = AnnSynDecl ops cps antype aneq
       ; let loc' = EpAnn (spanAsAnchor loc) noAnn cs
       ; return (L loc' (SynDecl { tcdSExt = anns
                                 , tcdLName = tc, tcdTyVars = tyvars
                                 , tcdFixity = fixity
                                 , tcdRhs = rhs })) }

mkStandaloneKindSig
  :: SrcSpan
  -> Located [LocatedN RdrName]   -- LHS
  -> LHsSigType GhcPs             -- RHS
  -> (EpToken "type", TokDcolon)
  -> P (LStandaloneKindSig GhcPs)
mkStandaloneKindSig loc lhs rhs anns =
  do { vs <- mapM check_lhs_name (unLoc lhs)
     ; v <- check_singular_lhs (reverse vs)
     ; return $ L (noAnnSrcSpan loc)
       $ StandaloneKindSig anns v rhs }
  where
    check_lhs_name v@(unLoc->name) =
      if isUnqual name && isTcOcc (rdrNameOcc name)
      then return v
      else addFatalError $ mkPlainErrorMsgEnvelope (getLocA v) $
             (PsErrUnexpectedQualifiedConstructor (unLoc v))
    check_singular_lhs vs =
      case vs of
        [] -> panic "mkStandaloneKindSig: empty left-hand side"
        [v] -> return v
        _ -> addFatalError $ mkPlainErrorMsgEnvelope (getLoc lhs) $
               (PsErrMultipleNamesInStandaloneKindSignature vs)

mkTyFamInstEqn :: SrcSpan
               -> HsOuterFamEqnTyVarBndrs GhcPs
               -> LHsType GhcPs
               -> LHsType GhcPs
               -> EpToken "="
               -> P (LTyFamInstEqn GhcPs)
mkTyFamInstEqn loc bndrs lhs rhs annEq
  = do { (tc, tparams, fixity, ops, cps, cs) <- checkTyClHdr False lhs
       ; let loc' = EpAnn (spanAsAnchor loc) noAnn cs
       ; return (L loc' $ FamEqn
                        { feqn_ext    = (ops, cps, annEq)
                        , feqn_tycon  = tc
                        , feqn_bndrs  = bndrs
                        , feqn_pats   = tparams
                        , feqn_fixity = fixity
                        , feqn_rhs    = rhs })}

mkDataFamInst :: SrcSpan
              -> NewOrData
              -> Maybe (LocatedP CType)
              -> (Maybe ( LHsContext GhcPs), HsOuterFamEqnTyVarBndrs GhcPs
                        , LHsType GhcPs)
              -> Maybe (LHsKind GhcPs)
              -> [LConDecl GhcPs]
              -> Located (HsDeriving GhcPs)
              -> AnnDataDefn
              -> P (LInstDecl GhcPs)
mkDataFamInst loc new_or_data cType (mcxt, bndrs, tycl_hdr)
              ksig data_cons (L _ maybe_deriv) anns
  = do { (tc, tparams, fixity, ops, cps, cs) <- checkTyClHdr False tycl_hdr
       ; data_cons <- checkNewOrData loc (unLoc tc) False new_or_data data_cons
       ; let anns' = anns {andd_openp = ops, andd_closep = cps}
       ; defn <- mkDataDefn cType mcxt ksig data_cons maybe_deriv anns'
       ; let loc' = EpAnn (spanAsAnchor loc) noAnn cs
       ; return (L loc' (DataFamInstD noExtField (DataFamInstDecl
                  (FamEqn { feqn_ext    = ([], [], NoEpTok)
                          , feqn_tycon  = tc
                          , feqn_bndrs  = bndrs
                          , feqn_pats   = tparams
                          , feqn_fixity = fixity
                          , feqn_rhs    = defn })))) }



mkTyFamInst :: SrcSpan
            -> TyFamInstEqn GhcPs
            -> EpToken "type"
            -> EpToken "instance"
            -> P (LInstDecl GhcPs)
mkTyFamInst loc eqn t i = do
  return (L (noAnnSrcSpan loc) (TyFamInstD noExtField
              (TyFamInstDecl (t,i) eqn)))

mkFamDecl :: SrcSpan
          -> FamilyInfo GhcPs
          -> TopLevelFlag
          -> LHsType GhcPs                   -- LHS
          -> LFamilyResultSig GhcPs          -- Optional result signature
          -> Maybe (LInjectivityAnn GhcPs)   -- Injectivity annotation
          -> AnnFamilyDecl
          -> P (LTyClDecl GhcPs)
mkFamDecl loc info topLevel lhs ksig injAnn annsIn
  = do { (tc, tparams, fixity, ops, cps, cs) <- checkTyClHdr False lhs
       ; tyvars <- checkTyVars (ppr info) equals_or_where tc tparams
       ; let loc' = EpAnn (spanAsAnchor loc) noAnn cs
       ; let anns' = annsIn { afd_openp = ops, afd_closep = cps }
       ; return (L loc' (FamDecl noExtField (FamilyDecl
                                           { fdExt       = anns'
                                           , fdTopLevel  = topLevel
                                           , fdInfo      = info, fdLName = tc
                                           , fdTyVars    = tyvars
                                           , fdFixity    = fixity
                                           , fdResultSig = ksig
                                           , fdInjectivityAnn = injAnn }))) }
  where
    equals_or_where = case info of
                        DataFamily          -> empty
                        OpenTypeFamily      -> empty
                        ClosedTypeFamily {} -> whereDots

mkSpliceDecl :: LHsExpr GhcPs -> (LHsDecl GhcPs)
-- If the user wrote
--      [pads| ... ]   then return a QuasiQuoteD
--      $(e)           then return a SpliceD
-- but if they wrote, say,
--      f x            then behave as if they'd written $(f x)
--                     ie a SpliceD
--
-- Typed splices are not allowed at the top level, thus we do not represent them
-- as spliced declaration.  See #10945
mkSpliceDecl lexpr@(L loc expr)
  | HsUntypedSplice _ splice@(HsUntypedSpliceExpr {}) <- expr
    = L loc $ SpliceD noExtField (SpliceDecl noExtField (L (l2l loc) splice) DollarSplice)

  | HsUntypedSplice _ splice@(HsQuasiQuote {}) <- expr
    = L loc $ SpliceD noExtField (SpliceDecl noExtField (L (l2l loc) splice) DollarSplice)

  | otherwise
    = L loc $ SpliceD noExtField (SpliceDecl noExtField
                                 (L (l2l loc) (HsUntypedSpliceExpr noAnn (la2la lexpr)))
                                       BareSplice)

mkRoleAnnotDecl :: SrcSpan
                -> LocatedN RdrName                -- type being annotated
                -> [Located (Maybe FastString)]    -- roles
                -> (EpToken "type", EpToken "role")
                -> P (LRoleAnnotDecl GhcPs)
mkRoleAnnotDecl loc tycon roles anns
  = do { roles' <- mapM parse_role roles
       ; !cs <- getCommentsFor loc
       ; return $ L (EpAnn (spanAsAnchor loc) noAnn cs)
         $ RoleAnnotDecl anns tycon roles' }
  where
    role_data_type = dataTypeOf (undefined :: Role)
    all_roles = map fromConstr $ dataTypeConstrs role_data_type
    possible_roles = [(fsFromRole role, role) | role <- all_roles]

    parse_role (L loc_role Nothing) = return $ L (noAnnSrcSpan loc_role) Nothing
    parse_role (L loc_role (Just role))
      = case lookup role possible_roles of
          Just found_role -> return $ L (noAnnSrcSpan loc_role) $ Just found_role
          Nothing         ->
            let nearby = fuzzyLookup (unpackFS role)
                  (mapFst unpackFS possible_roles)
            in
            addFatalError $ mkPlainErrorMsgEnvelope loc_role $
              (PsErrIllegalRoleName role nearby)

mkMDo :: HsDoFlavour -> LocatedLW [ExprLStmt GhcPs] -> EpaLocation -> EpaLocation -> HsExpr GhcPs
mkMDo ctxt stmts tok loc
  = mkHsDoAnns ctxt stmts (AnnList (Just loc) Nothing Nothing [] tok [])

-- | Converts a list of 'LHsTyVarBndr's annotated with their 'Specificity' to
-- binders without annotations. Only accepts specified variables, and errors if
-- any of the provided binders has an 'InferredSpec' annotation.
fromSpecTyVarBndrs :: [LHsTyVarBndr Specificity GhcPs] -> P [LHsTyVarBndr () GhcPs]
fromSpecTyVarBndrs = mapM fromSpecTyVarBndr

-- | Converts 'LHsTyVarBndr' annotated with its 'Specificity' to one without
-- annotations. Only accepts specified variables, and errors if the provided
-- binder has an 'InferredSpec' annotation.
fromSpecTyVarBndr :: LHsTyVarBndr Specificity GhcPs -> P (LHsTyVarBndr () GhcPs)
fromSpecTyVarBndr (L loc (HsTvb xtv flag idp k)) = do
  case flag of
    SpecifiedSpec -> return ()
    InferredSpec  -> addFatalError $ mkPlainErrorMsgEnvelope (locA loc) $
                                     PsErrInferredTypeVarNotAllowed
  return $ L loc (HsTvb xtv () idp k)

-- | Add the annotation for a 'where' keyword to existing @HsLocalBinds@
annBinds :: EpToken "where" -> EpAnnComments -> HsLocalBinds GhcPs
  -> (HsLocalBinds GhcPs, Maybe EpAnnComments)
annBinds w cs (HsValBinds an bs)  = (HsValBinds (add_where w an cs) bs, Nothing)
annBinds w cs (HsIPBinds an bs)   = (HsIPBinds (add_where w an cs) bs, Nothing)
annBinds _ cs  (EmptyLocalBinds x) = (EmptyLocalBinds x, Just cs)

add_where :: EpToken "where" -> EpAnn (AnnList (EpToken "where")) -> EpAnnComments -> EpAnn (AnnList (EpToken "where"))
add_where w@(EpTok (EpaSpan (RealSrcSpan rs _))) (EpAnn a al cs) cs2
  | valid_anchor a
  = EpAnn (widenAnchorT a w) (al { al_rest = w}) (cs Semi.<> cs2)
  | otherwise
  = EpAnn (patch_anchor rs a)
          (al { al_anchor = (fmap (patch_anchor rs) (al_anchor al))
              , al_rest = w})
          (cs Semi.<> cs2)
add_where _ _ _ = panic "add_where"
 -- EpaDelta should only be used for transformations

valid_anchor :: EpaLocation -> Bool
valid_anchor (EpaSpan (RealSrcSpan r _)) = srcSpanStartLine r >= 0
valid_anchor _ = False

-- If the decl list for where binds is empty, the anchor ends up
-- invalid. In this case, use the parent one
patch_anchor :: RealSrcSpan -> EpaLocation -> EpaLocation
patch_anchor r EpaDelta{} = EpaSpan (RealSrcSpan r Strict.Nothing)
patch_anchor r1 (EpaSpan (RealSrcSpan r0 mb)) = EpaSpan (RealSrcSpan r mb)
  where
    r = if srcSpanStartLine r0 < 0 then r1 else r0
patch_anchor _ (EpaSpan ss) = EpaSpan ss

-- | The anchor for a stmtlist is based on either the location or
-- the first semicolon annotion.
stmtsAnchor :: Located (OrdList (EpToken tok),a) -> Maybe EpaLocation
stmtsAnchor (L (RealSrcSpan l mb) ((ConsOL (EpTok (EpaSpan (RealSrcSpan r rb))) _), _))
  = Just $ widenAnchorS (EpaSpan (RealSrcSpan l mb)) (RealSrcSpan r rb)
stmtsAnchor (L (RealSrcSpan l mb) _) = Just $ EpaSpan (RealSrcSpan l mb)
stmtsAnchor _ = Nothing

stmtsLoc :: Located (OrdList (EpToken tok),a) -> SrcSpan
stmtsLoc (L l ((ConsOL aa _), _))
  = widenSpanT l aa
stmtsLoc (L l _) = l

{- **********************************************************************

  #cvBinds-etc# Converting to @HsBinds@, etc.

  ********************************************************************* -}

-- | Function definitions are restructured here. Each is assumed to be recursive
-- initially, and non recursive definitions are discovered by the dependency
-- analyser.


--  | Groups together bindings for a single function
cvTopDecls :: OrdList (LHsDecl GhcPs) -> [LHsDecl GhcPs]
cvTopDecls decls = getMonoBindAll (fromOL decls)

-- Declaration list may only contain value bindings and signatures.
cvBindGroup :: OrdList (LHsDecl GhcPs) -> P (HsValBinds GhcPs)
cvBindGroup binding
  = do { (mbs, sigs, fam_ds, tfam_insts
         , dfam_insts, _) <- cvBindsAndSigs binding
       ; massert (null fam_ds && null tfam_insts && null dfam_insts)
       ; return $ ValBinds NoAnnSortKey mbs sigs }

cvBindsAndSigs :: OrdList (LHsDecl GhcPs)
  -> P (LHsBinds GhcPs, [LSig GhcPs], [LFamilyDecl GhcPs]
          , [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs], [LDocDecl GhcPs])
-- Input decls contain just value bindings and signatures
-- and in case of class or instance declarations also
-- associated type declarations. They might also contain Haddock comments.
cvBindsAndSigs fb = do
  fb' <- drop_bad_decls (fromOL fb)
  return (partitionBindsAndSigs (getMonoBindAll fb'))
  where
    -- cvBindsAndSigs is called in several places in the parser,
    -- and its items can be produced by various productions:
    --
    --    * decl       (when parsing a where clause or a let-expression)
    --    * decl_inst  (when parsing an instance declaration)
    --    * decl_cls   (when parsing a class declaration)
    --
    -- partitionBindsAndSigs can handle almost all declaration forms produced
    -- by the aforementioned productions, except for SpliceD, which we filter
    -- out here (in drop_bad_decls).
    --
    -- We're not concerned with every declaration form possible, such as those
    -- produced by the topdecl parser production, because cvBindsAndSigs is not
    -- called on top-level declarations.
    drop_bad_decls [] = return []
    drop_bad_decls (L l (SpliceD _ d) : ds) = do
      addError $ mkPlainErrorMsgEnvelope (locA l) $ PsErrDeclSpliceNotAtTopLevel d
      drop_bad_decls ds
    drop_bad_decls (d:ds) = (d:) <$> drop_bad_decls ds

-----------------------------------------------------------------------------
-- Group function bindings into equation groups

getMonoBind :: LHsBind GhcPs -> [LHsDecl GhcPs]
  -> (LHsBind GhcPs, [LHsDecl GhcPs])
-- Suppose      (b',ds') = getMonoBind b ds
--      ds is a list of parsed bindings
--      b is a MonoBinds that has just been read off the front

-- Then b' is the result of grouping more equations from ds that
-- belong with b into a single MonoBinds, and ds' is the depleted
-- list of parsed bindings.
--
-- All Haddock comments between equations inside the group are
-- discarded.
--
-- No AndMonoBinds or EmptyMonoBinds here; just single equations

getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1)
                             , fun_matches =
                               MG { mg_alts = (L _ m1@[L _ mtchs1]) } }))
            binds
  | has_args m1
  = go [L loc1 mtchs1] (noAnnSrcSpan $ locA loc1) binds []
  where
    -- See Note [Exact Print Annotations for FunBind]
    go :: [LMatch GhcPs (LHsExpr GhcPs)] -- accumulates matches for current fun
       -> SrcSpanAnnA                    -- current top level loc
       -> [LHsDecl GhcPs]                -- Any docbinds seen
       -> [LHsDecl GhcPs]                -- rest of decls to be processed
       -> (LHsBind GhcPs, [LHsDecl GhcPs]) -- FunBind, rest of decls
    go mtchs loc
       ((L loc2 (ValD _ (FunBind { fun_id = (L _ f2)
                                 , fun_matches =
                                    MG { mg_alts = (L _ [L lm2 mtchs2]) } })))
         : binds) _
        | f1 == f2 =
          let (loc2', lm2') = transferAnnsA loc2 lm2
          in go (L lm2' mtchs2 : mtchs)
                        (combineSrcSpansA loc loc2') binds []
    go mtchs loc (doc_decl@(L loc2 (DocD {})) : binds) doc_decls
        = let doc_decls' = doc_decl : doc_decls
          in go mtchs (combineSrcSpansA loc loc2) binds doc_decls'
    go mtchs loc binds doc_decls
        = let
            L llm last_m = head mtchs -- Guaranteed at least one
            (llm',loc') = transferAnnsOnlyA llm loc -- Keep comments, transfer trailing

            matches' = reverse (L llm' last_m:tail mtchs)
            L lfm first_m =  head matches'
            (lfm', loc'') = transferCommentsOnlyA lfm loc'
          in
            ( L loc'' (makeFunBind fun_id1 (mkLocatedList $ (L lfm' first_m:tail matches')))
              , (reverse doc_decls) ++ binds)
        -- Reverse the final matches, to get it back in the right order
        -- Do the same thing with the trailing doc comments

getMonoBind bind binds = (bind, binds)

{- Note [Exact Print Annotations for FunBind]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An individual Match that ends up in a FunBind MatchGroup is initially
parsed as a LHsDecl. This takes the form

   L loc (ValD NoExtField (FunBind ... [L lm (Match ..)]))

The loc contains the annotations, in particular comments, which are to
precede the declaration when printed, and [TrailingAnn] which are to
follow it. The [TrailingAnn] captures semicolons that may appear after
it when using the braces and semis style of coding.

The match location (lm) has only a location in it at this point, no
annotations. Its location is the same as the top level location in
loc.

What getMonoBind does it to take a sequence of FunBind LHsDecls that
belong to the same function and group them into a single function with
the component declarations all combined into the single MatchGroup as
[LMatch GhcPs].

Given that when exact printing a FunBind the exact printer simply
iterates over all the matches and prints each in turn, the simplest
behaviour would be to simply take the top level annotations (loc) for
each declaration, and use them for the individual component matches
(lm).

The problem is the exact printer first has to deal with the top level
LHsDecl, which means annotations for the loc. This needs to be able to
be exact printed in the context of surrounding declarations, and if
some refactor decides to move the declaration elsewhere, the leading
comments and trailing semicolons need to be handled at that level.

So the solution is to combine all the matches into one, pushing the
annotations into the LMatch's, and then at the end extract the
comments from the first match and [TrailingAnn] from the last to go in
the top level LHsDecl.
-}

-- Group together adjacent FunBinds for every function.
getMonoBindAll :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
getMonoBindAll [] = []
getMonoBindAll (L l (ValD _ b) : ds) =
  let (L l' b', ds') = getMonoBind (L l b) ds
  in L l' (ValD noExtField b') : getMonoBindAll ds'
getMonoBindAll (d : ds) = d : getMonoBindAll ds

has_args :: [LMatch GhcPs (LHsExpr GhcPs)] -> Bool
has_args []                                  = panic "GHC.Parser.PostProcess.has_args"
has_args (L _ (Match { m_pats = L _ args }) : _) = not (null args)
        -- Don't group together FunBinds if they have
        -- no arguments.  This is necessary now that variable bindings
        -- with no arguments are now treated as FunBinds rather
        -- than pattern bindings (tests/rename/should_fail/rnfail002).

{- **********************************************************************

  #PrefixToHS-utils# Utilities for conversion

  ********************************************************************* -}

{- Note [Parsing data constructors is hard]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The problem with parsing data constructors is that they look a lot like types.
Compare:

  (s1)   data T = C t1 t2
  (s2)   type T = C t1 t2

Syntactically, there's little difference between these declarations, except in
(s1) 'C' is a data constructor, but in (s2) 'C' is a type constructor.

This similarity would pose no problem if we knew ahead of time if we are
parsing a type or a constructor declaration. Looking at (s1) and (s2), a simple
(but wrong!) rule comes to mind: in 'data' declarations assume we are parsing
data constructors, and in other contexts (e.g. 'type' declarations) assume we
are parsing type constructors.

This simple rule does not work because of two problematic cases:

  (p1)   data T = C t1 t2 :+ t3
  (p2)   data T = C t1 t2 => t3

In (p1) we encounter (:+) and it turns out we are parsing an infix data
declaration, so (C t1 t2) is a type and 'C' is a type constructor.
In (p2) we encounter (=>) and it turns out we are parsing an existential
context, so (C t1 t2) is a constraint and 'C' is a type constructor.

As the result, in order to determine whether (C t1 t2) declares a data
constructor, a type, or a context, we would need unlimited lookahead which
'happy' is not so happy with.
-}

-- | Reinterpret a type constructor, including type operators, as a data
--   constructor.
-- See Note [Parsing data constructors is hard]
tyConToDataCon :: LocatedN RdrName -> Either (MsgEnvelope PsMessage) (LocatedN RdrName)
tyConToDataCon (L loc tc)
  | okConOcc (occNameString occ)
  = return (L loc (setRdrNameSpace tc srcDataName))

  | otherwise
  = Left $ mkPlainErrorMsgEnvelope (locA loc) $ (PsErrNotADataCon tc)
  where
    occ = rdrNameOcc tc

mkPatSynMatchGroup :: LocatedN RdrName
                   -> LocatedLW (OrdList (LHsDecl GhcPs))
                   -> P (MatchGroup GhcPs (LHsExpr GhcPs))
mkPatSynMatchGroup (L loc patsyn_name) (L ld decls) =
    do { matches <- mapM fromDecl (fromOL decls)
       ; when (null matches) (wrongNumberErr (locA loc))
       ; return $ mkMatchGroup FromSource (L ld matches) }
  where
    fromDecl (L loc decl@(ValD _ (PatBind _
                         pat@(L _ (ConPat _conAnn ln@(L _ name) details))
                               _ rhs))) =
        do { unless (name == patsyn_name) $
               wrongNameBindingErr (locA loc) decl
           -- conAnn should only be AnnOpenP, AnnCloseP, so the rest should be empty
           ; let ann_fun = mk_ann_funrhs [] []
           ; match <- case details of
               PrefixCon _ pats -> return $ Match { m_ext = noExtField
                                                  , m_ctxt = ctxt, m_pats = L l pats
                                                  , m_grhss = rhs }
                   where
                     l = listLocation pats
                     ctxt = FunRhs { mc_fun = ln
                                   , mc_fixity = Prefix
                                   , mc_strictness = NoSrcStrict
                                   , mc_an = ann_fun }

               InfixCon p1 p2 -> return $ Match { m_ext = noExtField
                                                , m_ctxt = ctxt
                                                , m_pats = L l [p1, p2]
                                                , m_grhss = rhs }
                   where
                     l = listLocation [p1, p2]
                     ctxt = FunRhs { mc_fun = ln
                                   , mc_fixity = Infix
                                   , mc_strictness = NoSrcStrict
                                   , mc_an = ann_fun }

               RecCon{} -> recordPatSynErr (locA loc) pat
           ; return $ L loc match }
    fromDecl (L loc decl) = extraDeclErr (locA loc) decl

    extraDeclErr loc decl =
        addFatalError $ mkPlainErrorMsgEnvelope loc $
          (PsErrNoSingleWhereBindInPatSynDecl patsyn_name decl)

    wrongNameBindingErr loc decl =
      addFatalError $ mkPlainErrorMsgEnvelope loc $
          (PsErrInvalidWhereBindInPatSynDecl patsyn_name decl)

    wrongNumberErr loc =
      addFatalError $ mkPlainErrorMsgEnvelope loc $
        (PsErrEmptyWhereInPatSynDecl patsyn_name)

recordPatSynErr :: SrcSpan -> LPat GhcPs -> P a
recordPatSynErr loc pat =
    addFatalError $ mkPlainErrorMsgEnvelope loc $
      (PsErrRecordSyntaxInPatSynDecl pat)

mkConDeclH98 :: (TokDarrow, (TokForall, EpToken ".")) -> LocatedN RdrName -> Maybe [LHsTyVarBndr Specificity GhcPs]
                -> Maybe (LHsContext GhcPs) -> HsConDeclH98Details GhcPs
                -> ConDecl GhcPs

mkConDeclH98 (tdarrow, (tforall,tdot)) name mb_forall mb_cxt args
  = ConDeclH98 { con_ext    = AnnConDeclH98 tforall tdot tdarrow
               , con_name   = name
               , con_forall = isJust mb_forall
               , con_ex_tvs = mb_forall `orElse` []
               , con_mb_cxt = mb_cxt
               , con_args   = args
               , con_doc    = Nothing }

-- | Construct a GADT-style data constructor from the constructor names and
-- their type. Some interesting aspects of this function:
--
-- * This splits up the constructor type into its quantified type variables (if
--   provided), context (if provided), argument types, and result type, and
--   records whether this is a prefix or record GADT constructor. See
--   Note [GADT abstract syntax] in "GHC.Hs.Decls" for more details.
mkGadtDecl :: SrcSpan
           -> NonEmpty (LocatedN RdrName)
           -> TokDcolon
           -> LHsSigType GhcPs
           -> P (LConDecl GhcPs)
mkGadtDecl loc names dcol ty = do

  (args, res_ty, (ops, cps), csa) <-
    case body_ty of
     L ll (HsFunTy _ hsArr (L (EpAnn anc _ cs) (HsRecTy an rf)) res_ty) -> do
       arr <- case hsArr of
         HsUnrestrictedArrow arr -> return arr
         _ -> do addError $ mkPlainErrorMsgEnvelope (getLocA body_ty) $
                                 (PsErrIllegalGadtRecordMultiplicity hsArr)
                 return noAnn

       return ( RecConGADT arr (L (EpAnn anc an cs) rf), res_ty
              , ([], []), epAnnComments ll)
     _ -> do
       let ((ops, cps), cs, arg_types, res_type) = splitHsFunType body_ty
       return (PrefixConGADT noExtField arg_types, res_type, (ops,cps), cs)

  let bndrs_loc = case outer_bndrs of
        HsOuterImplicit{} -> getLoc ty
        HsOuterExplicit an _ -> EpAnn (entry an) noAnn emptyComments

  let l = EpAnn (spanAsAnchor loc) noAnn csa

  pure $ L l ConDeclGADT
                     { con_g_ext  = AnnConDeclGADT ops cps dcol
                     , con_names  = names
                     , con_bndrs  = L bndrs_loc outer_bndrs
                     , con_mb_cxt = mcxt
                     , con_g_args = args
                     , con_res_ty = res_ty
                     , con_doc    = Nothing }
  where
    (outer_bndrs, mcxt, body_ty) = splitLHsGadtTy ty

setRdrNameSpace :: RdrName -> NameSpace -> RdrName
-- ^ This rather gruesome function is used mainly by the parser.
--
-- Case #1. When parsing:
--
-- > data T a = T | T1 Int
--
-- we parse the data constructors as /types/ because of parser ambiguities,
-- so then we need to change the /type constr/ to a /data constr/
--
-- The exact-name case /can/ occur when parsing:
--
-- > data [] a = [] | a : [a]
--
-- For the exact-name case we return an original name.
--
-- Case #2. When parsing:
--
-- > x = fn (forall a. a)   -- RequiredTypeArguments
--
-- we use setRdrNameSpace to set the namespace of forall-bound variables.
--
setRdrNameSpace (Unqual occ) ns = Unqual (setOccNameSpace ns occ)
setRdrNameSpace (Qual m occ) ns = Qual m (setOccNameSpace ns occ)
setRdrNameSpace (Orig m occ) ns = Orig m (setOccNameSpace ns occ)
setRdrNameSpace (Exact n)    ns
  | Just thing <- wiredInNameTyThing_maybe n
  = setWiredInNameSpace thing ns
    -- Preserve Exact Names for wired-in things,
    -- notably tuples and lists

  | isExternalName n
  = Orig (nameModule n) occ

  | otherwise   -- This can happen when quoting and then
                -- splicing a fixity declaration for a type
  = Exact (mkSystemNameAt (nameUnique n) occ (nameSrcSpan n))
  where
    occ = setOccNameSpace ns (nameOccName n)

setWiredInNameSpace :: TyThing -> NameSpace -> RdrName
setWiredInNameSpace (ATyCon tc) ns
  | isDataConNameSpace ns
  = ty_con_data_con tc
  | isTcClsNameSpace ns
  = Exact (getName tc)      -- No-op

setWiredInNameSpace (AConLike (RealDataCon dc)) ns
  | isTcClsNameSpace ns
  = data_con_ty_con dc
  | isDataConNameSpace ns
  = Exact (getName dc)      -- No-op

setWiredInNameSpace thing ns
  = pprPanic "setWiredinNameSpace" (pprNameSpace ns <+> ppr thing)

ty_con_data_con :: TyCon -> RdrName
ty_con_data_con tc
  | isTupleTyCon tc
  , Just dc <- tyConSingleDataCon_maybe tc
  = Exact (getName dc)

  | tc `hasKey` listTyConKey
  = Exact nilDataConName

  | otherwise  -- See Note [setRdrNameSpace for wired-in names]
  = Unqual (setOccNameSpace srcDataName (getOccName tc))

data_con_ty_con :: DataCon -> RdrName
data_con_ty_con dc
  | let tc = dataConTyCon dc
  , isTupleTyCon tc
  = Exact (getName tc)

  | dc `hasKey` nilDataConKey
  = Exact listTyConName

  | otherwise  -- See Note [setRdrNameSpace for wired-in names]
  = Unqual (setOccNameSpace tcClsName (getOccName dc))



{- Note [setRdrNameSpace for wired-in names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In GHC.Types, which declares (:), we have
  infixr 5 :
The ambiguity about which ":" is meant is resolved by parsing it as a
data constructor, but then using dataTcOccs to try the type constructor too;
and that in turn calls setRdrNameSpace to change the name-space of ":" to
tcClsName.  There isn't a corresponding ":" type constructor, but it's painful
to make setRdrNameSpace partial, so we just make an Unqual name instead. It
really doesn't matter!
-}

eitherToP :: MonadP m => Either (MsgEnvelope PsMessage) a -> m a
-- Adapts the Either monad to the P monad
eitherToP (Left err)    = addFatalError err
eitherToP (Right thing) = return thing

checkTyVars :: SDoc -> SDoc -> LocatedN RdrName -> [LHsTypeArg GhcPs]
            -> P (LHsQTyVars GhcPs)  -- the synthesized type variables
-- ^ Check whether the given list of type parameters are all type variables
-- (possibly with a kind signature).
checkTyVars pp_what equals_or_where tc tparms
  = do { tvs <- mapM check tparms
       ; return (mkHsQTvs tvs) }
  where
    check (HsTypeArg at ki) = chkParens [] [] (HsBndrInvisible at) ki
    check (HsValArg _ ty) = chkParens [] [] (HsBndrRequired noExtField) ty
    check (HsArgPar sp) = addFatalError $ mkPlainErrorMsgEnvelope sp $
                            (PsErrMalformedDecl pp_what (unLoc tc))
        -- Keep around an action for adjusting the annotations of extra parens
    chkParens :: [EpaLocation] -> [EpaLocation] -> HsBndrVis GhcPs -> LHsType GhcPs
              -> P (LHsTyVarBndr (HsBndrVis GhcPs) GhcPs)
    chkParens ops cps bvis (L l (HsParTy _ (L lt ty)))
      = let
          (o,c) = mkParensLocs (realSrcSpan $ locA l)
          (_,lt') = transferCommentsOnlyA l lt
        in
          chkParens (o:ops) (c:cps) bvis (L lt' ty)
    chkParens ops cps bvis ty = chk ops cps bvis ty

        -- Check that the name space is correct!
    chk :: [EpaLocation] -> [EpaLocation] -> HsBndrVis GhcPs -> LHsType GhcPs -> P (LHsTyVarBndr (HsBndrVis GhcPs) GhcPs)
    chk ops cps bvis (L l (HsKindSig tok_dc (L annt t) k))
        | Just (ann, bvar) <- match_bndr_var t
            = let
                bkind = HsBndrKind noExtField k
                an = (reverse ops) ++ cps
              in
                return (L (widenLocatedAnL (l Semi.<> annt) (for_widening bvis:an))
                       (HsTvb (AnnTyVarBndr (reverse ops) cps ann tok_dc) bvis bvar bkind))
    chk ops cps bvis (L l t)
        | Just (ann, bvar) <- match_bndr_var t
            = let
                bkind = HsBndrNoKind noExtField
                an = (reverse ops) ++ cps
              in
                return (L (widenLocatedAnL l (for_widening bvis:an))
                                     (HsTvb (AnnTyVarBndr (reverse ops) cps ann noAnn) bvis bvar bkind))
    chk _ _ _ t@(L loc _)
        = addFatalError $ mkPlainErrorMsgEnvelope (locA loc) $
            (PsErrUnexpectedTypeInDecl t pp_what (unLoc tc) tparms equals_or_where)

    match_bndr_var :: HsType GhcPs -> Maybe (EpToken "'", HsBndrVar GhcPs)
    match_bndr_var (HsTyVar ann _ tv) | isRdrTyVar (unLoc tv)
      = Just (ann, HsBndrVar noExtField tv)
    match_bndr_var (HsWildCardTy _)
      = Just (noAnn, HsBndrWildCard noExtField)
    match_bndr_var _ = Nothing

    -- Return an AddEpAnn for use in widenLocatedAnL. The AnnKeywordId is not used.
    for_widening :: HsBndrVis GhcPs -> EpaLocation
    for_widening (HsBndrInvisible (EpTok loc)) = loc
    for_widening  _                            = noAnn


whereDots, equalsDots :: SDoc
-- Second argument to checkTyVars
whereDots  = text "where ..."
equalsDots = text "= ..."

checkDatatypeContext :: Maybe (LHsContext GhcPs) -> P ()
checkDatatypeContext Nothing = return ()
checkDatatypeContext (Just c)
    = do allowed <- getBit DatatypeContextsBit
         unless allowed $ addError $ mkPlainErrorMsgEnvelope (getLocA c) $
                                       (PsErrIllegalDataTypeContext c)

type LRuleTyTmVar = LocatedAn NoEpAnns RuleTyTmVar
data RuleTyTmVar = RuleTyTmVar AnnTyVarBndr (LocatedN RdrName) (Maybe (LHsType GhcPs))
-- ^ Essentially a wrapper for a @RuleBndr GhcPs@

-- turns RuleTyTmVars into RuleBnrs - this is straightforward
mkRuleBndrs :: [LRuleTyTmVar] -> [LRuleBndr GhcPs]
mkRuleBndrs = fmap (fmap cvt_one)
  where cvt_one (RuleTyTmVar ann v Nothing) = RuleBndr ann v
        cvt_one (RuleTyTmVar ann v (Just sig)) =
          RuleBndrSig ann v (mkHsPatSigType noAnn sig)

-- turns RuleTyTmVars into HsTyVarBndrs - this is more interesting
mkRuleTyVarBndrs :: [LRuleTyTmVar] -> [LHsTyVarBndr () GhcPs]
mkRuleTyVarBndrs = fmap (setLHsTyVarBndrNameSpace tvName . cvt_one)
  where cvt_one (L l (RuleTyTmVar ann v msig))
          = L (l2l l) (HsTvb ann () (HsBndrVar noExtField v) (cvt_sig msig))
        cvt_sig Nothing    = HsBndrNoKind noExtField
        cvt_sig (Just sig) = HsBndrKind   noExtField sig

-- See Note [Parsing explicit foralls in Rules] in Parser.y
checkRuleTyVarBndrNames :: [LHsTyVarBndr flag GhcPs] -> P ()
checkRuleTyVarBndrNames = mapM_ check . mapMaybe (hsTyVarLName . unLoc)
  where check (L loc (Unqual occ)) =
          when (occNameFS occ `elem` [fsLit "family",fsLit "role"])
            (addFatalError $ mkPlainErrorMsgEnvelope (locA loc) $
               (PsErrParseErrorOnInput occ))
        check _ = panic "checkRuleTyVarBndrNames"

checkRecordSyntax :: (MonadP m, Outputable a) => LocatedA a -> m (LocatedA a)
checkRecordSyntax lr@(L loc r)
    = do allowed <- getBit TraditionalRecordSyntaxBit
         unless allowed $ addError $ mkPlainErrorMsgEnvelope (locA loc) $
                                       (PsErrIllegalTraditionalRecordSyntax (ppr r))
         return lr

-- | Check if the gadt_constrlist is empty. Only raise parse error for
-- `data T where` to avoid affecting existing error message, see #8258.
checkEmptyGADTs :: Located ((EpToken "where", EpToken "{", EpToken "}"), [LConDecl GhcPs])
                -> P (Located ((EpToken "where", EpToken "{", EpToken "}"), [LConDecl GhcPs]))
checkEmptyGADTs gadts@(L span (_, []))           -- Empty GADT declaration.
    = do gadtSyntax <- getBit GadtSyntaxBit   -- GADTs implies GADTSyntax
         unless gadtSyntax $ addError $ mkPlainErrorMsgEnvelope span $
                                          PsErrIllegalWhereInDataDecl
         return gadts
checkEmptyGADTs gadts = return gadts              -- Ordinary GADT declaration.

checkTyClHdr :: Bool               -- True  <=> class header
                                   -- False <=> type header
             -> LHsType GhcPs
             -> P (LocatedN RdrName,     -- the head symbol (type or class name)
                   [LHsTypeArg GhcPs],   -- parameters of head symbol
                   LexicalFixity,        -- the declaration is in infix format
                   [EpToken "("],        -- API Annotation for HsParTy
                   [EpToken ")"],        -- when stripping parens
                   EpAnnComments)        -- Accumulated comments from re-arranging
-- Well-formedness check and decomposition of type and class heads.
-- Decomposes   T ty1 .. tyn   into    (T, [ty1, ..., tyn])
--              Int :*: Bool   into    (:*:, [Int, Bool])
-- returning the pieces
checkTyClHdr is_cls ty
  = goL emptyComments ty [] [] [] Prefix
  where
    goL cs (L l ty) acc ops cps fix = go cs l ty acc ops cps fix

    -- workaround to define '*' despite StarIsType
    go cs ll (HsParTy an (L l (HsStarTy _ isUni))) acc ops' cps' fix
      = do { addPsMessage (locA l) PsWarnStarBinder
           ; let name = mkOccNameFS tcClsName (starSym isUni)
           ; let a' = newAnns ll l an
           ; return (L a' (Unqual name), acc, fix
                    , (reverse ops'), cps', cs) }

    go cs l (HsTyVar _ _ ltc@(L _ tc)) acc ops cps fix
      | isRdrTc tc               = return (ltc, acc, fix, (reverse ops), cps, cs Semi.<> comments l)
    go cs l (HsOpTy _ _ t1 ltc@(L _ tc) t2) acc ops cps _fix
      | isRdrTc tc               = return (ltc, lhs:rhs:acc, Infix, (reverse ops), cps, cs Semi.<> comments l)
      where lhs = HsValArg noExtField t1
            rhs = HsValArg noExtField t2
    go cs l (HsParTy (o,c) ty)    acc ops cps fix = goL (cs Semi.<> comments l) ty acc (o:ops) (c:cps) fix
    go cs l (HsAppTy _ t1 t2) acc ops cps fix = goL (cs Semi.<> comments l) t1 (HsValArg noExtField t2:acc) ops cps fix
    go cs l (HsAppKindTy at ty ki) acc ops cps fix = goL (cs Semi.<> comments l) ty (HsTypeArg at ki:acc) ops cps fix
    go cs l (HsTupleTy _ HsBoxedOrConstraintTuple ts) [] ops cps fix
      = return (L (l2l l) (nameRdrName tup_name)
               , map (HsValArg noExtField) ts, fix, (reverse ops), cps, cs Semi.<> comments l)
      where
        arity = length ts
        tup_name | is_cls    = cTupleTyConName arity
                 | otherwise = getName (tupleTyCon Boxed arity)
          -- See Note [Unit tuples] in GHC.Hs.Type  (TODO: is this still relevant?)
    go _ l _ _ _ _ _
      = addFatalError $ mkPlainErrorMsgEnvelope (locA l) $
          (PsErrMalformedTyOrClDecl ty)

    -- Combine the annotations from the HsParTy and HsStarTy into a
    -- new one for the LocatedN RdrName
    newAnns :: SrcSpanAnnA -> SrcSpanAnnA -> (EpToken "(", EpToken ")") -> SrcSpanAnnN
    newAnns l@(EpAnn _ (AnnListItem _) csp0) l1@(EpAnn ap (AnnListItem ta) csp) (o,c) =
      let
        lr = combineSrcSpans (locA l1) (locA l)
      in
        EpAnn (EpaSpan lr) (NameAnn NameParens (getEpTokenLoc o) ap (getEpTokenLoc c) ta) (csp0 Semi.<> csp)

-- | Yield a parse error if we have a function applied directly to a do block
-- etc. and BlockArguments is not enabled.
checkExpBlockArguments :: LHsExpr GhcPs -> PV ()
checkCmdBlockArguments :: LHsCmd GhcPs -> PV ()
(checkExpBlockArguments, checkCmdBlockArguments) = (checkExpr, checkCmd)
  where
    checkExpr :: LHsExpr GhcPs -> PV ()
    checkExpr expr = case unLoc expr of
      HsDo _ (DoExpr m) _      -> check (PsErrDoInFunAppExpr m)               expr
      HsDo _ (MDoExpr m) _     -> check (PsErrMDoInFunAppExpr m)              expr
      HsCase {}                -> check PsErrCaseInFunAppExpr                 expr
      HsLam _ lam_variant _    -> check (PsErrLambdaInFunAppExpr lam_variant) expr
      HsLet {}                 -> check PsErrLetInFunAppExpr                  expr
      HsIf {}                  -> check PsErrIfInFunAppExpr                   expr
      HsProc {}                -> check PsErrProcInFunAppExpr                 expr
      _                        -> return ()

    checkCmd :: LHsCmd GhcPs -> PV ()
    checkCmd cmd = case unLoc cmd of
      HsCmdLam _ lam_variant _ -> check (PsErrLambdaCmdInFunAppCmd lam_variant) cmd
      HsCmdCase {}             -> check PsErrCaseCmdInFunAppCmd                 cmd
      HsCmdIf {}               -> check PsErrIfCmdInFunAppCmd                   cmd
      HsCmdLet {}              -> check PsErrLetCmdInFunAppCmd                  cmd
      HsCmdDo {}               -> check PsErrDoCmdInFunAppCmd                   cmd
      _                        -> return ()

    check err a = do
      blockArguments <- getBit BlockArgumentsBit
      unless blockArguments $
        addError $ mkPlainErrorMsgEnvelope (getLocA a) $ (err a)

-- | Validate the context constraints and break up a context into a list
-- of predicates.
--
-- @
--     (Eq a, Ord b)        -->  [Eq a, Ord b]
--     Eq a                 -->  [Eq a]
--     (Eq a)               -->  [Eq a]
--     (((Eq a)))           -->  [Eq a]
-- @
checkContext :: LHsType GhcPs -> P (LHsContext GhcPs)
checkContext orig_t@(L (EpAnn l _ cs) _orig_t) =
  check ([],[],cs) orig_t
 where
  check :: ([EpaLocation],[EpaLocation],EpAnnComments)
        -> LHsType GhcPs -> P (LHsContext GhcPs)
  check (oparens,cparens,cs) (L _l (HsTupleTy ann' HsBoxedOrConstraintTuple ts))
    -- (Eq a, Ord b) shows up as a tuple type. Only boxed tuples can
    -- be used as context constraints.
    -- Ditto ()
    = mkCTuple (oparens ++ [ap_open ann'], ap_close ann' : cparens, cs) ts

  -- With NoListTuplePuns, contexts are parsed as data constructors, which causes failure
  -- downstream.
  -- This converts them just like when they are parsed as types in the punned case.
  check (oparens,cparens,cs) (L _l (HsExplicitTupleTy (q,o,c) ts))
    = punsAllowed >>= \case
      True -> unprocessed
      False -> do
        let
          ol = AddEpAnn AnnOpenP (getEpTokenLoc o)
          cl = AddEpAnn AnnCloseP (getEpTokenLoc c)
          (op, cp) = case q of
            EpTok ql -> ([AddEpAnn AnnSimpleQuote ql], [cl])
            _        -> ([ol], [cl])
        mkCTuple (oparens ++ (addLoc <$> op), (addLoc <$> cp) ++ cparens, cs) ts
  check (opi,cpi,csi) (L _lp1 (HsParTy (o,c) ty))
                                             -- to be sure HsParTy doesn't get into the way
    = check (getEpTokenLoc o:opi, getEpTokenLoc c:cpi, csi) ty

  -- No need for anns, returning original
  check (_opi,_cpi,_csi) _t = unprocessed

  unprocessed =
    return (L (EpAnn l (AnnContext Nothing [] []) emptyComments) [orig_t])

  addLoc (AddEpAnn _ l) = l

  mkCTuple (oparens, cparens, cs) ts =
    -- Append parens so that the original order in the source is maintained
    return (L (EpAnn l (AnnContext Nothing oparens cparens) cs) ts)

-- | The same as `checkContext`, but for expressions.
--
-- Validate the context constraints and break up a context into a list
-- of predicates.
--
-- @
--     (Eq a, Ord b)        -->  [Eq a, Ord b]
--     Eq a                 -->  [Eq a]
--     (Eq a)               -->  [Eq a]
--     (((Eq a)))           -->  [Eq a]
-- @
checkContextExpr :: LHsExpr GhcPs -> PV (LocatedC [LHsExpr GhcPs])
checkContextExpr orig_expr@(L (EpAnn l _ cs) _) =
  check ([],[], cs) orig_expr
  where
    check :: ([EpaLocation],[EpaLocation],EpAnnComments)
        -> LHsExpr GhcPs -> PV (LocatedC [LHsExpr GhcPs])
    check (oparens,cparens,cs) (L _ (ExplicitTuple (ap_open, ap_close) tup_args boxity))
             -- Neither unboxed tuples (#e1,e2#) nor tuple sections (e1,,e2,) can be a context
      | isBoxed boxity
      , Just es <- tupArgsPresent_maybe tup_args
      = mkCTuple (oparens ++ [ap_open], ap_close : cparens, cs) es
    check (opi, cpi, csi) (L _ (HsPar (EpTok open_tok, EpTok close_tok) expr))
      = check (opi ++ [open_tok], close_tok : cpi, csi) expr
    check (oparens,cparens,cs) (L _ (HsVar _ (L (EpAnn _ (NameAnnOnly NameParens open closed []) _) name)))
      | name == nameRdrName (dataConName unitDataCon)
      = mkCTuple (oparens ++ [open], closed : cparens, cs) []
    check _ _ = unprocessed

    unprocessed =
      return (L (EpAnn l (AnnContext Nothing [] []) emptyComments) [orig_expr])

    mkCTuple (oparens, cparens, cs) ts =
      -- Append parens so that the original order in the source is maintained
      return (L (EpAnn l (AnnContext Nothing oparens cparens) cs) ts)

checkImportDecl :: Maybe EpaLocation
                -> Maybe EpaLocation
                -> P ()
checkImportDecl mPre mPost = do
  let whenJust mg f = maybe (pure ()) f mg

  importQualifiedPostEnabled <- getBit ImportQualifiedPostBit

  -- Error if 'qualified' found in postpositive position and
  -- 'ImportQualifiedPost' is not in effect.
  whenJust mPost $ \post ->
    when (not importQualifiedPostEnabled) $
      failNotEnabledImportQualifiedPost (RealSrcSpan (epaLocationRealSrcSpan post) Strict.Nothing)

  -- Error if 'qualified' occurs in both pre and postpositive
  -- positions.
  whenJust mPost $ \post ->
    when (isJust mPre) $
      failImportQualifiedTwice (RealSrcSpan (epaLocationRealSrcSpan post) Strict.Nothing)

  -- Warn if 'qualified' found in prepositive position and
  -- 'Opt_WarnPrepositiveQualifiedModule' is enabled.
  whenJust mPre $ \pre ->
    warnPrepositiveQualifiedModule (RealSrcSpan (epaLocationRealSrcSpan pre) Strict.Nothing)

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: LocatedA (PatBuilder GhcPs) -> P (LPat GhcPs)
checkPattern = runPV . checkLPat

checkPattern_details :: ParseContext -> PV (LocatedA (PatBuilder GhcPs)) -> P (LPat GhcPs)
checkPattern_details extraDetails pp = runPV_details extraDetails (pp >>= checkLPat)

checkLArgPat :: LocatedA (ArgPatBuilder GhcPs) -> PV (LPat GhcPs)
checkLArgPat (L l (ArgPatBuilderVisPat p)) = checkLPat (L l p)
checkLArgPat (L l (ArgPatBuilderArgPat p)) = return (L l p)

checkLPat :: LocatedA (PatBuilder GhcPs) -> PV (LPat GhcPs)
checkLPat (L l@(EpAnn anc an _) p) = do
  (L l' p', cs) <- checkPat (EpAnn anc an emptyComments) emptyComments (L l p) [] []
  return (L (addCommentsToEpAnn l' cs) p')

checkPat :: SrcSpanAnnA -> EpAnnComments -> LocatedA (PatBuilder GhcPs) -> [HsConPatTyArg GhcPs] -> [LPat GhcPs]
         -> PV (LPat GhcPs, EpAnnComments)
-- SG: I think this function checks what Haskell2010 calls the `pat` and `lpat`
-- productions
checkPat loc cs (L l e@(PatBuilderVar (L ln c))) tyargs args
  | isRdrDataCon c || isRdrTc c
  = return (L loc $ ConPat
      { pat_con_ext = noAnn -- AZ: where should this come from?
      , pat_con = L ln c
      , pat_args = PrefixCon tyargs args
      }, comments l Semi.<> cs)
  | (not (null args) && patIsRec c) = do
      ctx <- askParseContext
      patFail (locA l) . PsErrInPat e $ PEIP_RecPattern args YesPatIsRecursive ctx
checkPat loc cs (L la (PatBuilderAppType f at t)) tyargs args =
  checkPat loc (cs Semi.<> comments la) f (HsConPatTyArg at t : tyargs) args
checkPat loc cs (L la (PatBuilderApp f e)) [] args = do
  p <- checkLPat e
  checkPat loc (cs Semi.<> comments la) f [] (p : args)
checkPat loc cs (L l e) [] [] = do
  p <- checkAPat loc e
  return (L l p, cs)
checkPat loc _ e _ _ = do
  details <- fromParseContext <$> askParseContext
  patFail (locA loc) (PsErrInPat (unLoc e) details)

checkAPat :: SrcSpanAnnA -> PatBuilder GhcPs -> PV (Pat GhcPs)
checkAPat loc e0 = do
 nPlusKPatterns <- getBit NPlusKPatternsBit
 case e0 of
   PatBuilderPat p -> return p
   PatBuilderVar x -> return (VarPat noExtField x)

   -- Overloaded numeric patterns (e.g. f 0 x = x)
   -- Negation is recorded separately, so that the literal is zero or +ve
   -- NB. Negative *primitive* literals are already handled by the lexer
   PatBuilderOverLit pos_lit -> return (mkNPat (L (l2l loc) pos_lit) Nothing noAnn)

   -- n+k patterns
   PatBuilderOpApp
           (L _ (PatBuilderVar (L nloc n)))
           (L l plus)
           (L lloc (PatBuilderOverLit lit@(OverLit {ol_val = HsIntegral {}})))
           _
                     | nPlusKPatterns && (plus == plus_RDR)
                     -> return (mkNPlusKPat (L nloc n) (L (l2l lloc) lit)
                                (entry l))

   -- Improve error messages for the @-operator when the user meant an @-pattern
   PatBuilderOpApp _ op _ _ | opIsAt (unLoc op) -> do
     addError $ mkPlainErrorMsgEnvelope (getLocA op) PsErrAtInPatPos
     return (WildPat noExtField)

   PatBuilderOpApp l (L cl c) r (_os,_cs)
     | isRdrDataCon c || isRdrTc c -> do
         l <- checkLPat l
         r <- checkLPat r
         return $ ConPat
           { pat_con_ext = noAnn
           , pat_con = L cl c
           , pat_args = InfixCon l r
           }

   PatBuilderPar lpar e rpar -> do
     p <- checkLPat e
     return (ParPat (lpar, rpar) p)

   _           -> do
     details <- fromParseContext <$> askParseContext
     patFail (locA loc) (PsErrInPat e0 details)

placeHolderPunRhs :: DisambECP b => PV (LocatedA b)
-- The RHS of a punned record field will be filled in by the renamer
-- It's better not to make it an error, in case we want to print it when
-- debugging
placeHolderPunRhs = mkHsVarPV (noLocA pun_RDR)

plus_RDR, pun_RDR :: RdrName
plus_RDR = mkUnqual varName (fsLit "+") -- Hack
pun_RDR  = mkUnqual varName (fsLit "pun-right-hand-side")

checkPatField :: LHsRecField GhcPs (LocatedA (PatBuilder GhcPs))
              -> PV (LHsRecField GhcPs (LPat GhcPs))
checkPatField (L l fld) = do p <- checkLPat (hfbRHS fld)
                             return (L l (fld { hfbRHS = p }))

patFail :: SrcSpan -> PsMessage -> PV a
patFail loc msg = addFatalError $ mkPlainErrorMsgEnvelope loc $ msg

patIsRec :: RdrName -> Bool
patIsRec e = e == mkUnqual varName (fsLit "rec")

---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SrcSpan
            -> LocatedA (PatBuilder GhcPs)
            -> (HsMultAnn GhcPs, Maybe (EpUniToken "::" "∷", LHsType GhcPs))
            -> Located (GRHSs GhcPs (LHsExpr GhcPs))
            -> P (HsBind GhcPs)

checkValDef loc lhs (mult, Just (sigAnn, sig)) grhss
        -- x :: ty = rhs  parses as a *pattern* binding
  = do lhs' <- runPV $ mkHsTySigPV (combineLocsA lhs sig) lhs sig sigAnn
                        >>= checkLPat
       checkPatBind loc lhs' grhss mult

checkValDef loc lhs (mult_ann, Nothing) grhss
  | HsNoMultAnn{} <- mult_ann
  = do  { mb_fun <- isFunLhs lhs
        ; case mb_fun of
            Just (fun, is_infix, pats, ops, cps) -> do
              let ann_fun = mk_ann_funrhs ops cps
              let l = listLocation pats
              checkFunBind loc ann_fun
                           fun is_infix (L l pats) grhss
            Nothing -> do
              lhs' <- checkPattern lhs
              checkPatBind loc lhs' grhss mult_ann }

checkValDef loc lhs (mult_ann, Nothing) ghrss
        -- %p x = rhs  parses as a *pattern* binding
  = do lhs' <- checkPattern lhs
       checkPatBind loc lhs' ghrss mult_ann

mk_ann_funrhs :: [EpToken "("] -> [EpToken ")"] -> AnnFunRhs
mk_ann_funrhs ops cps = AnnFunRhs NoEpTok ops cps

checkFunBind :: SrcSpan
             -> AnnFunRhs
             -> LocatedN RdrName
             -> LexicalFixity
             -> LocatedE [LocatedA (ArgPatBuilder GhcPs)]
             -> Located (GRHSs GhcPs (LHsExpr GhcPs))
             -> P (HsBind GhcPs)
checkFunBind locF ann_fun (L lf fun) is_infix (L lp pats) (L _ grhss)
  = do  ps <- runPV_details extraDetails (mapM checkLArgPat pats)
        let match_span = noAnnSrcSpan $ locF
        return (makeFunBind (L (l2l lf) fun) (L (noAnnSrcSpan $ locA match_span)
                 [L match_span (Match { m_ext = noExtField
                                      , m_ctxt = FunRhs
                                          { mc_fun    = L lf fun
                                          , mc_fixity = is_infix
                                          , mc_strictness = NoSrcStrict
                                          , mc_an = ann_fun }
                                      , m_pats = L lp ps
                                      , m_grhss = grhss })]))
        -- The span of the match covers the entire equation.
        -- That isn't quite right, but it'll do for now.
  where
    extraDetails
      | Infix <- is_infix = ParseContext (Just fun) NoIncompleteDoBlock
      | otherwise         = noParseContext

makeFunBind :: LocatedN RdrName -> LocatedLW [LMatch GhcPs (LHsExpr GhcPs)]
            -> HsBind GhcPs
-- Like GHC.Hs.Utils.mkFunBind, but we need to be able to set the fixity too
makeFunBind fn ms
  = FunBind { fun_ext = noExtField,
              fun_id = fn,
              fun_matches = mkMatchGroup FromSource ms }

-- See Note [FunBind vs PatBind]
checkPatBind :: SrcSpan
             -> LPat GhcPs
             -> Located (GRHSs GhcPs (LHsExpr GhcPs))
             -> HsMultAnn GhcPs
             -> P (HsBind GhcPs)
checkPatBind loc (L _ (BangPat an (L _ (VarPat _ v))))
                        (L _match_span grhss) (HsNoMultAnn _)
      = return (makeFunBind v (L (noAnnSrcSpan loc)
                [L (noAnnSrcSpan loc) (m an v)]))
  where
    m a v = Match { m_ext = noExtField
                  , m_ctxt = FunRhs { mc_fun    = v
                                    , mc_fixity = Prefix
                                    , mc_strictness = SrcStrict
                                    , mc_an = AnnFunRhs a [] [] }
                  , m_pats = noLocA []
                 , m_grhss = grhss }

checkPatBind _loc lhs (L _ grhss) mult = do
  return (PatBind noExtField lhs mult grhss)


checkValSigLhs :: LHsExpr GhcPs -> P (LocatedN RdrName)
checkValSigLhs lhs@(L l lhs_expr) =
  case lhs_expr of
    HsVar _ lrdr@(L _ v) -> check_var v lrdr
    _                    -> make_err PsErrInvalidTypeSig_Other
  where
    check_var v lrdr
      | not (isUnqual v) = make_err PsErrInvalidTypeSig_Qualified
      | isDataOcc occ_n  = make_err PsErrInvalidTypeSig_DataCon
      | otherwise        = pure lrdr
      where occ_n = rdrNameOcc v
    make_err reason = addFatalError $
      mkPlainErrorMsgEnvelope (locA l) (PsErrInvalidTypeSignature reason lhs)


checkDoAndIfThenElse
  :: (Outputable a, Outputable b, Outputable c)
  => (a -> Bool -> b -> Bool -> c -> PsMessage)
  -> LocatedA a -> Bool -> LocatedA b -> Bool -> LocatedA c -> PV ()
checkDoAndIfThenElse err guardExpr semiThen thenExpr semiElse elseExpr
 | semiThen || semiElse = do
      doAndIfThenElse <- getBit DoAndIfThenElseBit
      let e   = err (unLoc guardExpr)
                    semiThen (unLoc thenExpr)
                    semiElse (unLoc elseExpr)
          loc = combineLocs (reLoc guardExpr) (reLoc elseExpr)

      unless doAndIfThenElse $ addError (mkPlainErrorMsgEnvelope loc e)
  | otherwise = return ()

isFunLhs :: LocatedA (PatBuilder GhcPs)
      -> P (Maybe (LocatedN RdrName, LexicalFixity,
                   [LocatedA (ArgPatBuilder GhcPs)],[EpToken "("],[EpToken ")"]))
-- A variable binding is parsed as a FunBind.
-- Just (fun, is_infix, arg_pats) if e is a function LHS
isFunLhs e = go e [] [] []
 where
   mk = fmap ArgPatBuilderVisPat

   go (L l (PatBuilderVar (L loc f))) es ops cps
       | not (isRdrDataCon f)        = do
           let (_l, loc') = transferCommentsOnlyA l loc
           return (Just (L loc' f, Prefix, es, (reverse ops), cps))
   go (L l (PatBuilderApp (L lf f) e))   es       ops cps = do
     let (_l, lf') = transferCommentsOnlyA l lf
     go (L lf' f) (mk e:es) ops cps
   go (L l (PatBuilderPar _ (L le e) _)) es@(_:_) ops cps = go (L le' e) es (o:ops) (c:cps)
      -- NB: es@(_:_) means that there must be an arg after the parens for the
      -- LHS to be a function LHS. This corresponds to the Haskell Report's definition
      -- of funlhs.
     where
       (_l, le') = transferCommentsOnlyA l le
       (o,c) = mkParensEpToks (realSrcSpan $ locA l)
   go (L loc (PatBuilderOpApp (L ll l) (L loc' op) r (os,cs))) es ops cps
      | not (isRdrDataCon op)         -- We have found the function!
      = do { let (_l, ll') = transferCommentsOnlyA loc ll
           ; return (Just (L loc' op, Infix, (mk (L ll' l):mk r:es), (os ++ reverse ops), (cs ++ cps))) }
      | otherwise                     -- Infix data con; keep going
      = do { let (_l, ll') = transferCommentsOnlyA loc ll
           ; mb_l <- go (L ll' l) es ops cps
           ; return (reassociate =<< mb_l) }
        where
          reassociate (op', Infix, j : L k_loc (ArgPatBuilderVisPat k) : es', ops', cps')
            = Just (op', Infix, j : op_app : es', ops', cps')
            where
              op_app = mk $ L loc (PatBuilderOpApp (L k_loc k)
                                    (L loc' op) r (reverse ops, cps))
          reassociate _other = Nothing
   go (L l (PatBuilderAppType (L lp pat) tok ty_pat@(HsTP _ (L (EpAnn anc ann cs) _)))) es ops cps
             = go (L lp' pat) (L (EpAnn anc' ann cs) (ArgPatBuilderArgPat invis_pat) : es) ops cps
             where invis_pat = InvisPat (tok, SpecifiedSpec) ty_pat
                   anc' = widenAnchorT anc tok
                   (_l, lp') = transferCommentsOnlyA l lp
   go _ _ _ _ = return Nothing

data ArgPatBuilder p
  = ArgPatBuilderVisPat (PatBuilder p)
  | ArgPatBuilderArgPat (Pat p)

instance Outputable (ArgPatBuilder GhcPs) where
  ppr (ArgPatBuilderVisPat p) = ppr p
  ppr (ArgPatBuilderArgPat p) = ppr p

mkBangTy :: EpaLocation -> SrcStrictness -> LHsType GhcPs -> HsType GhcPs
mkBangTy tok_loc strictness =
  HsBangTy ((noAnn, noAnn, tok_loc), NoSourceText) (HsBang NoSrcUnpack strictness)

-- | Result of parsing @{-\# UNPACK \#-}@ or @{-\# NOUNPACK \#-}@.
data UnpackednessPragma =
  UnpackednessPragma (EpaLocation, EpaLocation) SourceText SrcUnpackedness

-- | Annotate a type with either an @{-\# UNPACK \#-}@ or a @{-\# NOUNPACK \#-}@ pragma.
addUnpackednessP :: MonadP m => Located UnpackednessPragma -> LHsType GhcPs -> m (LHsType GhcPs)
addUnpackednessP (L lprag (UnpackednessPragma anns prag unpk)) ty = do
    let l' = combineSrcSpans lprag (getLocA ty)
    let t' = addUnpackedness anns ty
    return (L (noAnnSrcSpan l') t')
  where
    -- If we have a HsBangTy that only has a strictness annotation,
    -- such as ~T or !T, then add the pragma to the existing HsBangTy.
    --
    -- Otherwise, wrap the type in a new HsBangTy constructor.
    addUnpackedness (o,c) (L _ (HsBangTy ((_,_,tl), NoSourceText) bang t))
      | HsBang NoSrcUnpack strictness <- bang
      = HsBangTy ((o,c,tl), prag) (HsBang unpk strictness) t
    addUnpackedness (o,c) t
      = HsBangTy ((o,c,noAnn), prag) (HsBang unpk NoSrcStrict) t

---------------------------------------------------------------------------
-- | Check for monad comprehensions
--
-- If the flag MonadComprehensions is set, return a 'MonadComp' context,
-- otherwise use the usual 'ListComp' context

checkMonadComp :: PV HsDoFlavour
checkMonadComp = do
    monadComprehensions <- getBit MonadComprehensionsBit
    return $ if monadComprehensions
                then MonadComp
                else ListComp

-- -------------------------------------------------------------------------
-- Expression/command/pattern ambiguity.
-- See Note [Ambiguous syntactic categories]
--

-- See Note [Ambiguous syntactic categories]
--
-- This newtype is required to avoid impredicative types in monadic
-- productions. That is, in a production that looks like
--
--    | ... {% return (ECP ...) }
--
-- we are dealing with
--    P ECP
-- whereas without a newtype we would be dealing with
--    P (forall b. DisambECP b => PV (Located b))
--
newtype ECP =
  ECP { unECP :: forall b. DisambECP b => PV (LocatedA b) }

ecpFromExp :: LHsExpr GhcPs -> ECP
ecpFromExp a = ECP (ecpFromExp' a)

ecpFromCmd :: LHsCmd GhcPs -> ECP
ecpFromCmd a = ECP (ecpFromCmd' a)

ecpFromPat :: LPat GhcPs -> ECP
ecpFromPat a = ECP (ecpFromPat' a)

-- The 'fbinds' parser rule produces values of this type. See Note
-- [RecordDotSyntax field updates].
type Fbind b = Either (LHsRecField GhcPs (LocatedA b)) (LHsRecProj GhcPs (LocatedA b))

-- | Disambiguate infix operators.
-- See Note [Ambiguous syntactic categories]
class DisambInfixOp b where
  mkHsVarOpPV :: LocatedN RdrName -> PV (LocatedN b)
  mkHsConOpPV :: LocatedN RdrName -> PV (LocatedN b)
  mkHsInfixHolePV :: LocatedN (HsExpr GhcPs) -> PV (LocatedN b)

instance DisambInfixOp (HsExpr GhcPs) where
  mkHsVarOpPV v = return $ L (getLoc v) (HsVar noExtField v)
  mkHsConOpPV v = return $ L (getLoc v) (HsVar noExtField v)
  mkHsInfixHolePV h = return h

instance DisambInfixOp RdrName where
  mkHsConOpPV (L l v) = return $ L l v
  mkHsVarOpPV (L l v) = return $ L l v
  mkHsInfixHolePV (L l _) = addFatalError $ mkPlainErrorMsgEnvelope (getHasLoc l) $ PsErrInvalidInfixHole

type AnnoBody b
  = ( Anno (GRHS GhcPs (LocatedA (Body b GhcPs))) ~ EpAnnCO
    , Anno [LocatedA (Match GhcPs (LocatedA (Body b GhcPs)))] ~ SrcSpanAnnLW
    , Anno (Match GhcPs (LocatedA (Body b GhcPs))) ~ SrcSpanAnnA
    , Anno (StmtLR GhcPs GhcPs (LocatedA (Body (Body b GhcPs) GhcPs))) ~ SrcSpanAnnA
    , Anno [LocatedA (StmtLR GhcPs GhcPs
                       (LocatedA (Body (Body (Body b GhcPs) GhcPs) GhcPs)))] ~ SrcSpanAnnLW
    )

-- | Disambiguate constructs that may appear when we do not know ahead of time whether we are
-- parsing an expression, a command, or a pattern.
-- See Note [Ambiguous syntactic categories]
class (b ~ (Body b) GhcPs, AnnoBody b) => DisambECP b where
  -- | See Note [Body in DisambECP]
  type Body b :: Type -> Type
  -- | Return a command without ambiguity, or fail in a non-command context.
  ecpFromCmd' :: LHsCmd GhcPs -> PV (LocatedA b)
  -- | Return an expression without ambiguity, or fail in a non-expression context.
  ecpFromExp' :: LHsExpr GhcPs -> PV (LocatedA b)
  -- | Return a pattern without ambiguity, or fail in a non-pattern context.
  ecpFromPat' :: LPat GhcPs -> PV (LocatedA b)
  mkHsProjUpdatePV :: SrcSpan -> Located [LocatedAn NoEpAnns (DotFieldOcc GhcPs)]
    -> LocatedA b -> Bool -> Maybe (EpToken "=") -> PV (LHsRecProj GhcPs (LocatedA b))
  -- | Disambiguate "let ... in ..."
  mkHsLetPV
    :: SrcSpan
    -> EpToken "let"
    -> HsLocalBinds GhcPs
    -> EpToken "in"
    -> LocatedA b
    -> PV (LocatedA b)
  -- | Infix operator representation
  type InfixOp b
  -- | Bring superclass constraints on InfixOp into scope.
  -- See Note [UndecidableSuperClasses for associated types]
  superInfixOp
    :: (DisambInfixOp (InfixOp b) => PV (LocatedA b )) -> PV (LocatedA b)
  -- | Disambiguate "f # x" (infix operator)
  mkHsOpAppPV :: SrcSpan -> LocatedA b -> LocatedN (InfixOp b) -> LocatedA b
              -> PV (LocatedA b)
  -- | Disambiguate "case ... of ..."
  mkHsCasePV :: SrcSpan -> LHsExpr GhcPs -> (LocatedLW [LMatch GhcPs (LocatedA b)])
             -> EpAnnHsCase -> PV (LocatedA b)
  -- | Disambiguate "\... -> ..." (lambda), "\case" and "\cases"
  mkHsLamPV :: SrcSpan -> HsLamVariant
            -> (LocatedLW [LMatch GhcPs (LocatedA b)]) -> EpAnnLam
            -> PV (LocatedA b)
  -- | Function argument representation
  type FunArg b
  -- | Bring superclass constraints on FunArg into scope.
  -- See Note [UndecidableSuperClasses for associated types]
  superFunArg :: (DisambECP (FunArg b) => PV (LocatedA b)) -> PV (LocatedA b)
  -- | Disambiguate "f x" (function application)
  mkHsAppPV :: SrcSpanAnnA -> LocatedA b -> LocatedA (FunArg b) -> PV (LocatedA b)
  -- | Disambiguate "f @t" (visible type application)
  mkHsAppTypePV :: SrcSpanAnnA -> LocatedA b -> EpToken "@" -> LHsType GhcPs -> PV (LocatedA b)
  -- | Disambiguate "if ... then ... else ..."
  mkHsIfPV :: SrcSpan
         -> LHsExpr GhcPs
         -> Bool  -- semicolon?
         -> LocatedA b
         -> Bool  -- semicolon?
         -> LocatedA b
         -> AnnsIf
         -> PV (LocatedA b)
  -- | Disambiguate "do { ... }" (do notation)
  mkHsDoPV ::
    SrcSpan ->
    Maybe ModuleName ->
    LocatedLW [LStmt GhcPs (LocatedA b)] ->
    EpaLocation -> -- Token
    EpaLocation -> -- Anchor
    PV (LocatedA b)
  -- | Disambiguate "( ... )" (parentheses)
  mkHsParPV :: SrcSpan -> EpToken "(" -> LocatedA b -> EpToken ")" -> PV (LocatedA b)
  -- | Disambiguate a variable "f" or a data constructor "MkF".
  mkHsVarPV :: LocatedN RdrName -> PV (LocatedA b)
  -- | Disambiguate a monomorphic literal
  mkHsLitPV :: Located (HsLit GhcPs) -> PV (LocatedA b)
  -- | Disambiguate an overloaded literal
  mkHsOverLitPV :: LocatedAn a (HsOverLit GhcPs) -> PV (LocatedAn a b)
  -- | Disambiguate a wildcard
  mkHsWildCardPV :: (NoAnn a) => SrcSpan -> PV (LocatedAn a b)
  -- | Disambiguate "a :: t" (type annotation)
  mkHsTySigPV
    :: SrcSpanAnnA -> LocatedA b -> LHsType GhcPs -> EpUniToken "::" "∷" -> PV (LocatedA b)
  -- | Disambiguate "[a,b,c]" (list syntax)
  mkHsExplicitListPV :: SrcSpan -> [LocatedA b] -> AnnList () -> PV (LocatedA b)
  -- | Disambiguate "$(...)" and "[quasi|...|]" (TH splices)
  mkHsSplicePV :: Located (HsUntypedSplice GhcPs) -> PV (LocatedA b)
  -- | Disambiguate "f { a = b, ... }" syntax (record construction and record updates)
  mkHsRecordPV ::
    Bool -> -- Is OverloadedRecordUpdate in effect?
    SrcSpan ->
    SrcSpan ->
    LocatedA b ->
    ([Fbind b], Maybe SrcSpan) ->
    (Maybe (EpToken "{"), Maybe (EpToken "}")) ->
    PV (LocatedA b)
  -- | Disambiguate "-a" (negation)
  mkHsNegAppPV :: SrcSpan -> LocatedA b -> EpToken "-" -> PV (LocatedA b)
  -- | Disambiguate "(# a)" (right operator section)
  mkHsSectionR_PV
    :: SrcSpan -> LocatedA (InfixOp b) -> LocatedA b -> PV (LocatedA b)
  -- | Disambiguate "(a -> b)" (view pattern or function type arrow)
  mkHsArrowPV
    :: SrcSpan -> ArrowParsingMode lhs b -> LocatedA lhs -> HsArrowOf (LocatedA b) GhcPs -> LocatedA b -> PV (LocatedA b)
  -- | Disambiguate "%m" to the left of "->" (multiplicity)
  mkHsMultPV
    :: Located [HsModifierOf (LocatedA b) GhcPs] -> EpUniToken "->" "→" -> PV (HsArrowOf (LocatedA b) GhcPs)
  -- | Disambiguate "forall a. b" and "forall a -> b" (forall telescope)
  mkHsForallPV :: SrcSpan -> HsForAllTelescope GhcPs -> LocatedA b -> PV (LocatedA b)
  -- | Disambiguate "(a,b,c)" to the left of "=>" (constraint list)
  checkContextPV :: LocatedA b -> PV (LocatedC [LocatedA b])
  -- | Disambiguate "a => b" (constraint context)
  mkQualPV :: SrcSpan -> LocatedC [LocatedA b] -> LocatedA b -> PV (LocatedA b)
  -- | Disambiguate "a@b" (as-pattern)
  mkHsAsPatPV
    :: SrcSpan -> LocatedN RdrName -> EpToken "@" -> LocatedA b -> PV (LocatedA b)
  -- | Disambiguate "~a" (lazy pattern)
  mkHsLazyPatPV :: SrcSpan -> LocatedA b -> EpToken "~" -> PV (LocatedA b)
  -- | Disambiguate "!a" (bang pattern)
  mkHsBangPatPV :: SrcSpan -> LocatedA b -> EpToken "!" -> PV (LocatedA b)
  -- | Disambiguate tuple sections and unboxed sums
  mkSumOrTuplePV
    :: SrcSpanAnnA -> Boxity -> SumOrTuple b -> (EpaLocation, EpaLocation) -> PV (LocatedA b)
  -- | Disambiguate "type t" (embedded type)
  mkHsEmbTyPV :: SrcSpan -> EpToken "type" -> LHsType GhcPs -> PV (LocatedA b)
  -- | Validate infixexp LHS to reject unwanted {-# SCC ... #-} pragmas
  rejectPragmaPV :: LocatedA b -> PV ()

{- Note [UndecidableSuperClasses for associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(This Note is about the code in GHC, not about the user code that we are parsing)

Assume we have a class C with an associated type T:

  class C a where
    type T a
    ...

If we want to add 'C (T a)' as a superclass, we need -XUndecidableSuperClasses:

  {-# LANGUAGE UndecidableSuperClasses #-}
  class C (T a) => C a where
    type T a
    ...

Unfortunately, -XUndecidableSuperClasses don't work all that well, sometimes
making GHC loop. The workaround is to bring this constraint into scope
manually with a helper method:

  class C a where
    type T a
    superT :: (C (T a) => r) -> r

In order to avoid ambiguous types, 'r' must mention 'a'.

For consistency, we use this approach for all constraints on associated types,
even when -XUndecidableSuperClasses are not required.
-}

{- Note [Body in DisambECP]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are helper functions (mkBodyStmt, mkBindStmt, unguardedRHS, etc) that
require their argument to take a form of (body GhcPs) for some (body :: Type ->
*). To satisfy this requirement, we say that (b ~ Body b GhcPs) in the
superclass constraints of DisambECP.

The alternative is to change mkBodyStmt, mkBindStmt, unguardedRHS, etc, to drop
this requirement. It is possible and would allow removing the type index of
PatBuilder, but leads to worse type inference, breaking some code in the
typechecker.
-}

instance DisambECP (HsCmd GhcPs) where
  type Body (HsCmd GhcPs) = HsCmd
  ecpFromCmd' = return
  ecpFromExp' (L l e) = cmdFail (locA l) (ppr e)
  ecpFromPat' (L l p) = cmdFail (locA l) (ppr p)
  mkHsProjUpdatePV l _ _ _ _ = addFatalError $ mkPlainErrorMsgEnvelope l $
                                                 PsErrOverloadedRecordDotInvalid
  mkHsLamPV l lam_variant (L lm m) anns = do
    !cs <- getCommentsFor l
    let mg = mkLamCaseMatchGroup FromSource lam_variant (L lm m)
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsCmdLam anns lam_variant mg)

  mkHsLetPV l tkLet bs tkIn e = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsCmdLet (tkLet, tkIn) bs e)

  type InfixOp (HsCmd GhcPs) = HsExpr GhcPs

  superInfixOp m = m

  mkHsOpAppPV l c1 op c2 = do
    let cmdArg c = L (l2l $ getLoc c) $ HsCmdTop noExtField c
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) $ HsCmdArrForm noAnn (reLoc op) Infix [cmdArg c1, cmdArg c2]

  mkHsCasePV l c (L lm m) anns = do
    !cs <- getCommentsFor l
    let mg = mkMatchGroup FromSource (L lm m)
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsCmdCase anns c mg)

  type FunArg (HsCmd GhcPs) = HsExpr GhcPs
  superFunArg m = m
  mkHsAppPV l c e = do
    checkCmdBlockArguments c
    checkExpBlockArguments e
    return $ L l (HsCmdApp noExtField c e)
  mkHsAppTypePV l c _ t = cmdFail (locA l) (ppr c <+> text "@" <> ppr t)
  mkHsIfPV l c semi1 a semi2 b anns = do
    checkDoAndIfThenElse PsErrSemiColonsInCondCmd c semi1 a semi2 b
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (mkHsCmdIf c a b anns)
  mkHsDoPV l Nothing stmts tok_loc anc = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsCmdDo (AnnList (Just anc) Nothing Nothing [] tok_loc []) stmts)
  mkHsDoPV l (Just m)    _ _ _ = addFatalError $ mkPlainErrorMsgEnvelope l $ PsErrQualifiedDoInCmd m
  mkHsParPV l lpar c rpar = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsCmdPar (lpar, rpar) c)
  mkHsVarPV (L l v) = cmdFail (locA l) (ppr v)
  mkHsLitPV (L l a) = cmdFail l (ppr a)
  mkHsOverLitPV (L l a) = cmdFail (locA l) (ppr a)
  mkHsWildCardPV l = cmdFail l (text "_")
  mkHsTySigPV l a sig _ = cmdFail (locA l) (ppr a <+> text "::" <+> ppr sig)
  mkHsExplicitListPV l xs _ = cmdFail l $
    brackets (pprWithCommas ppr xs)
  mkHsSplicePV (L l sp) = cmdFail l (pprUntypedSplice True Nothing sp)
  mkHsRecordPV _ l _ a (fbinds, ddLoc) _ = do
    let (fs, ps) = partitionEithers fbinds
    if not (null ps)
      then addFatalError $ mkPlainErrorMsgEnvelope l $ PsErrOverloadedRecordDotInvalid
      else cmdFail l $ ppr a <+> ppr (mk_rec_fields fs ddLoc)
  mkHsNegAppPV l a _ = cmdFail l (text "-" <> ppr a)
  mkHsSectionR_PV l op c = cmdFail l $
    let pp_op = fromMaybe (panic "cannot print infix operator")
                          (ppr_infix_expr (unLoc op))
    in pp_op <> ppr c
  mkHsArrowPV l mode a arr b = cmdFail l $
    case mode of  -- matching on the mode brings Outputable instances into scope
      ArrowIsViewPat -> ppr a <+> pprHsArrow arr <+> ppr b
      ArrowIsFunType -> ppr a <+> pprHsArrow arr <+> ppr b
  mkHsMultPV lMods tok = case unLoc lMods of
    [] -> pure $ HsUnrestrictedArrow tok
    mods -> cmdFail (getLoc lMods) $ ppr mods
  mkHsForallPV l tele cmd = cmdFail l $
    pprHsForAll tele Nothing <+> ppr cmd
  checkContextPV ctxt = cmdFail (getLocA ctxt) $ ppr ctxt
  mkQualPV l ctxt cmd = cmdFail l $
    ppr ctxt <+> text "=>" <+> ppr cmd
  mkHsAsPatPV l v _ c = cmdFail l $
    pprPrefixOcc (unLoc v) <> text "@" <> ppr c
  mkHsLazyPatPV l c _ = cmdFail l $
    text "~" <> ppr c
  mkHsBangPatPV l c _ = cmdFail l $
    text "!" <> ppr c
  mkSumOrTuplePV l boxity a _ = cmdFail (locA l) (pprSumOrTuple boxity a)
  mkHsEmbTyPV l _ ty = cmdFail l (text "type" <+> ppr ty)
  rejectPragmaPV _ = return ()

cmdFail :: SrcSpan -> SDoc -> PV a
cmdFail loc e = addFatalError $ mkPlainErrorMsgEnvelope loc $ PsErrParseErrorInCmd e

checkLamMatchGroup :: SrcSpan -> HsLamVariant -> MatchGroup GhcPs (LHsExpr GhcPs) -> PV ()
checkLamMatchGroup l LamSingle (MG { mg_alts = (L _ (matches:_))}) = do
  when (null (hsLMatchPats matches)) $ addError $ mkPlainErrorMsgEnvelope l PsErrEmptyLambda
checkLamMatchGroup _ _ _ = return ()

instance DisambECP (HsExpr GhcPs) where
  type Body (HsExpr GhcPs) = HsExpr
  ecpFromCmd' (L l c) = do
    addError $ mkPlainErrorMsgEnvelope (locA l) $ PsErrArrowCmdInExpr c
    return (L l (hsHoleExpr noAnn))
  ecpFromExp' = return
  ecpFromPat' p@(L l _) = do
    addError $ mkPlainErrorMsgEnvelope (locA l) $ PsErrOrPatInExpr p
    return (L l (hsHoleExpr noAnn))
  mkHsProjUpdatePV l fields arg isPun anns = do
    !cs <- getCommentsFor l
    return $ mkRdrProjUpdate (EpAnn (spanAsAnchor l) noAnn cs) fields arg isPun anns
  mkHsLetPV l tkLet bs tkIn c = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsLet (tkLet, tkIn) bs c)
  type InfixOp (HsExpr GhcPs) = HsExpr GhcPs
  superInfixOp m = m
  mkHsOpAppPV l e1 op e2 = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) $ OpApp noExtField e1 (reLoc op) e2
  mkHsCasePV l e (L lm m) anns = do
    !cs <- getCommentsFor l
    let mg = mkMatchGroup FromSource (L lm m)
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsCase anns e mg)
  mkHsLamPV l lam_variant (L lm m) anns = do
    !cs <- getCommentsFor l
    let mg = mkLamCaseMatchGroup FromSource lam_variant (L lm m)
    checkLamMatchGroup l lam_variant mg
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsLam anns lam_variant mg)
  type FunArg (HsExpr GhcPs) = HsExpr GhcPs
  superFunArg m = m
  mkHsAppPV l e1 e2 = do
    checkExpBlockArguments e1
    checkExpBlockArguments e2
    return $ L l (HsApp noExtField e1 e2)
  mkHsAppTypePV l e at t = do
    checkExpBlockArguments e
    return $ L l (HsAppType at e (mkHsWildCardBndrs t))
  mkHsIfPV l c semi1 a semi2 b anns = do
    checkDoAndIfThenElse PsErrSemiColonsInCondExpr c semi1 a semi2 b
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (mkHsIf c a b anns)
  mkHsDoPV l mod stmts loc_tok anc = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsDo (AnnList (Just anc) Nothing Nothing [] loc_tok []) (DoExpr mod) stmts)
  mkHsParPV l lpar e rpar = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsPar (lpar, rpar) e)
  mkHsVarPV v@(L l@(EpAnn anc _ _) _) = do
    !cs <- getCommentsFor (getHasLoc l)
    return $ L (EpAnn anc noAnn cs) (HsVar noExtField v)
  mkHsLitPV (L l a) = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (HsLit noExtField a)
  mkHsOverLitPV (L (EpAnn l an csIn) a) = do
    !cs <- getCommentsFor (locA l)
    return $ L (EpAnn  l an (cs Semi.<> csIn)) (HsOverLit NoExtField a)
  mkHsWildCardPV l = return $ L (noAnnSrcSpan l) (hsHoleExpr noAnn)
  mkHsTySigPV l@(EpAnn anc an csIn) a sig anns = do
    !cs <- getCommentsFor (locA l)
    return $ L (EpAnn anc an (csIn Semi.<> cs)) (ExprWithTySig anns a (hsTypeToHsSigWcType sig))
  mkHsExplicitListPV l xs anns = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (ExplicitList anns xs)
  mkHsSplicePV (L l a) = do
    !cs <- getCommentsFor l
    return $ fmap (HsUntypedSplice NoExtField) (L (EpAnn (spanAsAnchor l) noAnn cs) a)
  mkHsRecordPV opts l lrec a (fbinds, ddLoc) anns = do
    !cs <- getCommentsFor l
    r <- mkRecConstrOrUpdate opts a lrec (fbinds, ddLoc) anns
    checkRecordSyntax (L (EpAnn (spanAsAnchor l) noAnn cs) r)
  mkHsNegAppPV l a anns = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (NegApp anns a noSyntaxExpr)
  mkHsSectionR_PV l op e = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (SectionR noExtField op e)
  mkHsAsPatPV l v _ e   = addError (mkPlainErrorMsgEnvelope l $ PsErrTypeAppWithoutSpace (unLoc v) e)
                          >> return (L (noAnnSrcSpan l) (hsHoleExpr noAnn))
  mkHsLazyPatPV l e   _ = addError (mkPlainErrorMsgEnvelope l $ PsErrLazyPatWithoutSpace e)
                          >> return (L (noAnnSrcSpan l) (hsHoleExpr noAnn))
  mkHsBangPatPV l e   _ = addError (mkPlainErrorMsgEnvelope l $ PsErrBangPatWithoutSpace e)
                          >> return (L (noAnnSrcSpan l) (hsHoleExpr noAnn))
  mkSumOrTuplePV = mkSumOrTupleExpr
  mkHsEmbTyPV l toktype ty =
    return $ L (noAnnSrcSpan l) $
      HsEmbTy toktype (mkHsWildCardBndrs ty)
  mkHsArrowPV l mode arg arr res =
    -- In expressions, (e1 -> e2) is always parsed as a function type,
    -- even if ViewPatterns are enabled.
    exprArrowParsingMode mode $
    return $ L (noAnnSrcSpan l) $
      HsFunArr noExtField arr arg res
  mkHsMultPV lMods tok = return $ case unLoc lMods of
    [] -> HsUnrestrictedArrow tok
    mods -> HsExplicitMult tok mods
  mkHsForallPV l telescope ty =
    return $ L (noAnnSrcSpan l) $
      HsForAll noExtField (setTelescopeBndrsNameSpace varName telescope) ty
  checkContextPV = checkContextExpr
  mkQualPV l qual ty =
    return $ L (noAnnSrcSpan l) $
      HsQual noExtField qual ty
  rejectPragmaPV (L _ (OpApp _ _ _ e)) =
    -- assuming left-associative parsing of operators
    rejectPragmaPV e
  rejectPragmaPV (L l (HsPragE _ prag _)) = addError $ mkPlainErrorMsgEnvelope (locA l) $
                                                         (PsErrUnallowedPragma prag)
  rejectPragmaPV _                        = return ()

hsHoleExpr :: Maybe EpAnnUnboundVar -> HsExpr GhcPs
hsHoleExpr anns = HsUnboundVar anns (mkRdrUnqual (mkVarOccFS (fsLit "_")))

instance DisambECP (PatBuilder GhcPs) where
  type Body (PatBuilder GhcPs) = PatBuilder
  ecpFromCmd' (L l c)    = addFatalError $ mkPlainErrorMsgEnvelope (locA l) $ PsErrArrowCmdInPat c
  ecpFromExp' (L l e)    = addFatalError $ mkPlainErrorMsgEnvelope (locA l) $ PsErrArrowExprInPat e
  ecpFromPat' (L l p)    = return $ L l (PatBuilderPat p)
  mkHsLetPV l _ _ _ _    = addFatalError $ mkPlainErrorMsgEnvelope l PsErrLetInPat
  mkHsProjUpdatePV l _ _ _ _ = addFatalError $ mkPlainErrorMsgEnvelope l PsErrOverloadedRecordDotInvalid
  type InfixOp (PatBuilder GhcPs) = RdrName
  superInfixOp m = m
  mkHsOpAppPV l p1 op p2 = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) $ PatBuilderOpApp p1 op p2 ([],[])

  mkHsLamPV l lam_variant _ _     = addFatalError $ mkPlainErrorMsgEnvelope l (PsErrLambdaInPat lam_variant)

  mkHsCasePV l _ _ _ = addFatalError $ mkPlainErrorMsgEnvelope l PsErrCaseInPat
  type FunArg (PatBuilder GhcPs) = PatBuilder GhcPs
  superFunArg m = m
  mkHsAppPV l p1 p2      = return $ L l (PatBuilderApp p1 p2)
  mkHsAppTypePV l p at t = do
    !cs <- getCommentsFor (locA l)
    return $ L (addCommentsToEpAnn l cs) (PatBuilderAppType p at (mkHsTyPat t))
  mkHsIfPV l _ _ _ _ _ _ = addFatalError $ mkPlainErrorMsgEnvelope l PsErrIfThenElseInPat
  mkHsDoPV l _ _ _ _    = addFatalError $ mkPlainErrorMsgEnvelope l PsErrDoNotationInPat
  mkHsParPV l lpar p rpar   = return $ L (noAnnSrcSpan l) (PatBuilderPar lpar p rpar)
  mkHsVarPV v@(getLoc -> l) = return $ L (l2l l) (PatBuilderVar v)
  mkHsLitPV lit@(L l a) = do
    checkUnboxedLitPat lit
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (LitPat noExtField a))
  mkHsOverLitPV (L l a) = return $ L l (PatBuilderOverLit a)
  mkHsWildCardPV l = return $ L (noAnnSrcSpan l) (PatBuilderPat (WildPat noExtField))
  mkHsTySigPV l p t anns = do
    p' <- checkLPat p
    let sig = mkHsPatSigType noAnn t
    sig_pat <- addSigPatP l p' sig anns
    return $ fmap PatBuilderPat sig_pat
  mkHsExplicitListPV l xs anns = do
    ps <- traverse checkLPat xs
    !cs <- getCommentsFor l
    return (L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (ListPat anns ps)))
  mkHsSplicePV (L l sp) = do
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (SplicePat noExtField sp))
  mkHsRecordPV _ l _ a (fbinds, ddLoc) anns = do
    let (fs, ps) = partitionEithers fbinds
    if not (null ps)
     then addFatalError $ mkPlainErrorMsgEnvelope l PsErrOverloadedRecordDotInvalid
     else do
       !cs <- getCommentsFor l
       r <- mkPatRec a (mk_rec_fields fs ddLoc) anns
       checkRecordSyntax (L (EpAnn (spanAsAnchor l) noAnn cs) r)
  mkHsNegAppPV l (L lp p) anns = do
    lit <- case p of
      PatBuilderOverLit pos_lit -> return (L (l2l lp) pos_lit)
      _ -> patFail l $ PsErrInPat p PEIP_NegApp
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (mkNPat lit (Just noSyntaxExpr) anns))
  mkHsSectionR_PV l op p = patFail l (PsErrParseRightOpSectionInPat (unLoc op) (unLoc p))
  mkHsArrowPV l ArrowIsViewPat a arr b = do
      p <- checkLPat b
      !cs <- getCommentsFor l
      return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (ViewPat tok a p))
    where
      tok :: EpUniToken "->" "→"
      tok = case arr of
        HsUnrestrictedArrow x -> x
        _ -> -- unreachable case because in Parser.y the reduction rules for
             -- (a %m -> b) and (a ->. b) use ArrowIsFunType
             panic "mkHsArrowPV ArrowIsViewPat: expected HsUnrestrictedArrow"
  mkHsArrowPV l ArrowIsFunType a arr b =
    patFail l (PsErrTypeSyntaxInPat (PETS_FunctionArrow a arr b))
  mkHsMultPV lMods tok = case unLoc lMods of
    [] -> pure $ HsUnrestrictedArrow tok
    mods -> patFail (getLoc lMods) $ PsErrTypeSyntaxInPat $ PETS_Multiplicity mods
  mkHsForallPV l tele body = patFail l (PsErrTypeSyntaxInPat (PETS_ForallTelescope tele body))
  checkContextPV ctx = patFail (getLocA ctx) (PsErrTypeSyntaxInPat (PETS_ConstraintContext ctx))
  mkQualPV _ _ _ =  -- unreachable because mkQualPV is only called on the result
                    -- of checkContextPV, which fails in patterns
                  panic "mkQualPV in a pattern"
  mkHsAsPatPV l v at e = do
    p <- checkLPat e
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (AsPat at v p))
  mkHsLazyPatPV l e a = do
    p <- checkLPat e
    !cs <- getCommentsFor l
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat (LazyPat a p))
  mkHsBangPatPV l e an = do
    p <- checkLPat e
    !cs <- getCommentsFor l
    let pb = BangPat an p
    hintBangPat l pb
    return $ L (EpAnn (spanAsAnchor l) noAnn cs) (PatBuilderPat pb)
  mkSumOrTuplePV = mkSumOrTuplePat
  mkHsEmbTyPV l toktype ty =
    return $ L (noAnnSrcSpan l) $
      PatBuilderPat (EmbTyPat toktype (mkHsTyPat ty))
  rejectPragmaPV _ = return ()

-- For reasons of backwards compatibility, we can't simply add the pattern
-- signature if the inner pattern is a view pattern. Consider:
--      (f -> p :: t)
-- There are two ways to parse it
--      (f -> (p :: t))   -- legacy parse
--      ((f -> p) :: t)   -- future parse
-- The grammar in Parser.y is structured in such a way that we get the
-- future parse by default. Until we're ready to make the breaking change,
-- we need to do some extra work here to push the signature under the view
-- pattern (and emit a warning).
addSigPatP :: SrcSpanAnnA -> LPat GhcPs -> HsPatSigType GhcPs -> EpUniToken "::" "∷" -> PV (LPat GhcPs)
addSigPatP l viewpat@(L _ ViewPat{}) sig anns =
  -- Test case: T24159_viewpat
  do { let futureParse = L l (SigPat anns viewpat sig)
     ; legacyParse <- go viewpat
     ; addPsMessage (locA l) (PsWarnViewPatternSignatures legacyParse futureParse)
     ; return legacyParse }
  where
    sig_loc_no_comments :: SrcSpan
    sig_loc_no_comments = getLocA (hsps_body sig)

    -- Test case for comments and locations preservation: Test24159
    go :: LPat GhcPs -> PV (LPat GhcPs)
    go  (L (EpAnn (EpaSpan view_pat_loc) anns cs1) (ViewPat anns' e' p')) = do
      sig' <- go  p'
      let new_loc = view_pat_loc `combineSrcSpans` sig_loc_no_comments
      cs2 <- getCommentsFor new_loc
      let ep_ann_loc = EpAnn (spanAsAnchor new_loc) anns (cs1 Semi.<> cs2)
      pure (L ep_ann_loc (ViewPat anns' e' sig'))

    go p = pure $ L new_loc (SigPat anns p sig)
      where new_loc = noAnnSrcSpan ((getLocA p) `combineSrcSpans` sig_loc_no_comments)

addSigPatP l p sig anns = do
  return $ L l (SigPat anns p sig)

{- Note [Arrow parsing mode]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this example:
  f (K (a -> b)) = ()

A pattern of the form (a -> b) could be parsed in one of two ways:
  * a view pattern `viewfn -> pat` (with ViewPatterns)
  * a function type `t1 -> t2`     (with RequiredTypeArguments)

This depends on the enabled extensions:
  NoViewPatterns, RequiredTypeArguments     =>  function type
  NoViewPatterns, NoRequiredTypeArguments   =>  error (suggest ViewPatterns)
  ViewPatterns,   RequiredTypeArguments     =>  view pattern
  ViewPatterns,   NoRequiredTypeArguments   =>  view pattern

The decision how to parse arrow patterns (p1 -> p2) is captured by the
`ArrowParsingMode` data type, produced in `withArrowParsingMode` and
consumed in `mkHsArrowPV`.

Naively, one might expect to see the following definition:

  -- a simple (but insufficient) definition
  data ArrowParsingMode = ArrowIsViewPat | ArrowIsFunType

However, there is a slight complication that leads us to parameterize these
constructor with GADT type indices. In a pattern (p1 -> p2), what is the AST
type to represent the LHS `p1`? It depends:
  * if (p1 -> p2) is a view pattern,  `p1` is an HsExpr
  * if (p1 -> p2) is a function type, `p1` is a  Pat (PatBuilder)

And since the decision how to parse `p1` depends on the arrow parsing mode, we
could try to encode the LHS type as a GADT index:

  -- a less simple (but still insufficient) definition
  data ArrowParsingMode lhs where
    ArrowIsViewPat :: ArrowParsingMode (PatBuilder GhcPs)
    ArrowIsFunType :: ArrowParsingMode (HsExpr GhcPs)

This definition would suffice for parsing patterns, but remember that
expressions, commands, and patterns are all parsed using a unified framework
`DisambECP`, as described in Note [Ambiguous syntactic categories].

In an expression (e1 -> e2), the LHS is always represented by an HsExpr.
We can account for this with a further refinement of the definition:

  -- actual definition
  data ArrowParsingMode lhs rhs where
    ArrowIsViewPat :: ArrowParsingMode (HsExpr GhcPs) b
    ArrowIsFunType :: ArrowParsingMode b b

So when parsing a view pattern, the LHS is an HsExpr; and when parsing a
function type, the type of the LHS is assumed to match the type of the RHS,
which works out just right both for expressions and patterns.
-}

-- The arrow parsing mode is selected depending on the enabled extensions and
-- determines how we parse patterns of the form (p1 -> p2). See Note [Arrow parsing mode]
data ArrowParsingMode lhs rhs where
  ArrowIsViewPat :: ArrowParsingMode (HsExpr GhcPs) b   -- the LHS is always of type HsExpr
  ArrowIsFunType :: ArrowParsingMode b b                -- the LHS is of the same type as RHS

-- When parsing an expression (e1 -> e2), the LHS `e1` is an HsExpr regardless of
-- the arrow parsing mode. `exprArrowParsingMode` proves this to the type checker.
-- See Note [Arrow parsing mode]
exprArrowParsingMode :: ArrowParsingMode lhs (HsExpr GhcPs) -> (lhs ~ HsExpr GhcPs => r) -> r
exprArrowParsingMode ArrowIsViewPat k = k
exprArrowParsingMode ArrowIsFunType k = k

-- Check the enabled extensions and select the appropriate ArrowParsingMode,
-- then pass it to a continuation. See Note [Arrow parsing mode]
withArrowParsingMode :: DisambECP b => (forall lhs. DisambECP lhs => ArrowParsingMode lhs b -> PV r) -> PV r
withArrowParsingMode cont = do
  vpEnabled <- getBit ViewPatternsBit
  rtaEnabled <- getBit RequiredTypeArgumentsBit
  if | vpEnabled  -> cont ArrowIsViewPat
     | rtaEnabled -> cont ArrowIsFunType
     | otherwise  -> cont ArrowIsViewPat -- Error message should suggest ViewPatterns in patterns

-- Type-restricted variant of `withArrowParsingMode` to aid type inference (#25103)
withArrowParsingMode' :: DisambECP b => (forall lhs. DisambECP lhs => ArrowParsingMode lhs b -> PV (LocatedA b)) -> PV (LocatedA b)
withArrowParsingMode' = withArrowParsingMode

-- When a forall-type occurs in term syntax, forall-bound variables should
-- inhabit the term namespace `varName` rather than the usual `tvName`.
-- See Note [Types in terms].
--
-- Since type variable binders in a `HsForAllTelescope` produced by the
-- `forall_telescope` nonterminal have their namespaces set to `tvName`,
-- we use `setTelescopeBndrsNameSpace` to fix them up.
setTelescopeBndrsNameSpace :: NameSpace -> HsForAllTelescope GhcPs -> HsForAllTelescope GhcPs
setTelescopeBndrsNameSpace ns forall_telescope =
  case forall_telescope of
    HsForAllInvis x bndrs -> HsForAllInvis x (set_bndrs_ns bndrs)
    HsForAllVis   x bndrs -> HsForAllVis   x (set_bndrs_ns bndrs)
  where
    set_bndrs_ns :: [LHsTyVarBndr flag GhcPs] -> [LHsTyVarBndr flag GhcPs]
    set_bndrs_ns = map (setLHsTyVarBndrNameSpace ns)

setLHsTyVarBndrNameSpace :: NameSpace -> LHsTyVarBndr flag GhcPs -> LHsTyVarBndr flag GhcPs
setLHsTyVarBndrNameSpace ns (L l tvb) = L l tvb'
  where tvb' = tvb { tvb_var = setHsBndrVarNameSpace ns (tvb_var tvb) }

setHsBndrVarNameSpace :: NameSpace -> HsBndrVar GhcPs -> HsBndrVar GhcPs
setHsBndrVarNameSpace ns (HsBndrVar x (L l rdr)) = HsBndrVar x (L l rdr')
  where rdr' = setRdrNameSpace rdr ns
setHsBndrVarNameSpace _ (HsBndrWildCard x) = HsBndrWildCard x

-- | Ensure that a literal pattern isn't of type Addr#, Float#, Double#.
checkUnboxedLitPat :: Located (HsLit GhcPs) -> PV ()
checkUnboxedLitPat (L loc lit) =
  case lit of
    -- Don't allow primitive string literal patterns.
    -- See #13260.
    HsStringPrim {}
      -> addError $ mkPlainErrorMsgEnvelope loc $
                           (PsErrIllegalUnboxedStringInPat lit)

   -- Don't allow Float#/Double# literal patterns.
   -- See #9238 and Note [Rules for floating-point comparisons]
   -- in GHC.Core.Opt.ConstantFold.
    _ | is_floating_lit lit
      -> addError $ mkPlainErrorMsgEnvelope loc $
                           (PsErrIllegalUnboxedFloatingLitInPat lit)

      | otherwise
      -> return ()

  where
    is_floating_lit :: HsLit GhcPs -> Bool
    is_floating_lit (HsFloatPrim  {}) = True
    is_floating_lit (HsDoublePrim {}) = True
    is_floating_lit _                 = False

mkPatRec ::
  LocatedA (PatBuilder GhcPs) ->
  HsRecFields GhcPs (LocatedA (PatBuilder GhcPs)) ->
  (Maybe (EpToken "{"), Maybe (EpToken "}")) ->
  PV (PatBuilder GhcPs)
mkPatRec (unLoc -> PatBuilderVar c) (HsRecFields x fs dd) anns
  | isRdrDataCon (unLoc c)
  = do fs <- mapM checkPatField fs
       return $ PatBuilderPat $ ConPat
         { pat_con_ext = anns
         , pat_con = c
         , pat_args = RecCon (HsRecFields x fs dd)
         }
mkPatRec p _ _ =
  addFatalError $ mkPlainErrorMsgEnvelope (getLocA p) $
                    (PsErrInvalidRecordCon (unLoc p))

-- | Disambiguate constructs that may appear when we do not know
-- ahead of time whether we are parsing a type or a newtype/data constructor.
--
-- See Note [Ambiguous syntactic categories] for the general idea.
--
-- See Note [Parsing data constructors is hard] for the specific issue this
-- particular class is solving.
--
class DisambTD b where
  -- | Process the head of a type-level function/constructor application,
  -- i.e. the @H@ in @H a b c@.
  mkHsAppTyHeadPV :: LHsType GhcPs -> PV (LocatedA b)
  -- | Disambiguate @f x@ (function application or prefix data constructor).
  mkHsAppTyPV :: LocatedA b -> LHsType GhcPs -> PV (LocatedA b)
  -- | Disambiguate @f \@t@ (visible kind application)
  mkHsAppKindTyPV :: LocatedA b -> EpToken "@" -> LHsType GhcPs -> PV (LocatedA b)
  -- | Disambiguate @f \# x@ (infix operator)
  mkHsOpTyPV :: PromotionFlag -> LHsType GhcPs -> LocatedN RdrName -> LHsType GhcPs -> PV (LocatedA b)
  -- | Disambiguate @{-\# UNPACK \#-} t@ (unpack/nounpack pragma)
  mkUnpackednessPV :: Located UnpackednessPragma -> LocatedA b -> PV (LocatedA b)

instance DisambTD (HsType GhcPs) where
  mkHsAppTyHeadPV = return
  mkHsAppTyPV t1 t2 = return (mkHsAppTy t1 t2)
  mkHsAppKindTyPV t at ki = return (mkHsAppKindTy at t ki)
  mkHsOpTyPV prom t1 op t2 = do
    let (L l ty) = mkLHsOpTy prom t1 op t2
    !cs <- getCommentsFor (locA l)
    return (L (addCommentsToEpAnn l cs) ty)
  mkUnpackednessPV = addUnpackednessP

dataConBuilderCon :: LocatedA DataConBuilder -> LocatedN RdrName
dataConBuilderCon (L _ (PrefixDataConBuilder _ dc)) = dc
dataConBuilderCon (L _ (InfixDataConBuilder _ dc _)) = dc

dataConBuilderDetails :: LocatedA DataConBuilder -> HsConDeclH98Details GhcPs

-- Detect when the record syntax is used:
--   data T = MkT { ... }
dataConBuilderDetails (L _ (PrefixDataConBuilder flds _))
  | [L (EpAnn anc _ cs) (HsRecTy an fields)] <- toList flds
  = RecCon (L (EpAnn anc an cs) fields)

-- Normal prefix constructor, e.g.  data T = MkT A B C
dataConBuilderDetails (L _ (PrefixDataConBuilder flds _))
  = PrefixCon noTypeArgs (map hsLinear (toList flds))

-- Infix constructor, e.g. data T = Int :! Bool
dataConBuilderDetails (L (EpAnn _ _ csl) (InfixDataConBuilder (L (EpAnn anc ann csll) lhs) _ rhs))
  = InfixCon (hsLinear (L (EpAnn anc ann (csl Semi.<> csll)) lhs)) (hsLinear rhs)


instance DisambTD DataConBuilder where
  mkHsAppTyHeadPV = tyToDataConBuilder

  mkHsAppTyPV (L l (PrefixDataConBuilder flds fn)) t =
    return $
      L (noAnnSrcSpan $ combineSrcSpans (locA l) (getLocA t))
        (PrefixDataConBuilder (flds `snocOL` t) fn)
  mkHsAppTyPV (L _ InfixDataConBuilder{}) _ =
    -- This case is impossible because of the way
    -- the grammar in Parser.y is written (see infixtype/ftype).
    panic "mkHsAppTyPV: InfixDataConBuilder"

  mkHsAppKindTyPV lhs at ki =
    addFatalError $ mkPlainErrorMsgEnvelope (getEpTokenSrcSpan at) $
                      (PsErrUnexpectedKindAppInDataCon (unLoc lhs) (unLoc ki))

  mkHsOpTyPV prom lhs tc rhs = do
      check_no_ops (unLoc rhs)  -- check the RHS because parsing type operators is right-associative
      data_con <- eitherToP $ tyConToDataCon tc
      !cs <- getCommentsFor (locA l)
      checkNotPromotedDataCon prom data_con
      return $ L (addCommentsToEpAnn l cs) (InfixDataConBuilder lhs data_con rhs)
    where
      l = combineLocsA lhs rhs
      check_no_ops (HsBangTy _ _ t) = check_no_ops (unLoc t)
      check_no_ops (HsOpTy{}) =
        addError $ mkPlainErrorMsgEnvelope (locA l) $
                     (PsErrInvalidInfixDataCon (unLoc lhs) (unLoc tc) (unLoc rhs))
      check_no_ops _ = return ()

  mkUnpackednessPV unpk constr_stuff
    | L _ (InfixDataConBuilder lhs data_con rhs) <- constr_stuff
    = -- When the user writes  data T = {-# UNPACK #-} Int :+ Bool
      --   we apply {-# UNPACK #-} to the LHS
      do lhs' <- addUnpackednessP unpk lhs
         let l = combineLocsA (reLoc unpk) constr_stuff
         return $ L l (InfixDataConBuilder lhs' data_con rhs)
    | otherwise =
      do addError $ mkPlainErrorMsgEnvelope (getLoc unpk) PsErrUnpackDataCon
         return constr_stuff

tyToDataConBuilder :: LHsType GhcPs -> PV (LocatedA DataConBuilder)
tyToDataConBuilder (L l (HsTyVar _ prom v)) = do
  data_con <- eitherToP $ tyConToDataCon v
  checkNotPromotedDataCon prom data_con
  return $ L l (PrefixDataConBuilder nilOL data_con)
tyToDataConBuilder (L l (HsTupleTy _ HsBoxedOrConstraintTuple ts)) = do
  let data_con = L (l2l l) (getRdrName (tupleDataCon Boxed (length ts)))
  return $ L l (PrefixDataConBuilder (toOL ts) data_con)
tyToDataConBuilder (L l (HsTupleTy _ HsUnboxedTuple ts)) = do
  let data_con = L (l2l l) (getRdrName (tupleDataCon Unboxed (length ts)))
  return $ L l (PrefixDataConBuilder (toOL ts) data_con)
tyToDataConBuilder t =
  addFatalError $ mkPlainErrorMsgEnvelope (getLocA t) $
                    (PsErrInvalidDataCon (unLoc t))

-- | Rejects declarations such as @data T = 'MkT@ (note the leading tick).
checkNotPromotedDataCon :: PromotionFlag -> LocatedN RdrName -> PV ()
checkNotPromotedDataCon NotPromoted _ = return ()
checkNotPromotedDataCon IsPromoted (L l name) =
  addError $ mkPlainErrorMsgEnvelope (locA l) $
    PsErrIllegalPromotionQuoteDataCon name

mkUnboxedSumCon :: LHsType GhcPs -> ConTag -> Arity -> (LocatedN RdrName, HsConDeclH98Details GhcPs)
mkUnboxedSumCon t tag arity =
  (noLocA (getRdrName (sumDataCon tag arity)), PrefixCon noTypeArgs [hsLinear t])

{- Note [Ambiguous syntactic categories]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are places in the grammar where we do not know whether we are parsing an
expression or a pattern without unlimited lookahead (which we do not have in
'happy'):

View patterns:

    f (Con a b     ) = ...  -- 'Con a b' is a pattern
    f (Con a b -> x) = ...  -- 'Con a b' is an expression

do-notation:

    do { Con a b <- x } -- 'Con a b' is a pattern
    do { Con a b }      -- 'Con a b' is an expression

Guards:

    x | True <- p && q = ...  -- 'True' is a pattern
    x | True           = ...  -- 'True' is an expression

Top-level value/function declarations (FunBind/PatBind):

    f ! a         -- TH splice
    f ! a = ...   -- function declaration

    Until we encounter the = sign, we don't know if it's a top-level
    TemplateHaskell splice where ! is used, or if it's a function declaration
    where ! is bound.

There are also places in the grammar where we do not know whether we are
parsing an expression or a command:

    proc x -> do { (stuff) -< x }   -- 'stuff' is an expression
    proc x -> do { (stuff) }        -- 'stuff' is a command

    Until we encounter arrow syntax (-<) we don't know whether to parse 'stuff'
    as an expression or a command.

In fact, do-notation is subject to both ambiguities:

    proc x -> do { (stuff) -< x }        -- 'stuff' is an expression
    proc x -> do { (stuff) <- f -< x }   -- 'stuff' is a pattern
    proc x -> do { (stuff) }             -- 'stuff' is a command

There are many possible solutions to this problem. For an overview of the ones
we decided against, see Note [Resolving parsing ambiguities: non-taken alternatives]

The solution that keeps basic definitions (such as HsExpr) clean, keeps the
concerns local to the parser, and does not require duplication of hsSyn types,
or an extra pass over the entire AST, is to parse into an overloaded
parser-validator (a so-called tagless final encoding):

    class DisambECP b where ...
    instance DisambECP (HsCmd GhcPs) where ...
    instance DisambECP (HsExp GhcPs) where ...
    instance DisambECP (PatBuilder GhcPs) where ...

The 'DisambECP' class contains functions to build and validate 'b'. For example,
to add parentheses we have:

  mkHsParPV :: DisambECP b => SrcSpan -> Located b -> PV (Located b)

'mkHsParPV' will wrap the inner value in HsCmdPar for commands, HsPar for
expressions, and 'PatBuilderPar' for patterns (later transformed into ParPat,
see Note [PatBuilder]).

Consider the 'alts' production used to parse case-of alternatives:

  alts :: { Located ([AddEpAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

We abstract over LHsExpr GhcPs, and it becomes:

  alts :: { forall b. DisambECP b => PV (Located ([AddEpAnn],[LMatch GhcPs (Located b)])) }
    : alts1     { $1 >>= \ $1 ->
                  return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { $2 >>= \ $2 ->
                  return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Compared to the initial definition, the added bits are:

    forall b. DisambECP b => PV ( ... ) -- in the type signature
    $1 >>= \ $1 -> return $             -- in one reduction rule
    $2 >>= \ $2 -> return $             -- in another reduction rule

The overhead is constant relative to the size of the rest of the reduction
rule, so this approach scales well to large parser productions.

Note that we write ($1 >>= \ $1 -> ...), so the second $1 is in a binding
position and shadows the previous $1. We can do this because internally
'happy' desugars $n to happy_var_n, and the rationale behind this idiom
is to be able to write (sLL $1 $>) later on. The alternative would be to
write this as ($1 >>= \ fresh_name -> ...), but then we couldn't refer
to the last fresh name as $>.

Finally, we instantiate the polymorphic type to a concrete one, and run the
parser-validator, for example:

    stmt   :: { forall b. DisambECP b => PV (LStmt GhcPs (Located b)) }
    e_stmt :: { LStmt GhcPs (LHsExpr GhcPs) }
            : stmt {% runPV $1 }

In e_stmt, three things happen:

  1. we instantiate: b ~ HsExpr GhcPs
  2. we embed the PV computation into P by using runPV
  3. we run validation by using a monadic production, {% ... }

At this point the ambiguity is resolved.
-}


{- Note [Resolving parsing ambiguities: non-taken alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Alternative I, extra constructors in GHC.Hs.Expr
------------------------------------------------
We could add extra constructors to HsExpr to represent command-specific and
pattern-specific syntactic constructs. Under this scheme, we parse patterns
and commands as expressions and rejig later.  This is what GHC used to do, and
it polluted 'HsExpr' with irrelevant constructors:

  * for commands: 'HsArrForm', 'HsArrApp'
  * for patterns: 'EWildPat', 'EAsPat', 'EViewPat', 'ELazyPat'

(As of now, we still do that for patterns, but we plan to fix it).

There are several issues with this:

  * The implementation details of parsing are leaking into hsSyn definitions.

  * Code that uses HsExpr has to panic on these impossible-after-parsing cases.

  * HsExpr is arbitrarily selected as the extension basis. Why not extend
    HsCmd or HsPat with extra constructors instead?

Alternative II, extra constructors in GHC.Hs.Expr for GhcPs
-----------------------------------------------------------
We could address some of the problems with Alternative I by using Trees That
Grow and extending HsExpr only in the GhcPs pass. However, GhcPs corresponds to
the output of parsing, not to its intermediate results, so we wouldn't want
them there either.

Alternative III, extra constructors in GHC.Hs.Expr for GhcPrePs
---------------------------------------------------------------
We could introduce a new pass, GhcPrePs, to keep GhcPs pristine.
Unfortunately, creating a new pass would significantly bloat conversion code
and slow down the compiler by adding another linear-time pass over the entire
AST. For example, in order to build HsExpr GhcPrePs, we would need to build
HsLocalBinds GhcPrePs (as part of HsLet), and we never want HsLocalBinds
GhcPrePs.


Alternative IV, sum type and bottom-up data flow
------------------------------------------------
Expressions and commands are disjoint. There are no user inputs that could be
interpreted as either an expression or a command depending on outer context:

  5        -- definitely an expression
  x -< y   -- definitely a command

Even though we have both 'HsLam' and 'HsCmdLam', we can look at
the body to disambiguate:

  \p -> 5        -- definitely an expression
  \p -> x -< y   -- definitely a command

This means we could use a bottom-up flow of information to determine
whether we are parsing an expression or a command, using a sum type
for intermediate results:

  Either (LHsExpr GhcPs) (LHsCmd GhcPs)

There are two problems with this:

  * We cannot handle the ambiguity between expressions and
    patterns, which are not disjoint.

  * Bottom-up flow of information leads to poor error messages. Consider

        if ... then 5 else (x -< y)

    Do we report that '5' is not a valid command or that (x -< y) is not a
    valid expression?  It depends on whether we want the entire node to be
    'HsIf' or 'HsCmdIf', and this information flows top-down, from the
    surrounding parsing context (are we in 'proc'?)

Alternative V, backtracking with parser combinators
---------------------------------------------------
One might think we could sidestep the issue entirely by using a backtracking
parser and doing something along the lines of (try pExpr <|> pPat).

Turns out, this wouldn't work very well, as there can be patterns inside
expressions (e.g. via 'case', 'let', 'do') and expressions inside patterns
(e.g. view patterns). To handle this, we would need to backtrack while
backtracking, and unbound levels of backtracking lead to very fragile
performance.

Alternative VI, an intermediate data type
-----------------------------------------
There are common syntactic elements of expressions, commands, and patterns
(e.g. all of them must have balanced parentheses), and we can capture this
common structure in an intermediate data type, Frame:

data Frame
  = FrameVar RdrName
    -- ^ Identifier: Just, map, BS.length
  | FrameTuple [LTupArgFrame] Boxity
    -- ^ Tuple (section): (a,b) (a,b,c) (a,,) (,a,)
  | FrameTySig LFrame (LHsSigWcType GhcPs)
    -- ^ Type signature: x :: ty
  | FramePar (SrcSpan, SrcSpan) LFrame
    -- ^ Parentheses
  | FrameIf LFrame LFrame LFrame
    -- ^ If-expression: if p then x else y
  | FrameCase LFrame [LFrameMatch]
    -- ^ Case-expression: case x of { p1 -> e1; p2 -> e2 }
  | FrameDo HsStmtContextRn [LFrameStmt]
    -- ^ Do-expression: do { s1; a <- s2; s3 }
  ...
  | FrameExpr (HsExpr GhcPs)   -- unambiguously an expression
  | FramePat (HsPat GhcPs)     -- unambiguously a pattern
  | FrameCommand (HsCmd GhcPs) -- unambiguously a command

To determine which constructors 'Frame' needs to have, we take the union of
intersections between HsExpr, HsCmd, and HsPat.

The intersection between HsPat and HsExpr:

  HsPat  =  VarPat   | TuplePat      | SigPat        | ParPat   | ...
  HsExpr =  HsVar    | ExplicitTuple | ExprWithTySig | HsPar    | ...
  -------------------------------------------------------------------
  Frame  =  FrameVar | FrameTuple    | FrameTySig    | FramePar | ...

The intersection between HsCmd and HsExpr:

  HsCmd  = HsCmdIf | HsCmdCase | HsCmdDo | HsCmdPar
  HsExpr = HsIf    | HsCase    | HsDo    | HsPar
  ------------------------------------------------
  Frame = FrameIf  | FrameCase | FrameDo | FramePar

The intersection between HsCmd and HsPat:

  HsPat  = ParPat   | ...
  HsCmd  = HsCmdPar | ...
  -----------------------
  Frame  = FramePar | ...

Take the union of each intersection and this yields the final 'Frame' data
type. The problem with this approach is that we end up duplicating a good
portion of hsSyn:

    Frame         for  HsExpr, HsPat, HsCmd
    TupArgFrame   for  HsTupArg
    FrameMatch    for  Match
    FrameStmt     for  StmtLR
    FrameGRHS     for  GRHS
    FrameGRHSs    for  GRHSs
    ...

Alternative VII, a product type
-------------------------------
We could avoid the intermediate representation of Alternative VI by parsing
into a product of interpretations directly:

    type ExpCmdPat = ( PV (LHsExpr GhcPs)
                     , PV (LHsCmd GhcPs)
                     , PV (LHsPat GhcPs) )

This means that in positions where we do not know whether to produce
expression, a pattern, or a command, we instead produce a parser-validator for
each possible option.

Then, as soon as we have parsed far enough to resolve the ambiguity, we pick
the appropriate component of the product, discarding the rest:

    checkExpOf3 (e, _, _) = e  -- interpret as an expression
    checkCmdOf3 (_, c, _) = c  -- interpret as a command
    checkPatOf3 (_, _, p) = p  -- interpret as a pattern

We can easily define ambiguities between arbitrary subsets of interpretations.
For example, when we know ahead of type that only an expression or a command is
possible, but not a pattern, we can use a smaller type:

    type ExpCmd = (PV (LHsExpr GhcPs), PV (LHsCmd GhcPs))

    checkExpOf2 (e, _) = e  -- interpret as an expression
    checkCmdOf2 (_, c) = c  -- interpret as a command

However, there is a slight problem with this approach, namely code duplication
in parser productions. Consider the 'alts' production used to parse case-of
alternatives:

  alts :: { Located ([AddEpAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Under the new scheme, we have to completely duplicate its type signature and
each reduction rule:

  alts :: { ( PV (Located ([AddEpAnn],[LMatch GhcPs (LHsExpr GhcPs)])) -- as an expression
            , PV (Located ([AddEpAnn],[LMatch GhcPs (LHsCmd GhcPs)]))  -- as a command
            ) }
    : alts1
        { ( checkExpOf2 $1 >>= \ $1 ->
            return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1)
          , checkCmdOf2 $1 >>= \ $1 ->
            return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1)
          ) }
    | ';' alts
        { ( checkExpOf2 $2 >>= \ $2 ->
            return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2)
          , checkCmdOf2 $2 >>= \ $2 ->
            return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2)
          ) }

And the same goes for other productions: 'altslist', 'alts1', 'alt', 'alt_rhs',
'ralt', 'gdpats', 'gdpat', 'exp', ... and so on. That is a lot of code!

Alternative VIII, a function from a GADT
----------------------------------------
We could avoid code duplication of the Alternative VII by representing the product
as a function from a GADT:

    data ExpCmdG b where
      ExpG :: ExpCmdG HsExpr
      CmdG :: ExpCmdG HsCmd

    type ExpCmd = forall b. ExpCmdG b -> PV (Located (b GhcPs))

    checkExp :: ExpCmd -> PV (LHsExpr GhcPs)
    checkCmd :: ExpCmd -> PV (LHsCmd GhcPs)
    checkExp f = f ExpG  -- interpret as an expression
    checkCmd f = f CmdG  -- interpret as a command

Consider the 'alts' production used to parse case-of alternatives:

  alts :: { Located ([AddEpAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

We abstract over LHsExpr, and it becomes:

  alts :: { forall b. ExpCmdG b -> PV (Located ([AddEpAnn],[LMatch GhcPs (Located (b GhcPs))])) }
    : alts1
        { \tag -> $1 tag >>= \ $1 ->
                  return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts
        { \tag -> $2 tag >>= \ $2 ->
                  return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Note that 'ExpCmdG' is a singleton type, the value is completely
determined by the type:

  when (b~HsExpr),  tag = ExpG
  when (b~HsCmd),   tag = CmdG

This is a clear indication that we can use a class to pass this value behind
the scenes:

  class    ExpCmdI b      where expCmdG :: ExpCmdG b
  instance ExpCmdI HsExpr where expCmdG = ExpG
  instance ExpCmdI HsCmd  where expCmdG = CmdG

And now the 'alts' production is simplified, as we no longer need to
thread 'tag' explicitly:

  alts :: { forall b. ExpCmdI b => PV (Located ([AddEpAnn],[LMatch GhcPs (Located (b GhcPs))])) }
    : alts1     { $1 >>= \ $1 ->
                  return $ sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { $2 >>= \ $2 ->
                  return $ sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

This encoding works well enough, but introduces an extra GADT unlike the
tagless final encoding, and there's no need for this complexity.

-}

{- Note [PatBuilder]
~~~~~~~~~~~~~~~~~~~~
Unlike HsExpr or HsCmd, the Pat type cannot accommodate all intermediate forms,
so we introduce the notion of a PatBuilder.

Consider a pattern like this:

  Con a b c

We parse arguments to "Con" one at a time in the  fexp aexp  parser production,
building the result with mkHsAppPV, so the intermediate forms are:

  1. Con
  2. Con a
  3. Con a b
  4. Con a b c

In 'HsExpr', we have 'HsApp', so the intermediate forms are represented like
this (pseudocode):

  1. "Con"
  2. HsApp "Con" "a"
  3. HsApp (HsApp "Con" "a") "b"
  3. HsApp (HsApp (HsApp "Con" "a") "b") "c"

Similarly, in 'HsCmd' we have 'HsCmdApp'. In 'Pat', however, what we have
instead is 'ConPatIn', which is very awkward to modify and thus unsuitable for
the intermediate forms.

We also need an intermediate representation to postpone disambiguation between
FunBind and PatBind. Consider:

  a `Con` b = ...
  a `fun` b = ...

How do we know that (a `Con` b) is a PatBind but (a `fun` b) is a FunBind? We
learn this by inspecting an intermediate representation in 'isFunLhs' and
seeing that 'Con' is a data constructor but 'f' is not. We need an intermediate
representation capable of representing both a FunBind and a PatBind, so Pat is
insufficient.

PatBuilder is an extension of Pat that is capable of representing intermediate
parsing results for patterns and function bindings:

  data PatBuilder p
    = PatBuilderPat (Pat p)
    | PatBuilderApp (LocatedA (PatBuilder p)) (LocatedA (PatBuilder p))
    | PatBuilderOpApp (LocatedA (PatBuilder p)) (LocatedA RdrName) (LocatedA (PatBuilder p))
    ...

It can represent any pattern via 'PatBuilderPat', but it also has a variety of
other constructors which were added by following a simple principle: we never
pattern match on the pattern stored inside 'PatBuilderPat'.
-}

---------------------------------------------------------------------------
-- Miscellaneous utilities

-- | Check if a fixity is valid. We support bypassing the usual bound checks
-- for some special operators.
checkPrecP
        :: Located (SourceText,Int)              -- ^ precedence
        -> Located (OrdList (LocatedN RdrName))  -- ^ operators
        -> P ()
checkPrecP (L l (_,i)) (L _ ol)
 | 0 <= i, i <= maxPrecedence = pure ()
 | all specialOp ol = pure ()
 | otherwise = addFatalError $ mkPlainErrorMsgEnvelope l (PsErrPrecedenceOutOfRange i)
  where
    -- If you change this, consider updating Note [Fixity of (->)] in GHC/Types.hs
    specialOp op = unLoc op == getRdrName unrestrictedFunTyCon

mkRecConstrOrUpdate
        :: Bool
        -> LHsExpr GhcPs
        -> SrcSpan
        -> ([Fbind (HsExpr GhcPs)], Maybe SrcSpan)
        -> (Maybe (EpToken "{"), Maybe (EpToken "}"))
        -> PV (HsExpr GhcPs)
mkRecConstrOrUpdate _ (L _ (HsVar _ (L l c))) _lrec (fbinds,dd) anns
  | isRdrDataCon c
  = do
      let (fs, ps) = partitionEithers fbinds
      case ps of
          p:_ -> addFatalError $ mkPlainErrorMsgEnvelope (getLocA p) $
              PsErrOverloadedRecordDotInvalid
          _ -> return (mkRdrRecordCon (L l c) (mk_rec_fields fs dd) anns)
mkRecConstrOrUpdate overloaded_update exp _ (fs,dd) anns
  | Just dd_loc <- dd = addFatalError $ mkPlainErrorMsgEnvelope dd_loc $
                                          PsErrDotsInRecordUpdate
  | otherwise = mkRdrRecordUpd overloaded_update exp fs anns

mkRdrRecordUpd :: Bool -> LHsExpr GhcPs -> [Fbind (HsExpr GhcPs)] -> (Maybe (EpToken "{"), Maybe (EpToken "}"))
  -> PV (HsExpr GhcPs)
mkRdrRecordUpd overloaded_on exp@(L loc _) fbinds anns = do
  -- We do not need to know if OverloadedRecordDot is in effect. We do
  -- however need to know if OverloadedRecordUpdate (passed in
  -- overloaded_on) is in effect because it affects the Left/Right nature
  -- of the RecordUpd value we calculate.
  let (fs, ps) = partitionEithers fbinds
      fs' :: [LHsRecUpdField GhcPs GhcPs]
      fs' = map (fmap mk_rec_upd_field) fs
  case overloaded_on of
    False | not $ null ps ->
      -- A '.' was found in an update and OverloadedRecordUpdate isn't on.
      addFatalError $ mkPlainErrorMsgEnvelope (locA loc) PsErrOverloadedRecordUpdateNotEnabled
    False ->
      -- This is just a regular record update.
      return RecordUpd {
        rupd_ext = anns
      , rupd_expr = exp
      , rupd_flds =
          RegularRecUpdFields
            { xRecUpdFields = noExtField
            , recUpdFields  = fs' } }
    -- This is a RecordDotSyntax update.
    True -> do
      let qualifiedFields =
            [ L l lbl | L _ (HsFieldBind _ (L l lbl) _ _) <- fs'
                      , isQual . fieldOccRdrName $ lbl
            ]
      case qualifiedFields of
          qf:_ -> addFatalError $ mkPlainErrorMsgEnvelope (getLocA qf) $
                  PsErrOverloadedRecordUpdateNoQualifiedFields
          _ -> return $
               RecordUpd
                { rupd_ext = anns
                , rupd_expr = exp
                , rupd_flds =
                   OverloadedRecUpdFields
                     { xOLRecUpdFields = noExtField
                     , olRecUpdFields  = toProjUpdates fbinds } }
  where
    toProjUpdates :: [Fbind (HsExpr GhcPs)] -> [LHsRecUpdProj GhcPs]
    toProjUpdates = map (\case { Right p -> p; Left f -> recFieldToProjUpdate f })

    -- Convert a top-level field update like {foo=2} or {bar} (punned)
    -- to a projection update.
    recFieldToProjUpdate :: LHsRecField GhcPs  (LHsExpr GhcPs) -> LHsRecUpdProj GhcPs
    recFieldToProjUpdate (L l (HsFieldBind anns (L _ (FieldOcc _ (L loc rdr))) arg pun)) =
        -- The idea here is to convert the label to a singleton [FastString].
        let f = occNameFS . rdrNameOcc $ rdr
            fl = DotFieldOcc noAnn (L loc (FieldLabelString f))
            lf = locA loc
        in mkRdrProjUpdate l (L lf [L (l2l loc) fl]) (punnedVar f) pun anns
        where
          -- If punning, compute HsVar "f" otherwise just arg. This
          -- has the effect that sentinel HsVar "pun-rhs" is replaced
          -- by HsVar "f" here, before the update is written to a
          -- setField expressions.
          punnedVar :: FastString -> LHsExpr GhcPs
          punnedVar f  = if not pun then arg else noLocA . HsVar noExtField . noLocA . mkRdrUnqual . mkVarOccFS $ f

mkRdrRecordCon
  :: LocatedN RdrName -> HsRecordBinds GhcPs -> (Maybe (EpToken "{"), Maybe (EpToken "}")) -> HsExpr GhcPs
mkRdrRecordCon con flds anns
  = RecordCon { rcon_ext = anns, rcon_con = con, rcon_flds = flds }

mk_rec_fields :: [LocatedA (HsRecField GhcPs arg)] -> Maybe SrcSpan -> HsRecFields GhcPs arg
mk_rec_fields fs Nothing = HsRecFields { rec_ext = noExtField, rec_flds = fs, rec_dotdot = Nothing }
mk_rec_fields fs (Just s)  = HsRecFields { rec_ext = noExtField, rec_flds = fs
                                     , rec_dotdot = Just (L (l2l s) (RecFieldsDotDot $ length fs)) }

mk_rec_upd_field :: HsRecField GhcPs (LHsExpr GhcPs) -> HsRecUpdField GhcPs GhcPs
mk_rec_upd_field (HsFieldBind noAnn (L loc (FieldOcc _ rdr)) arg pun)
  = HsFieldBind noAnn (L loc (FieldOcc noExtField rdr)) arg pun

mkInlinePragma :: SourceText -> (InlineSpec, RuleMatchInfo) -> Maybe Activation
               -> InlinePragma
-- The (Maybe Activation) is because the user can omit
-- the activation spec (and usually does)
mkInlinePragma src (inl, match_info) mb_act
  = InlinePragma { inl_src = src -- See Note [Pragma source text] in "GHC.Types.SourceText"
                 , inl_inline = inl
                 , inl_sat    = Nothing
                 , inl_act    = act
                 , inl_rule   = match_info }
  where
    act = case mb_act of
            Just act -> act
            Nothing  -> -- No phase specified
                        case inl of
                          NoInline _  -> NeverActive
                          Opaque _    -> NeverActive
                          _other      -> AlwaysActive

mkOpaquePragma :: SourceText -> InlinePragma
mkOpaquePragma src
  = InlinePragma { inl_src    = src
                 , inl_inline = Opaque src
                 , inl_sat    = Nothing
                 -- By marking the OPAQUE pragma NeverActive we stop
                 -- (constructor) specialisation on OPAQUE things.
                 --
                 -- See Note [OPAQUE pragma]
                 , inl_act    = NeverActive
                 , inl_rule   = FunLike
                 }

checkNewOrData :: SrcSpan -> RdrName -> Bool -> NewOrData -> [LConDecl GhcPs]
               -> P (DataDefnCons (LConDecl GhcPs))
checkNewOrData span name is_type_data = curry $ \ case
    (NewType, [a]) -> pure $ NewTypeCon a
    (DataType, as) -> pure $ DataTypeCons is_type_data (handle_type_data as)
    (NewType, as) -> addFatalError $ mkPlainErrorMsgEnvelope span $ PsErrMultipleConForNewtype name (length as)
  where
    -- In a "type data" declaration, the constructors are in the type/class
    -- namespace rather than the data constructor namespace.
    -- See Note [Type data declarations] in GHC.Rename.Module.
    handle_type_data
      | is_type_data = map (fmap promote_constructor)
      | otherwise = id

    promote_constructor (dc@ConDeclGADT { con_names = cons })
      = dc { con_names = fmap (fmap promote_name) cons }
    promote_constructor (dc@ConDeclH98 { con_name = con })
      = dc { con_name = fmap promote_name con }
    promote_constructor dc = dc

    promote_name name = fromMaybe name (promoteRdrName name)

-----------------------------------------------------------------------------
-- utilities for foreign declarations

-- construct a foreign import declaration
--
mkImport :: Located CCallConv
         -> Located Safety
         -> (Located StringLiteral, LocatedN RdrName, LHsSigType GhcPs)
         -> (EpToken "import", TokDcolon)
         -> P (EpToken "foreign" -> HsDecl GhcPs)
mkImport cconv safety (L loc (StringLiteral esrc entity _), v, ty) (timport, td) =
    case unLoc cconv of
      CCallConv          -> returnSpec =<< mkCImport
      CApiConv           -> do
        imp <- mkCImport
        if isCWrapperImport imp
          then addFatalError $ mkPlainErrorMsgEnvelope loc PsErrInvalidCApiImport
          else returnSpec imp
      StdCallConv        -> returnSpec =<< mkCImport
      PrimCallConv       -> mkOtherImport
      JavaScriptCallConv -> mkOtherImport
  where
    -- Parse a C-like entity string of the following form:
    --   "[static] [chname] [&] [cid]" | "dynamic" | "wrapper"
    -- If 'cid' is missing, the function name 'v' is used instead as symbol
    -- name (cf section 8.5.1 in Haskell 2010 report).
    mkCImport = do
      let e = unpackFS entity
      case parseCImport (reLoc cconv) (reLoc safety) (mkExtName (unLoc v)) e (L loc esrc) of
        Nothing         -> addFatalError $ mkPlainErrorMsgEnvelope loc $
                             PsErrMalformedEntityString
        Just importSpec -> return importSpec

    isCWrapperImport (CImport _ _ _ _ CWrapper) = True
    isCWrapperImport _ = False

    -- currently, all the other import conventions only support a symbol name in
    -- the entity string. If it is missing, we use the function name instead.
    mkOtherImport = returnSpec importSpec
      where
        entity'    = if nullFS entity
                        then mkExtName (unLoc v)
                        else entity
        funcTarget = CFunction (StaticTarget esrc entity' Nothing True)
        importSpec = CImport (L (l2l loc) esrc) (reLoc cconv) (reLoc safety) Nothing funcTarget

    returnSpec spec = return $ \tforeign -> ForD noExtField $ ForeignImport
          { fd_i_ext  = (tforeign, timport, td)
          , fd_name   = v
          , fd_sig_ty = ty
          , fd_fi     = spec
          }



-- the string "foo" is ambiguous: either a header or a C identifier.  The
-- C identifier case comes first in the alternatives below, so we pick
-- that one.
parseCImport :: LocatedE CCallConv -> LocatedE Safety -> FastString -> String
             -> Located SourceText
             -> Maybe (ForeignImport (GhcPass p))
parseCImport cconv safety nm str sourceText =
 listToMaybe $ map fst $ filter (null.snd) $
     readP_to_S parse str
 where
   parse = do
       skipSpaces
       r <- choice [
          string "dynamic" >> return (mk Nothing (CFunction DynamicTarget)),
          string "wrapper" >> return (mk Nothing CWrapper),
          do optional (token "static" >> skipSpaces)
             ((mk Nothing <$> cimp nm) +++
              (do h <- munch1 hdr_char
                  skipSpaces
                  let src = mkFastString h
                  mk (Just (Header (SourceText src) src))
                      <$> cimp nm))
         ]
       skipSpaces
       return r

   token str = do _ <- string str
                  toks <- look
                  case toks of
                      c : _
                       | id_char c -> pfail
                      _            -> return ()

   mk h n = CImport (reLoc sourceText) (reLoc cconv) (reLoc safety) h n

   hdr_char c = not (isSpace c)
   -- header files are filenames, which can contain
   -- pretty much any char (depending on the platform),
   -- so just accept any non-space character
   id_first_char c = isAlpha    c || c == '_'
   id_char       c = isAlphaNum c || c == '_'

   cimp nm = (ReadP.char '&' >> skipSpaces >> CLabel <$> cid)
             +++ (do isFun <- case unLoc cconv of
                               CApiConv ->
                                  option True
                                         (do token "value"
                                             skipSpaces
                                             return False)
                               _ -> return True
                     cid' <- cid
                     return (CFunction (StaticTarget NoSourceText cid'
                                        Nothing isFun)))
          where
            cid = return nm +++
                  (do c  <- satisfy id_first_char
                      cs <-  many (satisfy id_char)
                      return (mkFastString (c:cs)))


-- construct a foreign export declaration
--
mkExport :: Located CCallConv
         -> (Located StringLiteral, LocatedN RdrName, LHsSigType GhcPs)
         -> ( EpToken "export", TokDcolon)
         -> P (EpToken "foreign" -> HsDecl GhcPs)
mkExport (L lc cconv) (L le (StringLiteral esrc entity _), v, ty) (texport, td)
 = return $ \tforeign -> ForD noExtField $
   ForeignExport { fd_e_ext = (tforeign, texport, td), fd_name = v, fd_sig_ty = ty
                 , fd_fe = CExport (L (l2l le) esrc) (L (l2l lc) (CExportStatic esrc entity' cconv)) }
  where
    entity' | nullFS entity = mkExtName (unLoc v)
            | otherwise     = entity

-- Supplying the ext_name in a foreign decl is optional; if it
-- isn't there, the Haskell name is assumed. Note that no transformation
-- of the Haskell name is then performed, so if you foreign export (++),
-- it's external name will be "++". Too bad; it's important because we don't
-- want z-encoding (e.g. names with z's in them shouldn't be doubled)
--
mkExtName :: RdrName -> CLabelString
mkExtName rdrNm = occNameFS (rdrNameOcc rdrNm)

--------------------------------------------------------------------------------
-- Help with module system imports/exports

data ImpExpSubSpec = ImpExpAbs
                   | ImpExpAll (EpToken "..")
                   | ImpExpList [LocatedA ImpExpQcSpec]
                   | ImpExpAllWith [LocatedA ImpExpQcSpec]

data ImpExpQcSpec = ImpExpQcName (LocatedN RdrName)
                  | ImpExpQcType EpaLocation (LocatedN RdrName)
                  | ImpExpQcWildcard (EpToken "..") (EpToken ",")

mkModuleImpExp :: Maybe (LWarningTxt GhcPs) -> (EpToken "(", EpToken ")") -> LocatedA ImpExpQcSpec
               -> ImpExpSubSpec -> P (IE GhcPs)
mkModuleImpExp warning (top, tcp) (L l specname) subs = do
  case subs of
    ImpExpAbs
      | isVarNameSpace (rdrNameSpace name)
                       -> return $ IEVar warning
                           (L l (ieNameFromSpec specname)) Nothing
      | otherwise      -> IEThingAbs warning . L l <$> nameT <*> pure noExportDoc
    ImpExpAll tok      -> IEThingAll (warning, (top, tok, tcp)) . L l <$> nameT <*> pure noExportDoc
    ImpExpList xs      ->
      (\newName -> IEThingWith (warning, (top,NoEpTok,NoEpTok,tcp)) (L l newName)
        NoIEWildcard (wrapped xs)) <$> nameT <*> pure noExportDoc
    ImpExpAllWith xs                       ->
      do allowed <- getBit PatternSynonymsBit
         if allowed
          then
            let withs = map unLoc xs
                pos   = maybe NoIEWildcard IEWildcard
                          (findIndex isImpExpQcWildcard withs)
                (td,tc) = case find isImpExpQcWildcard withs of
                  Just (ImpExpQcWildcard td tc) -> (td,tc)
                  _ -> (NoEpTok, NoEpTok)
                ies :: [LocatedA (IEWrappedName GhcPs)]
                ies   = wrapped $ filter (not . isImpExpQcWildcard . unLoc) xs
            in (\newName
                        -> IEThingWith (warning, (top,td,tc,tcp)) (L l newName) pos ies)
               <$> nameT <*> pure noExportDoc
          else addFatalError $ mkPlainErrorMsgEnvelope (locA l) $
                 PsErrIllegalPatSynExport
  where
    noExportDoc :: Maybe (LHsDoc GhcPs)
    noExportDoc = Nothing

    name = ieNameVal specname
    nameT =
      if isVarNameSpace (rdrNameSpace name)
        then addFatalError $ mkPlainErrorMsgEnvelope (locA l) $
               (PsErrVarForTyCon name)
        else return $ ieNameFromSpec specname

    ieNameVal (ImpExpQcName ln)   = unLoc ln
    ieNameVal (ImpExpQcType _ ln) = unLoc ln
    ieNameVal ImpExpQcWildcard{}  = panic "ieNameVal got wildcard"

    ieNameFromSpec :: ImpExpQcSpec -> IEWrappedName GhcPs
    ieNameFromSpec (ImpExpQcName   (L l n)) = IEName noExtField (L l n)
    ieNameFromSpec (ImpExpQcType r (L l n)) = IEType r (L l n)
    ieNameFromSpec ImpExpQcWildcard{}       = panic "ieName got wildcard"

    wrapped = map (fmap ieNameFromSpec)

mkTypeImpExp :: LocatedN RdrName   -- TcCls or Var name space
             -> P (LocatedN RdrName)
mkTypeImpExp name =
  do requireExplicitNamespaces (getLocA name)
     return (fmap (`setRdrNameSpace` tcClsName) name)

checkImportSpec :: LocatedLI [LIE GhcPs] -> P (LocatedLI [LIE GhcPs])
checkImportSpec ie@(L _ specs) =
    case [l | (L l (IEThingWith _ _ (IEWildcard _) _ _)) <- specs] of
      [] -> return ie
      (l:_) -> importSpecError (locA l)
  where
    importSpecError l =
      addFatalError $ mkPlainErrorMsgEnvelope l PsErrIllegalImportBundleForm

-- In the correct order
mkImpExpSubSpec :: [LocatedA ImpExpQcSpec] -> P ImpExpSubSpec
mkImpExpSubSpec [] = return (ImpExpList [])
mkImpExpSubSpec [L _ (ImpExpQcWildcard td _tc)] =
  return (ImpExpAll td)
mkImpExpSubSpec xs =
  if (any (isImpExpQcWildcard . unLoc) xs)
    then return $ (ImpExpAllWith xs)
    else return $ (ImpExpList xs)

isImpExpQcWildcard :: ImpExpQcSpec -> Bool
isImpExpQcWildcard (ImpExpQcWildcard _ _) = True
isImpExpQcWildcard _                      = False

-----------------------------------------------------------------------------
-- Warnings and failures

warnPrepositiveQualifiedModule :: SrcSpan -> P ()
warnPrepositiveQualifiedModule span =
  addPsMessage span PsWarnImportPreQualified

failNotEnabledImportQualifiedPost :: SrcSpan -> P ()
failNotEnabledImportQualifiedPost loc =
  addError $ mkPlainErrorMsgEnvelope loc $ PsErrImportPostQualified

failImportQualifiedTwice :: SrcSpan -> P ()
failImportQualifiedTwice loc =
  addError $ mkPlainErrorMsgEnvelope loc $ PsErrImportQualifiedTwice

warnStarIsType :: SrcSpan -> P ()
warnStarIsType span = addPsMessage span PsWarnStarIsType

failOpFewArgs :: MonadP m => LocatedN RdrName -> m a
failOpFewArgs (L loc op) =
  do { star_is_type <- getBit StarIsTypeBit
     ; let is_star_type = if star_is_type then StarIsType else StarIsNotType
     ; addFatalError $ mkPlainErrorMsgEnvelope (locA loc) $
         (PsErrOpFewArgs is_star_type op) }

requireExplicitNamespaces :: MonadP m => SrcSpan -> m ()
requireExplicitNamespaces l = do
  allowed <- getBit ExplicitNamespacesBit
  unless allowed $
    addError $ mkPlainErrorMsgEnvelope l PsErrIllegalExplicitNamespace

-----------------------------------------------------------------------------
-- Misc utils

data PV_Context =
  PV_Context
    { pv_options :: ParserOpts
    , pv_details :: ParseContext -- See Note [Parser-Validator Details]
    }

data PV_Accum =
  PV_Accum
    { pv_warnings        :: Messages PsMessage
    , pv_errors          :: Messages PsMessage
    , pv_header_comments :: Strict.Maybe [LEpaComment]
    , pv_comment_q       :: [LEpaComment]
    }

data PV_Result a = PV_Ok PV_Accum a | PV_Failed PV_Accum
  deriving (Foldable, Functor, Traversable)

-- During parsing, we make use of several monadic effects: reporting parse errors,
-- accumulating warnings, adding API annotations, and checking for extensions. These
-- effects are captured by the 'MonadP' type class.
--
-- Sometimes we need to postpone some of these effects to a later stage due to
-- ambiguities described in Note [Ambiguous syntactic categories].
-- We could use two layers of the P monad, one for each stage:
--
--   abParser :: forall x. DisambAB x => P (P x)
--
-- The outer layer of P consumes the input and builds the inner layer, which
-- validates the input. But this type is not particularly helpful, as it obscures
-- the fact that the inner layer of P never consumes any input.
--
-- For clarity, we introduce the notion of a parser-validator: a parser that does
-- not consume any input, but may fail or use other effects. Thus we have:
--
--   abParser :: forall x. DisambAB x => P (PV x)
--
newtype PV a = PV { unPV :: PV_Context -> PV_Accum -> PV_Result a }
  deriving (Functor)

instance Applicative PV where
  pure a = a `seq` PV (\_ acc -> PV_Ok acc a)
  (<*>) = ap

instance Monad PV where
  m >>= f = PV $ \ctx acc ->
    case unPV m ctx acc of
      PV_Ok acc' a -> unPV (f a) ctx acc'
      PV_Failed acc' -> PV_Failed acc'

runPV :: PV a -> P a
runPV = runPV_details noParseContext

askParseContext :: PV ParseContext
askParseContext = PV $ \(PV_Context _ details) acc -> PV_Ok acc details

runPV_details :: ParseContext -> PV a -> P a
runPV_details details m =
  P $ \s ->
    let
      pv_ctx = PV_Context
        { pv_options = options s
        , pv_details = details }
      pv_acc = PV_Accum
        { pv_warnings = warnings s
        , pv_errors   = errors s
        , pv_header_comments = header_comments s
        , pv_comment_q = comment_q s }
      mkPState acc' =
        s { warnings = pv_warnings acc'
          , errors   = pv_errors acc'
          , comment_q = pv_comment_q acc' }
    in
      case unPV m pv_ctx pv_acc of
        PV_Ok acc' a -> POk (mkPState acc') a
        PV_Failed acc' -> PFailed (mkPState acc')

instance MonadP PV where
  addError err =
    PV $ \_ctx acc -> PV_Ok acc{pv_errors = err `addMessage` pv_errors acc} ()
  addWarning w =
    PV $ \_ctx acc ->
      -- No need to check for the warning flag to be set, GHC will correctly discard suppressed
      -- diagnostics.
      PV_Ok acc{pv_warnings= w `addMessage` pv_warnings acc} ()
  addFatalError err =
    addError err >> PV (const PV_Failed)
  getParserOpts = PV $ \ctx acc -> PV_Ok acc $! pv_options ctx
  allocateCommentsP ss = PV $ \_ s ->
    if null (pv_comment_q s) then PV_Ok s emptyComments else  -- fast path
    let (comment_q', newAnns) = allocateComments ss (pv_comment_q s) in
      PV_Ok s {
         pv_comment_q = comment_q'
       } (EpaComments newAnns)
  allocatePriorCommentsP ss = PV $ \_ s ->
    let (header_comments', comment_q', newAnns)
          = allocatePriorComments ss (pv_comment_q s) (pv_header_comments s) in
      PV_Ok s {
         pv_header_comments = header_comments',
         pv_comment_q = comment_q'
       } (EpaComments newAnns)
  allocateFinalCommentsP ss = PV $ \_ s ->
    let (header_comments', comment_q', newAnns)
          = allocateFinalComments ss (pv_comment_q s) (pv_header_comments s) in
      PV_Ok s {
         pv_header_comments = header_comments',
         pv_comment_q = comment_q'
       } (EpaCommentsBalanced (Strict.fromMaybe [] header_comments') newAnns)

{- Note [Parser-Validator Details]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A PV computation is parameterized by some 'ParseContext' for diagnostic messages, which can be set
depending on validation context. We use this in checkPattern to fix #984.

Consider this example, where the user has forgotten a 'do':

  f _ = do
    x <- computation
    case () of
      _ ->
        result <- computation
        case () of () -> undefined

GHC parses it as follows:

  f _ = do
    x <- computation
    (case () of
      _ ->
        result) <- computation
        case () of () -> undefined

Note that this fragment is parsed as a pattern:

  case () of
    _ ->
      result

We attempt to detect such cases and add a hint to the diagnostic messages:

  T984.hs:6:9:
    Parse error in pattern: case () of { _ -> result }
    Possibly caused by a missing 'do'?

The "Possibly caused by a missing 'do'?" suggestion is the hint that is computed
out of the 'ParseContext', which are read by functions like 'patFail' when
constructing the 'PsParseErrorInPatDetails' data structure. When validating in a
context other than 'bindpat' (a pattern to the left of <-), we set the
details to 'noParseContext' and it has no effect on the diagnostic messages.

-}

-- | Hint about bang patterns, assuming @BangPatterns@ is off.
hintBangPat :: SrcSpan -> Pat GhcPs -> PV ()
hintBangPat span e = do
    bang_on <- getBit BangPatBit
    unless bang_on $
      addError $ mkPlainErrorMsgEnvelope span $ PsErrIllegalBangPattern e

mkSumOrTupleExpr :: SrcSpanAnnA -> Boxity -> SumOrTuple (HsExpr GhcPs)
                 -> (EpaLocation, EpaLocation)
                 -> PV (LHsExpr GhcPs)

-- Tuple
mkSumOrTupleExpr l@(EpAnn anc an csIn) boxity (Tuple es) anns = do
    !cs <- getCommentsFor (locA l)
    return $ L (EpAnn anc an (csIn Semi.<> cs)) (ExplicitTuple anns (map toTupArg es) boxity)
  where
    toTupArg :: Either (EpAnn Bool) (LHsExpr GhcPs) -> HsTupArg GhcPs
    toTupArg (Left ann) = missingTupArg ann
    toTupArg (Right a)  = Present noExtField a

-- Sum
-- mkSumOrTupleExpr l Unboxed (Sum alt arity e) =
--     return $ L l (ExplicitSum noExtField alt arity e)
mkSumOrTupleExpr l@(EpAnn anc anIn csIn) Unboxed (Sum alt arity e barsp barsa) (o, c) = do
    let an = AnnExplicitSum o barsp barsa c
    !cs <- getCommentsFor (locA l)
    return $ L (EpAnn anc anIn (csIn Semi.<> cs)) (ExplicitSum an alt arity e)
mkSumOrTupleExpr l Boxed a@Sum{} _ =
    addFatalError $ mkPlainErrorMsgEnvelope (locA l) $ PsErrUnsupportedBoxedSumExpr a

mkSumOrTuplePat
  :: SrcSpanAnnA -> Boxity -> SumOrTuple (PatBuilder GhcPs) -> (EpaLocation, EpaLocation)
  -> PV (LocatedA (PatBuilder GhcPs))

-- Tuple
mkSumOrTuplePat l boxity (Tuple ps) anns = do
  ps' <- traverse toTupPat ps
  return $ L l (PatBuilderPat (TuplePat anns ps' boxity))
  where
    toTupPat :: Either (EpAnn Bool) (LocatedA (PatBuilder GhcPs)) -> PV (LPat GhcPs)
    -- Ignore the element location so that the error message refers to the
    -- entire tuple. See #19504 (and the discussion) for details.
    toTupPat p = case p of
      Left _ -> addFatalError $
                  mkPlainErrorMsgEnvelope (locA l) PsErrTupleSectionInPat
      Right p' -> checkLPat p'

-- Sum
mkSumOrTuplePat l Unboxed (Sum alt arity p barsb barsa) anns = do
   p' <- checkLPat p
   let an = EpAnnSumPat anns barsb barsa
   return $ L l (PatBuilderPat (SumPat an p' alt arity))
mkSumOrTuplePat l Boxed a@Sum{} _ =
    addFatalError $
      mkPlainErrorMsgEnvelope (locA l) $ PsErrUnsupportedBoxedSumPat a

mkLHsOpTy :: PromotionFlag -> LHsType GhcPs -> LocatedN RdrName -> LHsType GhcPs -> LHsType GhcPs
mkLHsOpTy prom x op y =
  let loc = locA x `combineSrcSpans` locA op `combineSrcSpans` locA y
  in L (noAnnSrcSpan loc) (mkHsOpTy prom x op y)

mkMultAnn :: EpToken "%" -> LHsType GhcPs -> HsMultAnn GhcPs
mkMultAnn pct t@(L _ (HsTyLit _ (HsNumTy (SourceText (unpackFS -> "1")) 1)))
  -- See #18888 for the use of (SourceText "1") above
  = HsPct1Ann pct1
  where
    -- The location of "%" combined with the location of "1".
    pct1 :: EpToken "%1"
    pct1 = epTokenWidenR pct (locA (getLoc t))
mkMultAnn pct t = HsMultAnn pct t

mkTokenLocation :: SrcSpan -> TokenLocation
mkTokenLocation (UnhelpfulSpan _) = NoTokenLoc
mkTokenLocation (RealSrcSpan r mb) = TokenLoc (EpaSpan (RealSrcSpan r mb))

-- Precondition: the EpToken has EpaSpan, never EpaDelta.
epTokenWidenR :: EpToken tok -> SrcSpan -> EpToken tok'
epTokenWidenR NoEpTok _ = NoEpTok
epTokenWidenR (EpTok l) (UnhelpfulSpan _) = EpTok l
epTokenWidenR (EpTok (EpaSpan s1)) s2 = EpTok (EpaSpan (combineSrcSpans s1 s2))
epTokenWidenR (EpTok EpaDelta{}) _ =
  -- Never happens because the parser does not produce EpaDelta.
  panic "epTokenWidenR: EpaDelta"

-----------------------------------------------------------------------------
-- Token symbols

starSym :: Bool -> FastString
starSym True = fsLit "★"
starSym False = fsLit "*"

-----------------------------------------
-- Bits and pieces for RecordDotSyntax.

mkRdrGetField :: LHsExpr GhcPs -> LocatedAn NoEpAnns (DotFieldOcc GhcPs)
  -> HsExpr GhcPs
mkRdrGetField arg field =
  HsGetField {
      gf_ext = NoExtField
    , gf_expr = arg
    , gf_field = field
    }

mkRdrProjection :: NonEmpty (LocatedAn NoEpAnns (DotFieldOcc GhcPs)) -> AnnProjection -> HsExpr GhcPs
mkRdrProjection flds anns =
  HsProjection {
      proj_ext = anns
    , proj_flds = fmap unLoc flds
    }

mkRdrProjUpdate :: SrcSpanAnnA -> Located [LocatedAn NoEpAnns (DotFieldOcc GhcPs)]
                -> LHsExpr GhcPs -> Bool -> Maybe (EpToken "=")
                -> LHsRecProj GhcPs (LHsExpr GhcPs)
mkRdrProjUpdate _ (L _ []) _ _ _ = panic "mkRdrProjUpdate: The impossible has happened!"
mkRdrProjUpdate loc (L l flds) arg isPun anns =
  L loc HsFieldBind {
      hfbAnn = anns
    , hfbLHS = L (noAnnSrcSpan l) (FieldLabelStrings flds)
    , hfbRHS = arg
    , hfbPun = isPun
  }

-----------------------------------------------------------------------------
-- Tuple and list punning

punsAllowed :: P Bool
punsAllowed = getBit ListTuplePunsBit

-- | Check whether @ListTuplePuns@ is enabled and return the first arg if it is,
-- the second arg otherwise.
punsIfElse :: a -> a -> P a
punsIfElse enabled disabled = do
  allowed <- punsAllowed
  pure (if allowed then enabled else disabled)

-- | Emit an error of type 'PsErrInvalidPun' with a location from @start@ to
-- @end@ if the extension @ListTuplePuns@ is disabled.
--
-- This is used in Parser.y to guard rules that require punning.
requireLTPuns :: PsErrPunDetails -> Located a -> Located b -> P ()
requireLTPuns err start end =
  unlessM punsAllowed $ do
    addError (mkPlainErrorMsgEnvelope loc (PsErrInvalidPun err))
  where
    loc = (combineSrcSpans (getLoc start) (getLoc end))

-- | Call a parser with a span and its comments given by a start and end token.
withCombinedComments ::
  HasLoc l1 =>
  HasLoc l2 =>
  l1 ->
  l2 ->
  (SrcSpan -> P a) ->
  P (LocatedA a)
withCombinedComments start end use = do
  cs <- getCommentsFor fullSpan
  a <- use fullSpan
  pure (L (EpAnn (spanAsAnchor fullSpan) noAnn cs) a)
  where
    fullSpan = combineSrcSpans (getHasLoc start) (getHasLoc end)

-- | Decide whether to parse tuple syntax @(Int, Double)@ in a type as a
-- type or data constructor, based on the extension @ListTuplePuns@.
-- The case with an explicit promotion quote, @'(Int, Double)@, is handled
-- by 'mkExplicitTupleTy'.
mkTupleSyntaxTy :: EpaLocation
                -> [LocatedA (HsType GhcPs)]
                -> EpaLocation
                -> P (HsType GhcPs)
mkTupleSyntaxTy parOpen args parClose =
  punsIfElse enabled disabled
  where
    enabled =
      HsTupleTy annParen HsBoxedOrConstraintTuple args
    disabled =
      HsExplicitTupleTy annsKeyword args

    annParen = AnnParen AnnParens parOpen parClose
    annsKeyword = (NoEpTok, EpTok parOpen, EpTok parClose)

-- | Decide whether to parse tuple con syntax @(,)@ in a type as a
-- type or data constructor, based on the extension @ListTuplePuns@.
-- The case with an explicit promotion quote, @'(,)@, is handled
-- by the rule @SIMPLEQUOTE sysdcon_nolist@ in @atype@.
mkTupleSyntaxTycon :: Boxity -> Int -> P RdrName
mkTupleSyntaxTycon boxity n =
  punsIfElse
    (getRdrName (tupleTyCon boxity n))
    (getRdrName (tupleDataCon boxity n))

-- | Decide whether to parse list tycon syntax @[]@ in a type as a type or data
-- constructor, based on the extension @ListTuplePuns@.
-- The case with an explicit promotion quote, @'[]@, is handled by
-- 'mkExplicitListTy'.
mkListSyntaxTy0 :: EpaLocation
                -> EpaLocation
                -> SrcSpan
                -> P (HsType GhcPs)
mkListSyntaxTy0 brkOpen brkClose span =
  punsIfElse enabled disabled
  where
    enabled = HsTyVar noAnn NotPromoted rn

    -- attach the comments only to the RdrName since it's the innermost AST node
    rn = L (EpAnn fullLoc rdrNameAnn emptyComments) listTyCon_RDR

    disabled =
      HsExplicitListTy annsKeyword NotPromoted []

    rdrNameAnn = NameAnnOnly NameSquare brkOpen brkClose []
    annsKeyword = (NoEpTok, EpTok brkOpen, EpTok brkClose)
    fullLoc = EpaSpan span

-- | Decide whether to parse list type syntax @[Int]@ in a type as a
-- type or data constructor, based on the extension @ListTuplePuns@.
-- The case with an explicit promotion quote, @'[Int]@, is handled
-- by 'mkExplicitListTy'.
mkListSyntaxTy1 :: EpaLocation
                -> LocatedA (HsType GhcPs)
                -> EpaLocation
                -> P (HsType GhcPs)
mkListSyntaxTy1 brkOpen t brkClose =
  punsIfElse enabled disabled
  where
    enabled = HsListTy annParen t

    disabled =
      HsExplicitListTy annsKeyword NotPromoted [t]

    annsKeyword = (NoEpTok, EpTok brkOpen, EpTok brkClose)
    annParen = AnnParen AnnParensSquare brkOpen brkClose
