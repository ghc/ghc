{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

--
--  (c) The University of Glasgow 2002-2006
--

-- Functions over HsSyn specialised to RdrName.

module GHC.Parser.PostProcess (
        mkHsOpApp,
        mkHsIntegral, mkHsFractional, mkHsIsString,
        mkHsDo, mkSpliceDecl,
        mkRoleAnnotDecl,
        mkClassDecl,
        mkTyData, mkDataFamInst,
        mkTySynonym, mkTyFamInstEqn,
        mkStandaloneKindSig,
        mkTyFamInst,
        mkFamDecl,
        mkInlinePragma,
        mkPatSynMatchGroup,
        mkRecConstrOrUpdate, -- HsExp -> [HsFieldUpdate] -> P HsExp
        mkTyClD, mkInstD,
        mkRdrRecordCon, mkRdrRecordUpd,
        setRdrNameSpace,
        fromSpecTyVarBndr, fromSpecTyVarBndrs,

        cvBindGroup,
        cvBindsAndSigs,
        cvTopDecls,
        placeHolderPunRhs,

        -- Stuff to do with Foreign declarations
        mkImport,
        parseCImport,
        mkExport,
        mkExtName,    -- RdrName -> CLabelString
        mkGadtDecl,   -- [Located RdrName] -> LHsType RdrName -> ConDecl RdrName
        mkConDeclH98,

        -- Bunch of functions in the parser monad for
        -- checking and constructing values
        checkImportDecl,
        checkExpBlockArguments, checkCmdBlockArguments,
        checkPrecP,           -- Int -> P Int
        checkContext,         -- HsType -> P HsContext
        checkPattern,         -- HsExp -> P HsPat
        checkPattern_hints,
        checkMonadComp,       -- P (HsStmtContext GhcPs)
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
        mkMultTy,

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
        failOpNotEnabledImportQualifiedPost,
        failOpImportQualifiedTwice,

        SumOrTuple (..),

        -- Expression/command/pattern ambiguity resolution
        PV,
        runPV,
        ECP(ECP, unECP),
        DisambInfixOp(..),
        DisambECP(..),
        ecpFromExp,
        ecpFromCmd,
        PatBuilder,

        -- Type/datacon ambiguity resolution
        DisambTD(..),
        addUnpackednessP,
        dataConBuilderCon,
        dataConBuilderDetails,
    ) where

import GHC.Prelude
import GHC.Hs           -- Lots of it
import GHC.Core.TyCon          ( TyCon, isTupleTyCon, tyConSingleDataCon_maybe )
import GHC.Core.DataCon        ( DataCon, dataConTyCon )
import GHC.Core.ConLike        ( ConLike(..) )
import GHC.Core.Coercion.Axiom ( Role, fsFromRole )
import GHC.Types.Name.Reader
import GHC.Types.Name
import GHC.Unit.Module (ModuleName)
import GHC.Types.Basic
import GHC.Types.Fixity
import GHC.Types.SourceText
import GHC.Parser.Types
import GHC.Parser.Lexer
import GHC.Parser.Errors
import GHC.Utils.Lexeme ( isLexCon )
import GHC.Types.TyThing
import GHC.Core.Type    ( unrestrictedFunTyCon, Specificity(..) )
import GHC.Builtin.Types( cTupleTyConName, tupleTyCon, tupleDataCon,
                          nilDataConName, nilDataConKey,
                          listTyConName, listTyConKey, eqTyCon_RDR )
import GHC.Types.ForeignCall
import GHC.Types.SrcLoc
import GHC.Types.Unique ( hasKey )
import GHC.Data.OrdList
import GHC.Utils.Outputable as Outputable
import GHC.Data.FastString
import GHC.Data.Maybe
import GHC.Data.Bag
import GHC.Utils.Misc
import GHC.Parser.Annotation
import Data.List
import Data.Foldable
import GHC.Driver.Flags ( WarningFlag(..) )
import GHC.Utils.Panic

import Control.Monad
import Text.ParserCombinators.ReadP as ReadP
import Data.Char
import Data.Data       ( dataTypeOf, fromConstr, dataTypeConstrs )
import Data.Kind       ( Type )

#include "HsVersions.h"


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
            -> LayoutInfo
            -> P (LTyClDecl GhcPs)

mkClassDecl loc (L _ (mcxt, tycl_hdr)) fds where_cls layoutInfo
  = do { (binds, sigs, ats, at_defs, _, docs) <- cvBindsAndSigs where_cls
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; (cls, tparams, fixity, ann) <- checkTyClHdr True tycl_hdr
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars,annst) <- checkTyVars (text "class") whereDots cls tparams
       ; addAnnsAt loc annst -- Add any API Annotations to the top SrcSpan
       ; return (L loc (ClassDecl { tcdCExt = layoutInfo
                                  , tcdCtxt = cxt
                                  , tcdLName = cls, tcdTyVars = tyvars
                                  , tcdFixity = fixity
                                  , tcdFDs = snd (unLoc fds)
                                  , tcdSigs = mkClassOpSigs sigs
                                  , tcdMeths = binds
                                  , tcdATs = ats, tcdATDefs = at_defs
                                  , tcdDocs  = docs })) }

mkTyData :: SrcSpan
         -> NewOrData
         -> Maybe (Located CType)
         -> Located (Maybe (LHsContext GhcPs), LHsType GhcPs)
         -> Maybe (LHsKind GhcPs)
         -> [LConDecl GhcPs]
         -> HsDeriving GhcPs
         -> P (LTyClDecl GhcPs)
mkTyData loc new_or_data cType (L _ (mcxt, tycl_hdr))
         ksig data_cons maybe_deriv
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False tycl_hdr
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars, anns) <- checkTyVars (ppr new_or_data) equalsDots tc tparams
       ; addAnnsAt loc anns -- Add any API Annotations to the top SrcSpan
       ; defn <- mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
       ; return (L loc (DataDecl { tcdDExt = noExtField,
                                   tcdLName = tc, tcdTyVars = tyvars,
                                   tcdFixity = fixity,
                                   tcdDataDefn = defn })) }

mkDataDefn :: NewOrData
           -> Maybe (Located CType)
           -> Maybe (LHsContext GhcPs)
           -> Maybe (LHsKind GhcPs)
           -> [LConDecl GhcPs]
           -> HsDeriving GhcPs
           -> P (HsDataDefn GhcPs)
mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
  = do { checkDatatypeContext mcxt
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; return (HsDataDefn { dd_ext = noExtField
                            , dd_ND = new_or_data, dd_cType = cType
                            , dd_ctxt = cxt
                            , dd_cons = data_cons
                            , dd_kindSig = ksig
                            , dd_derivs = maybe_deriv }) }


mkTySynonym :: SrcSpan
            -> LHsType GhcPs  -- LHS
            -> LHsType GhcPs  -- RHS
            -> P (LTyClDecl GhcPs)
mkTySynonym loc lhs rhs
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars, anns) <- checkTyVars (text "type") equalsDots tc tparams
       ; addAnnsAt loc anns -- Add any API Annotations to the top SrcSpan
       ; return (L loc (SynDecl { tcdSExt = noExtField
                                , tcdLName = tc, tcdTyVars = tyvars
                                , tcdFixity = fixity
                                , tcdRhs = rhs })) }

mkStandaloneKindSig
  :: SrcSpan
  -> Located [Located RdrName] -- LHS
  -> LHsSigType GhcPs          -- RHS
  -> P (LStandaloneKindSig GhcPs)
mkStandaloneKindSig loc lhs rhs =
  do { vs <- mapM check_lhs_name (unLoc lhs)
     ; v <- check_singular_lhs (reverse vs)
     ; return $ L loc $ StandaloneKindSig noExtField v rhs }
  where
    check_lhs_name v@(unLoc->name) =
      if isUnqual name && isTcOcc (rdrNameOcc name)
      then return v
      else addFatalError $ PsError (PsErrUnexpectedQualifiedConstructor (unLoc v)) [] (getLoc v)
    check_singular_lhs vs =
      case vs of
        [] -> panic "mkStandaloneKindSig: empty left-hand side"
        [v] -> return v
        _ -> addFatalError $ PsError (PsErrMultipleNamesInStandaloneKindSignature vs) [] (getLoc lhs)

mkTyFamInstEqn :: HsOuterFamEqnTyVarBndrs GhcPs
               -> LHsType GhcPs
               -> LHsType GhcPs
               -> P (TyFamInstEqn GhcPs,[AddAnn])
mkTyFamInstEqn bndrs lhs rhs
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; return (FamEqn { feqn_ext    = noExtField
                        , feqn_tycon  = tc
                        , feqn_bndrs  = bndrs
                        , feqn_pats   = tparams
                        , feqn_fixity = fixity
                        , feqn_rhs    = rhs },
                 ann) }

mkDataFamInst :: SrcSpan
              -> NewOrData
              -> Maybe (Located CType)
              -> (Maybe ( LHsContext GhcPs), HsOuterFamEqnTyVarBndrs GhcPs
                        , LHsType GhcPs)
              -> Maybe (LHsKind GhcPs)
              -> [LConDecl GhcPs]
              -> HsDeriving GhcPs
              -> P (LInstDecl GhcPs)
mkDataFamInst loc new_or_data cType (mcxt, bndrs, tycl_hdr)
              ksig data_cons maybe_deriv
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False tycl_hdr
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; defn <- mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
       ; return (L loc (DataFamInstD noExtField (DataFamInstDecl
                  (FamEqn { feqn_ext    = noExtField
                          , feqn_tycon  = tc
                          , feqn_bndrs  = bndrs
                          , feqn_pats   = tparams
                          , feqn_fixity = fixity
                          , feqn_rhs    = defn })))) }

mkTyFamInst :: SrcSpan
            -> TyFamInstEqn GhcPs
            -> P (LInstDecl GhcPs)
mkTyFamInst loc eqn
  = return (L loc (TyFamInstD noExtField (TyFamInstDecl eqn)))

mkFamDecl :: SrcSpan
          -> FamilyInfo GhcPs
          -> LHsType GhcPs                   -- LHS
          -> Located (FamilyResultSig GhcPs) -- Optional result signature
          -> Maybe (LInjectivityAnn GhcPs)   -- Injectivity annotation
          -> P (LTyClDecl GhcPs)
mkFamDecl loc info lhs ksig injAnn
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars, anns) <- checkTyVars (ppr info) equals_or_where tc tparams
       ; addAnnsAt loc anns -- Add any API Annotations to the top SrcSpan
       ; return (L loc (FamDecl noExtField (FamilyDecl
                                           { fdExt       = noExtField
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

mkSpliceDecl :: LHsExpr GhcPs -> HsDecl GhcPs
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
  | HsSpliceE _ splice@(HsUntypedSplice {}) <- expr
  = SpliceD noExtField (SpliceDecl noExtField (L loc splice) ExplicitSplice)

  | HsSpliceE _ splice@(HsQuasiQuote {}) <- expr
  = SpliceD noExtField (SpliceDecl noExtField (L loc splice) ExplicitSplice)

  | otherwise
  = SpliceD noExtField (SpliceDecl noExtField (L loc (mkUntypedSplice BareSplice lexpr))
                              ImplicitSplice)

mkRoleAnnotDecl :: SrcSpan
                -> Located RdrName                -- type being annotated
                -> [Located (Maybe FastString)]      -- roles
                -> P (LRoleAnnotDecl GhcPs)
mkRoleAnnotDecl loc tycon roles
  = do { roles' <- mapM parse_role roles
       ; return $ L loc $ RoleAnnotDecl noExtField tycon roles' }
  where
    role_data_type = dataTypeOf (undefined :: Role)
    all_roles = map fromConstr $ dataTypeConstrs role_data_type
    possible_roles = [(fsFromRole role, role) | role <- all_roles]

    parse_role (L loc_role Nothing) = return $ L loc_role Nothing
    parse_role (L loc_role (Just role))
      = case lookup role possible_roles of
          Just found_role -> return $ L loc_role $ Just found_role
          Nothing         ->
            let nearby = fuzzyLookup (unpackFS role)
                  (mapFst unpackFS possible_roles)
            in
            addFatalError $ PsError (PsErrIllegalRoleName role nearby) [] loc_role

-- | Converts a list of 'LHsTyVarBndr's annotated with their 'Specificity' to
-- binders without annotations. Only accepts specified variables, and errors if
-- any of the provided binders has an 'InferredSpec' annotation.
fromSpecTyVarBndrs :: [LHsTyVarBndr Specificity GhcPs] -> P [LHsTyVarBndr () GhcPs]
fromSpecTyVarBndrs = mapM fromSpecTyVarBndr

-- | Converts 'LHsTyVarBndr' annotated with its 'Specificity' to one without
-- annotations. Only accepts specified variables, and errors if the provided
-- binder has an 'InferredSpec' annotation.
fromSpecTyVarBndr :: LHsTyVarBndr Specificity GhcPs -> P (LHsTyVarBndr () GhcPs)
fromSpecTyVarBndr bndr = case bndr of
  (L loc (UserTyVar xtv flag idp))     -> (check_spec flag loc)
                                          >> return (L loc $ UserTyVar xtv () idp)
  (L loc (KindedTyVar xtv flag idp k)) -> (check_spec flag loc)
                                          >> return (L loc $ KindedTyVar xtv () idp k)
  where
    check_spec :: Specificity -> SrcSpan -> P ()
    check_spec SpecifiedSpec _   = return ()
    check_spec InferredSpec  loc = addFatalError $ PsError PsErrInferredTypeVarNotAllowed [] loc

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
       ; ASSERT( null fam_ds && null tfam_insts && null dfam_insts)
         return $ ValBinds noExtField mbs sigs }

cvBindsAndSigs :: OrdList (LHsDecl GhcPs)
  -> P (LHsBinds GhcPs, [LSig GhcPs], [LFamilyDecl GhcPs]
          , [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs], [LDocDecl])
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
      addError $ PsError (PsErrDeclSpliceNotAtTopLevel d) [] l
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
                               MG { mg_alts = (L _ mtchs1) } }))
            binds
  | has_args mtchs1
  = go mtchs1 loc1 binds []
  where
    go mtchs loc
       ((L loc2 (ValD _ (FunBind { fun_id = (L _ f2)
                                 , fun_matches =
                                    MG { mg_alts = (L _ mtchs2) } })))
         : binds) _
        | f1 == f2 = go (mtchs2 ++ mtchs)
                        (combineSrcSpans loc loc2) binds []
    go mtchs loc (doc_decl@(L loc2 (DocD {})) : binds) doc_decls
        = let doc_decls' = doc_decl : doc_decls
          in go mtchs (combineSrcSpans loc loc2) binds doc_decls'
    go mtchs loc binds doc_decls
        = ( L loc (makeFunBind fun_id1 (reverse mtchs))
          , (reverse doc_decls) ++ binds)
        -- Reverse the final matches, to get it back in the right order
        -- Do the same thing with the trailing doc comments

getMonoBind bind binds = (bind, binds)

-- Group together adjacent FunBinds for every function.
getMonoBindAll :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
getMonoBindAll [] = []
getMonoBindAll (L l (ValD _ b) : ds) =
  let (L l' b', ds') = getMonoBind (L l b) ds
  in L l' (ValD noExtField b') : getMonoBindAll ds'
getMonoBindAll (d : ds) = d : getMonoBindAll ds

has_args :: [LMatch GhcPs (LHsExpr GhcPs)] -> Bool
has_args []                                  = panic "GHC.Parser.PostProcess.has_args"
has_args (L _ (Match { m_pats = args }) : _) = not (null args)
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
tyConToDataCon :: SrcSpan -> RdrName -> Either PsError (Located RdrName)
tyConToDataCon loc tc
  | isTcOcc occ || isDataOcc occ
  , isLexCon (occNameFS occ)
  = return (L loc (setRdrNameSpace tc srcDataName))

  | otherwise
  = Left $ PsError (PsErrNotADataCon tc) [] loc
  where
    occ = rdrNameOcc tc

mkPatSynMatchGroup :: Located RdrName
                   -> Located (OrdList (LHsDecl GhcPs))
                   -> P (MatchGroup GhcPs (LHsExpr GhcPs))
mkPatSynMatchGroup (L loc patsyn_name) (L _ decls) =
    do { matches <- mapM fromDecl (fromOL decls)
       ; when (null matches) (wrongNumberErr loc)
       ; return $ mkMatchGroup FromSource matches }
  where
    fromDecl (L loc decl@(ValD _ (PatBind _
                         pat@(L _ (ConPat NoExtField ln@(L _ name) details))
                               rhs _))) =
        do { unless (name == patsyn_name) $
               wrongNameBindingErr loc decl
           ; match <- case details of
               PrefixCon _ pats -> return $ Match { m_ext = noExtField
                                                  , m_ctxt = ctxt, m_pats = pats
                                                  , m_grhss = rhs }
                   where
                     ctxt = FunRhs { mc_fun = ln
                                   , mc_fixity = Prefix
                                   , mc_strictness = NoSrcStrict }

               InfixCon p1 p2 -> return $ Match { m_ext = noExtField
                                                , m_ctxt = ctxt
                                                , m_pats = [p1, p2]
                                                , m_grhss = rhs }
                   where
                     ctxt = FunRhs { mc_fun = ln
                                   , mc_fixity = Infix
                                   , mc_strictness = NoSrcStrict }

               RecCon{} -> recordPatSynErr loc pat
           ; return $ L loc match }
    fromDecl (L loc decl) = extraDeclErr loc decl

    extraDeclErr loc decl =
        addFatalError $ PsError (PsErrNoSingleWhereBindInPatSynDecl patsyn_name decl) [] loc

    wrongNameBindingErr loc decl =
      addFatalError $ PsError (PsErrInvalidWhereBindInPatSynDecl patsyn_name decl) [] loc

    wrongNumberErr loc =
      addFatalError $ PsError (PsErrEmptyWhereInPatSynDecl patsyn_name) [] loc

recordPatSynErr :: SrcSpan -> LPat GhcPs -> P a
recordPatSynErr loc pat =
    addFatalError $ PsError (PsErrRecordSyntaxInPatSynDecl pat) [] loc

mkConDeclH98 :: Located RdrName -> Maybe [LHsTyVarBndr Specificity GhcPs]
                -> Maybe (LHsContext GhcPs) -> HsConDeclH98Details GhcPs
                -> ConDecl GhcPs

mkConDeclH98 name mb_forall mb_cxt args
  = ConDeclH98 { con_ext    = noExtField
               , con_name   = name
               , con_forall = noLoc $ isJust mb_forall
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
mkGadtDecl :: [Located RdrName]
           -> LHsSigType GhcPs
           -> P (ConDecl GhcPs, [AddAnn])
mkGadtDecl names ty = do
  let (args, res_ty, anns)
        | L _ (HsFunTy _ _w (L loc (HsRecTy _ rf)) res_ty) <- body_ty
        = (RecConGADT (L loc rf), res_ty, [])
        | otherwise
        = let (arg_types, res_type, anns) = splitHsFunType body_ty
          in (PrefixConGADT arg_types, res_type, anns)

  pure ( ConDeclGADT { con_g_ext  = noExtField
                     , con_names  = names
                     , con_bndrs  = L (getLoc ty) outer_bndrs
                     , con_mb_cxt = mcxt
                     , con_g_args = args
                     , con_res_ty = res_ty
                     , con_doc    = Nothing }
       , anns )
  where
    (outer_bndrs, mcxt, body_ty) = splitLHsGadtTy ty

setRdrNameSpace :: RdrName -> NameSpace -> RdrName
-- ^ This rather gruesome function is used mainly by the parser.
-- When parsing:
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

eitherToP :: MonadP m => Either PsError a -> m a
-- Adapts the Either monad to the P monad
eitherToP (Left err)    = addFatalError err
eitherToP (Right thing) = return thing

checkTyVars :: SDoc -> SDoc -> Located RdrName -> [LHsTypeArg GhcPs]
            -> P ( LHsQTyVars GhcPs  -- the synthesized type variables
                 , [AddAnn] )        -- action which adds annotations
-- ^ Check whether the given list of type parameters are all type variables
-- (possibly with a kind signature).
checkTyVars pp_what equals_or_where tc tparms
  = do { (tvs, anns) <- fmap unzip $ mapM check tparms
       ; return (mkHsQTvs tvs, concat anns) }
  where
    check (HsTypeArg _ ki@(L loc _)) = addFatalError $ PsError (PsErrUnexpectedTypeAppInDecl ki pp_what (unLoc tc)) [] loc
    check (HsValArg ty) = chkParens [] ty
    check (HsArgPar sp) = addFatalError $ PsError (PsErrMalformedDecl pp_what (unLoc tc)) [] sp
        -- Keep around an action for adjusting the annotations of extra parens
    chkParens :: [AddAnn] -> LHsType GhcPs
              -> P (LHsTyVarBndr () GhcPs, [AddAnn])
    chkParens acc (L l (HsParTy _ ty)) = chkParens (mkParensApiAnn l ++ acc) ty
    chkParens acc ty = do
      tv <- chk ty
      return (tv, reverse acc)

        -- Check that the name space is correct!
    chk :: LHsType GhcPs -> P (LHsTyVarBndr () GhcPs)
    chk (L l (HsKindSig _ (L lv (HsTyVar _ _ (L _ tv))) k))
        | isRdrTyVar tv    = return (L l (KindedTyVar noExtField () (L lv tv) k))
    chk (L l (HsTyVar _ _ (L ltv tv)))
        | isRdrTyVar tv    = return (L l (UserTyVar noExtField () (L ltv tv)))
    chk t@(L loc _)
        = addFatalError $ PsError (PsErrUnexpectedTypeInDecl t pp_what (unLoc tc) tparms equals_or_where) [] loc


whereDots, equalsDots :: SDoc
-- Second argument to checkTyVars
whereDots  = text "where ..."
equalsDots = text "= ..."

checkDatatypeContext :: Maybe (LHsContext GhcPs) -> P ()
checkDatatypeContext Nothing = return ()
checkDatatypeContext (Just c)
    = do allowed <- getBit DatatypeContextsBit
         unless allowed $ addError $ PsError (PsErrIllegalDataTypeContext c) [] (getLoc c)

type LRuleTyTmVar = Located RuleTyTmVar
data RuleTyTmVar = RuleTyTmVar (Located RdrName) (Maybe (LHsType GhcPs))
-- ^ Essentially a wrapper for a @RuleBndr GhcPs@

-- turns RuleTyTmVars into RuleBnrs - this is straightforward
mkRuleBndrs :: [LRuleTyTmVar] -> [LRuleBndr GhcPs]
mkRuleBndrs = fmap (fmap cvt_one)
  where cvt_one (RuleTyTmVar v Nothing)    = RuleBndr    noExtField v
        cvt_one (RuleTyTmVar v (Just sig)) =
          RuleBndrSig noExtField v (mkHsPatSigType sig)

-- turns RuleTyTmVars into HsTyVarBndrs - this is more interesting
mkRuleTyVarBndrs :: [LRuleTyTmVar] -> [LHsTyVarBndr () GhcPs]
mkRuleTyVarBndrs = fmap (fmap cvt_one)
  where cvt_one (RuleTyTmVar v Nothing)
          = UserTyVar noExtField () (fmap tm_to_ty v)
        cvt_one (RuleTyTmVar v (Just sig))
          = KindedTyVar noExtField () (fmap tm_to_ty v) sig
    -- takes something in namespace 'varName' to something in namespace 'tvName'
        tm_to_ty (Unqual occ) = Unqual (setOccNameSpace tvName occ)
        tm_to_ty _ = panic "mkRuleTyVarBndrs"

-- See note [Parsing explicit foralls in Rules] in Parser.y
checkRuleTyVarBndrNames :: [LHsTyVarBndr flag GhcPs] -> P ()
checkRuleTyVarBndrNames = mapM_ (check . fmap hsTyVarName)
  where check (L loc (Unqual occ)) =
          -- TODO: don't use string here, OccName has a Unique/FastString
          when ((occNameString occ ==) `any` ["forall","family","role"])
            (addFatalError $ PsError (PsErrParseErrorOnInput occ) [] loc)
        check _ = panic "checkRuleTyVarBndrNames"

checkRecordSyntax :: (MonadP m, Outputable a) => Located a -> m (Located a)
checkRecordSyntax lr@(L loc r)
    = do allowed <- getBit TraditionalRecordSyntaxBit
         unless allowed $ addError $ PsError (PsErrIllegalTraditionalRecordSyntax (ppr r)) [] loc
         return lr

-- | Check if the gadt_constrlist is empty. Only raise parse error for
-- `data T where` to avoid affecting existing error message, see #8258.
checkEmptyGADTs :: Located ([AddAnn], [LConDecl GhcPs])
                -> P (Located ([AddAnn], [LConDecl GhcPs]))
checkEmptyGADTs gadts@(L span (_, []))           -- Empty GADT declaration.
    = do gadtSyntax <- getBit GadtSyntaxBit   -- GADTs implies GADTSyntax
         unless gadtSyntax $ addError $ PsError PsErrIllegalWhereInDataDecl [] span
         return gadts
checkEmptyGADTs gadts = return gadts              -- Ordinary GADT declaration.

checkTyClHdr :: Bool               -- True  <=> class header
                                   -- False <=> type header
             -> LHsType GhcPs
             -> P (Located RdrName,      -- the head symbol (type or class name)
                   [LHsTypeArg GhcPs],      -- parameters of head symbol
                   LexicalFixity,        -- the declaration is in infix format
                   [AddAnn]) -- API Annotation for HsParTy when stripping parens
-- Well-formedness check and decomposition of type and class heads.
-- Decomposes   T ty1 .. tyn   into    (T, [ty1, ..., tyn])
--              Int :*: Bool   into    (:*:, [Int, Bool])
-- returning the pieces
checkTyClHdr is_cls ty
  = goL ty [] [] Prefix
  where
    goL (L l ty) acc ann fix = go l ty acc ann fix

    -- workaround to define '*' despite StarIsType
    go lp (HsParTy _ (L l (HsStarTy _ isUni))) acc ann fix
      = do { addWarning Opt_WarnStarBinder (PsWarnStarBinder l)
           ; let name = mkOccName tcClsName (starSym isUni)
           ; return (L l (Unqual name), acc, fix, (ann ++ mkParensApiAnn lp)) }

    go _ (HsTyVar _ _ ltc@(L _ tc)) acc ann fix
      | isRdrTc tc               = return (ltc, acc, fix, ann)
    go _ (HsOpTy _ t1 ltc@(L _ tc) t2) acc ann _fix
      | isRdrTc tc               = return (ltc, HsValArg t1:HsValArg t2:acc, Infix, ann)
    go l (HsParTy _ ty)    acc ann fix = goL ty acc (ann ++mkParensApiAnn l) fix
    go _ (HsAppTy _ t1 t2) acc ann fix = goL t1 (HsValArg t2:acc) ann fix
    go _ (HsAppKindTy l ty ki) acc ann fix = goL ty (HsTypeArg l ki:acc) ann fix
    go l (HsTupleTy _ HsBoxedOrConstraintTuple ts) [] ann fix
      = return (L l (nameRdrName tup_name), map HsValArg ts, fix, ann)
      where
        arity = length ts
        tup_name | is_cls    = cTupleTyConName arity
                 | otherwise = getName (tupleTyCon Boxed arity)
          -- See Note [Unit tuples] in GHC.Hs.Type  (TODO: is this still relevant?)
    go l _ _ _ _
      = addFatalError $ PsError (PsErrMalformedTyOrClDecl ty) [] l

-- | Yield a parse error if we have a function applied directly to a do block
-- etc. and BlockArguments is not enabled.
checkExpBlockArguments :: LHsExpr GhcPs -> PV ()
checkCmdBlockArguments :: LHsCmd GhcPs -> PV ()
(checkExpBlockArguments, checkCmdBlockArguments) = (checkExpr, checkCmd)
  where
    checkExpr :: LHsExpr GhcPs -> PV ()
    checkExpr expr = case unLoc expr of
      HsDo _ (DoExpr m) _  -> check (PsErrDoInFunAppExpr m)     expr
      HsDo _ (MDoExpr m) _ -> check (PsErrMDoInFunAppExpr m)    expr
      HsLam {}             -> check PsErrLambdaInFunAppExpr     expr
      HsCase {}            -> check PsErrCaseInFunAppExpr       expr
      HsLamCase {}         -> check PsErrLambdaCaseInFunAppExpr expr
      HsLet {}             -> check PsErrLetInFunAppExpr        expr
      HsIf {}              -> check PsErrIfInFunAppExpr         expr
      HsProc {}            -> check PsErrProcInFunAppExpr       expr
      _                    -> return ()

    checkCmd :: LHsCmd GhcPs -> PV ()
    checkCmd cmd = case unLoc cmd of
      HsCmdLam {}  -> check PsErrLambdaCmdInFunAppCmd cmd
      HsCmdCase {} -> check PsErrCaseCmdInFunAppCmd   cmd
      HsCmdIf {}   -> check PsErrIfCmdInFunAppCmd     cmd
      HsCmdLet {}  -> check PsErrLetCmdInFunAppCmd    cmd
      HsCmdDo {}   -> check PsErrDoCmdInFunAppCmd     cmd
      _            -> return ()

    check err a = do
      blockArguments <- getBit BlockArgumentsBit
      unless blockArguments $
        addError $ PsError (err a) [] (getLoc a)

-- | Validate the context constraints and break up a context into a list
-- of predicates.
--
-- @
--     (Eq a, Ord b)        -->  [Eq a, Ord b]
--     Eq a                 -->  [Eq a]
--     (Eq a)               -->  [Eq a]
--     (((Eq a)))           -->  [Eq a]
-- @
checkContext :: LHsType GhcPs -> P ([AddAnn],LHsContext GhcPs)
checkContext (L l orig_t)
  = check [] (L l orig_t)
 where
  check anns (L lp (HsTupleTy _ HsBoxedOrConstraintTuple ts))
    -- (Eq a, Ord b) shows up as a tuple type. Only boxed tuples can
    -- be used as context constraints.
    = return (anns ++ mkParensApiAnn lp,L l ts)                -- Ditto ()

  check anns (L lp1 (HsParTy _ ty))
                                  -- to be sure HsParTy doesn't get into the way
       = check anns' ty
         where anns' = if l == lp1 then anns
                                   else (anns ++ mkParensApiAnn lp1)

  -- no need for anns, returning original
  check _anns _t = return ([],L l [L l orig_t])

checkImportDecl :: Maybe (Located Token)
                -> Maybe (Located Token)
                -> P ()
checkImportDecl mPre mPost = do
  let whenJust mg f = maybe (pure ()) f mg

  importQualifiedPostEnabled <- getBit ImportQualifiedPostBit

  -- Error if 'qualified' found in postpositive position and
  -- 'ImportQualifiedPost' is not in effect.
  whenJust mPost $ \post ->
    when (not importQualifiedPostEnabled) $
      failOpNotEnabledImportQualifiedPost (getLoc post)

  -- Error if 'qualified' occurs in both pre and postpositive
  -- positions.
  whenJust mPost $ \post ->
    when (isJust mPre) $
      failOpImportQualifiedTwice (getLoc post)

  -- Warn if 'qualified' found in prepositive position and
  -- 'Opt_WarnPrepositiveQualifiedModule' is enabled.
  whenJust mPre $ \pre ->
    warnPrepositiveQualifiedModule (getLoc pre)

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: Located (PatBuilder GhcPs) -> P (LPat GhcPs)
checkPattern = runPV . checkLPat

checkPattern_hints :: [Hint] -> PV (Located (PatBuilder GhcPs)) -> P (LPat GhcPs)
checkPattern_hints hints pp = runPV_hints hints (pp >>= checkLPat)

checkLPat :: Located (PatBuilder GhcPs) -> PV (LPat GhcPs)
checkLPat e@(L l _) = checkPat l e [] []

checkPat :: SrcSpan -> Located (PatBuilder GhcPs) -> [HsPatSigType GhcPs] -> [LPat GhcPs]
         -> PV (LPat GhcPs)
checkPat loc (L l e@(PatBuilderVar (L _ c))) tyargs args
  | isRdrDataCon c = return . L loc $ ConPat
      { pat_con_ext = noExtField
      , pat_con = L l c
      , pat_args = PrefixCon tyargs args
      }
  | not (null tyargs) =
      add_hint TypeApplicationsInPatternsOnlyDataCons $
      patFail l (ppr e <+> hsep [text "@" <> ppr t | t <- tyargs])
  | not (null args) && patIsRec c =
      add_hint SuggestRecursiveDo $
      patFail l (ppr e)
checkPat loc (L _ (PatBuilderAppType f t)) tyargs args = do
  checkPat loc f (t : tyargs) args
checkPat loc (L _ (PatBuilderApp f e)) [] args = do
  p <- checkLPat e
  checkPat loc f [] (p : args)
checkPat loc (L _ e) [] [] = do
  p <- checkAPat loc e
  return (L loc p)
checkPat loc e _ _ = patFail loc (ppr e)

checkAPat :: SrcSpan -> PatBuilder GhcPs -> PV (Pat GhcPs)
checkAPat loc e0 = do
 nPlusKPatterns <- getBit NPlusKPatternsBit
 case e0 of
   PatBuilderPat p -> return p
   PatBuilderVar x -> return (VarPat noExtField x)

   -- Overloaded numeric patterns (e.g. f 0 x = x)
   -- Negation is recorded separately, so that the literal is zero or +ve
   -- NB. Negative *primitive* literals are already handled by the lexer
   PatBuilderOverLit pos_lit -> return (mkNPat (L loc pos_lit) Nothing)

   -- n+k patterns
   PatBuilderOpApp
           (L nloc (PatBuilderVar (L _ n)))
           (L _ plus)
           (L lloc (PatBuilderOverLit lit@(OverLit {ol_val = HsIntegral {}})))
                      | nPlusKPatterns && (plus == plus_RDR)
                      -> return (mkNPlusKPat (L nloc n) (L lloc lit))

   -- Improve error messages for the @-operator when the user meant an @-pattern
   PatBuilderOpApp _ op _ | opIsAt (unLoc op) -> do
     addError $ PsError PsErrAtInPatPos [] (getLoc op)
     return (WildPat noExtField)

   PatBuilderOpApp l (L cl c) r
     | isRdrDataCon c -> do
         l <- checkLPat l
         r <- checkLPat r
         return $ ConPat
           { pat_con_ext = noExtField
           , pat_con = L cl c
           , pat_args = InfixCon l r
           }

   PatBuilderPar e    -> checkLPat e >>= (return . (ParPat noExtField))
   _           -> patFail loc (ppr e0)

placeHolderPunRhs :: DisambECP b => PV (Located b)
-- The RHS of a punned record field will be filled in by the renamer
-- It's better not to make it an error, in case we want to print it when
-- debugging
placeHolderPunRhs = mkHsVarPV (noLoc pun_RDR)

plus_RDR, pun_RDR :: RdrName
plus_RDR = mkUnqual varName (fsLit "+") -- Hack
pun_RDR  = mkUnqual varName (fsLit "pun-right-hand-side")

checkPatField :: LHsRecField GhcPs (Located (PatBuilder GhcPs))
              -> PV (LHsRecField GhcPs (LPat GhcPs))
checkPatField (L l fld) = do p <- checkLPat (hsRecFieldArg fld)
                             return (L l (fld { hsRecFieldArg = p }))

patFail :: SrcSpan -> SDoc -> PV a
patFail loc e = addFatalError $ PsError (PsErrParseErrorInPat e) [] loc

patIsRec :: RdrName -> Bool
patIsRec e = e == mkUnqual varName (fsLit "rec")

---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: Located (PatBuilder GhcPs)
            -> Maybe (LHsType GhcPs)
            -> Located (a,GRHSs GhcPs (LHsExpr GhcPs))
            -> P ([AddAnn],HsBind GhcPs)

checkValDef lhs (Just sig) grhss
        -- x :: ty = rhs  parses as a *pattern* binding
  = do lhs' <- runPV $ mkHsTySigPV (combineLocs lhs sig) lhs sig >>= checkLPat
       checkPatBind lhs' grhss

checkValDef lhs Nothing g@(L l (_,grhss))
  = do  { mb_fun <- isFunLhs lhs
        ; case mb_fun of
            Just (fun, is_infix, pats, ann) ->
              checkFunBind NoSrcStrict ann (getLoc lhs)
                           fun is_infix pats (L l grhss)
            Nothing -> do
              lhs' <- checkPattern lhs
              checkPatBind lhs' g }

checkFunBind :: SrcStrictness
             -> [AddAnn]
             -> SrcSpan
             -> Located RdrName
             -> LexicalFixity
             -> [Located (PatBuilder GhcPs)]
             -> Located (GRHSs GhcPs (LHsExpr GhcPs))
             -> P ([AddAnn],HsBind GhcPs)
checkFunBind strictness ann lhs_loc fun is_infix pats (L rhs_span grhss)
  = do  ps <- runPV_hints param_hints (mapM checkLPat pats)
        let match_span = combineSrcSpans lhs_loc rhs_span
        -- Add back the annotations stripped from any HsPar values in the lhs
        -- mapM_ (\a -> a match_span) ann
        return (ann, makeFunBind fun
                  [L match_span (Match { m_ext = noExtField
                                       , m_ctxt = FunRhs
                                           { mc_fun    = fun
                                           , mc_fixity = is_infix
                                           , mc_strictness = strictness }
                                       , m_pats = ps
                                       , m_grhss = grhss })])
        -- The span of the match covers the entire equation.
        -- That isn't quite right, but it'll do for now.
  where
    param_hints
      | Infix <- is_infix = [SuggestInfixBindMaybeAtPat (unLoc fun)]
      | otherwise         = []

makeFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)]
            -> HsBind GhcPs
-- Like GHC.Hs.Utils.mkFunBind, but we need to be able to set the fixity too
makeFunBind fn ms
  = FunBind { fun_ext = noExtField,
              fun_id = fn,
              fun_matches = mkMatchGroup FromSource ms,
              fun_tick = [] }

-- See Note [FunBind vs PatBind]
checkPatBind :: LPat GhcPs
             -> Located (a,GRHSs GhcPs (LHsExpr GhcPs))
             -> P ([AddAnn],HsBind GhcPs)
checkPatBind lhs (L rhs_span (_,grhss))
    | BangPat _ p <- unLoc lhs
    , VarPat _ v <- unLoc p
    = return ([], makeFunBind v [L match_span (m v)])
  where
    match_span = combineSrcSpans (getLoc lhs) rhs_span
    m v = Match { m_ext = noExtField
                , m_ctxt = FunRhs { mc_fun    = v
                                  , mc_fixity = Prefix
                                  , mc_strictness = SrcStrict }
                , m_pats = []
                , m_grhss = grhss }

checkPatBind lhs (L _ (_,grhss))
  = return ([],PatBind noExtField lhs grhss ([],[]))

checkValSigLhs :: LHsExpr GhcPs -> P (Located RdrName)
checkValSigLhs (L _ (HsVar _ lrdr@(L _ v)))
  | isUnqual v
  , not (isDataOcc (rdrNameOcc v))
  = return lrdr

checkValSigLhs lhs@(L l _)
  = addFatalError $ PsError (PsErrInvalidTypeSignature lhs) [] l

checkDoAndIfThenElse
  :: (Outputable a, Outputable b, Outputable c)
  => (a -> Bool -> b -> Bool -> c -> PsErrorDesc)
  -> Located a -> Bool -> Located b -> Bool -> Located c -> PV ()
checkDoAndIfThenElse err guardExpr semiThen thenExpr semiElse elseExpr
 | semiThen || semiElse = do
      doAndIfThenElse <- getBit DoAndIfThenElseBit
      let e   = err (unLoc guardExpr)
                    semiThen (unLoc thenExpr)
                    semiElse (unLoc elseExpr)
          loc = combineLocs guardExpr elseExpr

      unless doAndIfThenElse $ addError (PsError e [] loc)
  | otherwise = return ()

isFunLhs :: Located (PatBuilder GhcPs)
      -> P (Maybe (Located RdrName, LexicalFixity, [Located (PatBuilder GhcPs)],[AddAnn]))
-- A variable binding is parsed as a FunBind.
-- Just (fun, is_infix, arg_pats) if e is a function LHS
isFunLhs e = go e [] []
 where
   go (L loc (PatBuilderVar (L _ f))) es ann
       | not (isRdrDataCon f)        = return (Just (L loc f, Prefix, es, ann))
   go (L _ (PatBuilderApp f e)) es       ann = go f (e:es) ann
   go (L l (PatBuilderPar e))   es@(_:_) ann = go e es (ann ++ mkParensApiAnn l)
   go (L loc (PatBuilderOpApp l (L loc' op) r)) es ann
        | not (isRdrDataCon op)         -- We have found the function!
        = return (Just (L loc' op, Infix, (l:r:es), ann))
        | otherwise                     -- Infix data con; keep going
        = do { mb_l <- go l es ann
             ; case mb_l of
                 Just (op', Infix, j : k : es', ann')
                   -> return (Just (op', Infix, j : op_app : es', ann'))
                   where
                     op_app = L loc (PatBuilderOpApp k
                               (L loc' op) r)
                 _ -> return Nothing }
   go _ _ _ = return Nothing

mkBangTy :: SrcStrictness -> LHsType GhcPs -> HsType GhcPs
mkBangTy strictness =
  HsBangTy noExtField (HsSrcBang NoSourceText NoSrcUnpack strictness)

-- | Result of parsing @{-\# UNPACK \#-}@ or @{-\# NOUNPACK \#-}@.
data UnpackednessPragma =
  UnpackednessPragma [AddAnn] SourceText SrcUnpackedness

-- | Annotate a type with either an @{-\# UNPACK \#-}@ or a @{-\# NOUNPACK \#-}@ pragma.
addUnpackednessP :: MonadP m => Located UnpackednessPragma -> LHsType GhcPs -> m (LHsType GhcPs)
addUnpackednessP (L lprag (UnpackednessPragma anns prag unpk)) ty = do
    let l' = combineSrcSpans lprag (getLoc ty)
        t' = addUnpackedness ty
    addAnnsAt l' anns
    return (L l' t')
  where
    -- If we have a HsBangTy that only has a strictness annotation,
    -- such as ~T or !T, then add the pragma to the existing HsBangTy.
    --
    -- Otherwise, wrap the type in a new HsBangTy constructor.
    addUnpackedness (L _ (HsBangTy x bang t))
      | HsSrcBang NoSourceText NoSrcUnpack strictness <- bang
      = HsBangTy x (HsSrcBang prag unpk strictness) t
    addUnpackedness t
      = HsBangTy noExtField (HsSrcBang prag unpk NoSrcStrict) t

---------------------------------------------------------------------------
-- | Check for monad comprehensions
--
-- If the flag MonadComprehensions is set, return a 'MonadComp' context,
-- otherwise use the usual 'ListComp' context

checkMonadComp :: PV (HsStmtContext GhcRn)
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
  ECP { unECP :: forall b. DisambECP b => PV (Located b) }

ecpFromExp :: LHsExpr GhcPs -> ECP
ecpFromExp a = ECP (ecpFromExp' a)

ecpFromCmd :: LHsCmd GhcPs -> ECP
ecpFromCmd a = ECP (ecpFromCmd' a)

-- | Disambiguate infix operators.
-- See Note [Ambiguous syntactic categories]
class DisambInfixOp b where
  mkHsVarOpPV :: Located RdrName -> PV (Located b)
  mkHsConOpPV :: Located RdrName -> PV (Located b)
  mkHsInfixHolePV :: SrcSpan -> PV (Located b)

instance DisambInfixOp (HsExpr GhcPs) where
  mkHsVarOpPV v = return $ L (getLoc v) (HsVar noExtField v)
  mkHsConOpPV v = return $ L (getLoc v) (HsVar noExtField v)
  mkHsInfixHolePV l = return $ L l hsHoleExpr

instance DisambInfixOp RdrName where
  mkHsConOpPV (L l v) = return $ L l v
  mkHsVarOpPV (L l v) = return $ L l v
  mkHsInfixHolePV l   = addFatalError $ PsError PsErrInvalidInfixHole [] l

-- | Disambiguate constructs that may appear when we do not know ahead of time whether we are
-- parsing an expression, a command, or a pattern.
-- See Note [Ambiguous syntactic categories]
class b ~ (Body b) GhcPs => DisambECP b where
  -- | See Note [Body in DisambECP]
  type Body b :: Type -> Type
  -- | Return a command without ambiguity, or fail in a non-command context.
  ecpFromCmd' :: LHsCmd GhcPs -> PV (Located b)
  -- | Return an expression without ambiguity, or fail in a non-expression context.
  ecpFromExp' :: LHsExpr GhcPs -> PV (Located b)
  -- | Disambiguate "\... -> ..." (lambda)
  mkHsLamPV :: SrcSpan -> MatchGroup GhcPs (Located b) -> PV (Located b)
  -- | Disambiguate "let ... in ..."
  mkHsLetPV :: SrcSpan -> LHsLocalBinds GhcPs -> Located b -> PV (Located b)
  -- | Infix operator representation
  type InfixOp b
  -- | Bring superclass constraints on InfixOp into scope.
  -- See Note [UndecidableSuperClasses for associated types]
  superInfixOp :: (DisambInfixOp (InfixOp b) => PV (Located b )) -> PV (Located b)
  -- | Disambiguate "f # x" (infix operator)
  mkHsOpAppPV :: SrcSpan -> Located b -> Located (InfixOp b) -> Located b -> PV (Located b)
  -- | Disambiguate "case ... of ..."
  mkHsCasePV :: SrcSpan -> LHsExpr GhcPs -> MatchGroup GhcPs (Located b) -> PV (Located b)
  -- | Disambiguate @\\case ...@ (lambda case)
  mkHsLamCasePV :: SrcSpan -> MatchGroup GhcPs (Located b) -> PV (Located b)
  -- | Function argument representation
  type FunArg b
  -- | Bring superclass constraints on FunArg into scope.
  -- See Note [UndecidableSuperClasses for associated types]
  superFunArg :: (DisambECP (FunArg b) => PV (Located b)) -> PV (Located b)
  -- | Disambiguate "f x" (function application)
  mkHsAppPV :: SrcSpan -> Located b -> Located (FunArg b) -> PV (Located b)
  -- | Disambiguate "f @t" (visible type application)
  mkHsAppTypePV :: SrcSpan -> Located b -> LHsType GhcPs -> PV (Located b)
  -- | Disambiguate "if ... then ... else ..."
  mkHsIfPV :: SrcSpan
         -> LHsExpr GhcPs
         -> Bool  -- semicolon?
         -> Located b
         -> Bool  -- semicolon?
         -> Located b
         -> PV (Located b)
  -- | Disambiguate "do { ... }" (do notation)
  mkHsDoPV ::
    SrcSpan ->
    Maybe ModuleName ->
    Located [LStmt GhcPs (Located b)] ->
    PV (Located b)
  -- | Disambiguate "( ... )" (parentheses)
  mkHsParPV :: SrcSpan -> Located b -> PV (Located b)
  -- | Disambiguate a variable "f" or a data constructor "MkF".
  mkHsVarPV :: Located RdrName -> PV (Located b)
  -- | Disambiguate a monomorphic literal
  mkHsLitPV :: Located (HsLit GhcPs) -> PV (Located b)
  -- | Disambiguate an overloaded literal
  mkHsOverLitPV :: Located (HsOverLit GhcPs) -> PV (Located b)
  -- | Disambiguate a wildcard
  mkHsWildCardPV :: SrcSpan -> PV (Located b)
  -- | Disambiguate "a :: t" (type annotation)
  mkHsTySigPV :: SrcSpan -> Located b -> LHsType GhcPs -> PV (Located b)
  -- | Disambiguate "[a,b,c]" (list syntax)
  mkHsExplicitListPV :: SrcSpan -> [Located b] -> PV (Located b)
  -- | Disambiguate "$(...)" and "[quasi|...|]" (TH splices)
  mkHsSplicePV :: Located (HsSplice GhcPs) -> PV (Located b)
  -- | Disambiguate "f { a = b, ... }" syntax (record construction and record updates)
  mkHsRecordPV ::
    SrcSpan ->
    SrcSpan ->
    Located b ->
    ([LHsRecField GhcPs (Located b)], Maybe SrcSpan) ->
    PV (Located b)
  -- | Disambiguate "-a" (negation)
  mkHsNegAppPV :: SrcSpan -> Located b -> PV (Located b)
  -- | Disambiguate "(# a)" (right operator section)
  mkHsSectionR_PV :: SrcSpan -> Located (InfixOp b) -> Located b -> PV (Located b)
  -- | Disambiguate "(a -> b)" (view pattern)
  mkHsViewPatPV :: SrcSpan -> LHsExpr GhcPs -> Located b -> PV (Located b)
  -- | Disambiguate "a@b" (as-pattern)
  mkHsAsPatPV :: SrcSpan -> Located RdrName -> Located b -> PV (Located b)
  -- | Disambiguate "~a" (lazy pattern)
  mkHsLazyPatPV :: SrcSpan -> Located b -> PV (Located b)
  -- | Disambiguate "!a" (bang pattern)
  mkHsBangPatPV :: SrcSpan -> Located b -> PV (Located b)
  -- | Disambiguate tuple sections and unboxed sums
  mkSumOrTuplePV :: SrcSpan -> Boxity -> SumOrTuple b -> PV (Located b)
  -- | Validate infixexp LHS to reject unwanted {-# SCC ... #-} pragmas
  rejectPragmaPV :: Located b -> PV ()


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
  ecpFromExp' (L l e) = cmdFail l (ppr e)
  mkHsLamPV l mg = return $ L l (HsCmdLam noExtField mg)
  mkHsLetPV l bs e = return $ L l (HsCmdLet noExtField bs e)
  type InfixOp (HsCmd GhcPs) = HsExpr GhcPs
  superInfixOp m = m
  mkHsOpAppPV l c1 op c2 = do
    let cmdArg c = L (getLoc c) $ HsCmdTop noExtField c
    return $ L l $ HsCmdArrForm noExtField op Infix Nothing [cmdArg c1, cmdArg c2]
  mkHsCasePV l c mg = return $ L l (HsCmdCase noExtField c mg)
  mkHsLamCasePV l mg = return $ L l (HsCmdLamCase noExtField mg)
  type FunArg (HsCmd GhcPs) = HsExpr GhcPs
  superFunArg m = m
  mkHsAppPV l c e = do
    checkCmdBlockArguments c
    checkExpBlockArguments e
    return $ L l (HsCmdApp noExtField c e)
  mkHsAppTypePV l c t = cmdFail l (ppr c <+> text "@" <> ppr t)
  mkHsIfPV l c semi1 a semi2 b = do
    checkDoAndIfThenElse PsErrSemiColonsInCondCmd c semi1 a semi2 b
    return $ L l (mkHsCmdIf c a b)
  mkHsDoPV l Nothing stmts = return $ L l (HsCmdDo noExtField stmts)
  mkHsDoPV l (Just m)    _ = addFatalError $ PsError (PsErrQualifiedDoInCmd m) [] l
  mkHsParPV l c = return $ L l (HsCmdPar noExtField c)
  mkHsVarPV (L l v) = cmdFail l (ppr v)
  mkHsLitPV (L l a) = cmdFail l (ppr a)
  mkHsOverLitPV (L l a) = cmdFail l (ppr a)
  mkHsWildCardPV l = cmdFail l (text "_")
  mkHsTySigPV l a sig = cmdFail l (ppr a <+> text "::" <+> ppr sig)
  mkHsExplicitListPV l xs = cmdFail l $
    brackets (fsep (punctuate comma (map ppr xs)))
  mkHsSplicePV (L l sp) = cmdFail l (ppr sp)
  mkHsRecordPV l _ a (fbinds, ddLoc) = cmdFail l $
    ppr a <+> ppr (mk_rec_fields fbinds ddLoc)
  mkHsNegAppPV l a = cmdFail l (text "-" <> ppr a)
  mkHsSectionR_PV l op c = cmdFail l $
    let pp_op = fromMaybe (panic "cannot print infix operator")
                          (ppr_infix_expr (unLoc op))
    in pp_op <> ppr c
  mkHsViewPatPV l a b = cmdFail l $
    ppr a <+> text "->" <+> ppr b
  mkHsAsPatPV l v c = cmdFail l $
    pprPrefixOcc (unLoc v) <> text "@" <> ppr c
  mkHsLazyPatPV l c = cmdFail l $
    text "~" <> ppr c
  mkHsBangPatPV l c = cmdFail l $
    text "!" <> ppr c
  mkSumOrTuplePV l boxity a = cmdFail l (pprSumOrTuple boxity a)
  rejectPragmaPV _ = return ()

cmdFail :: SrcSpan -> SDoc -> PV a
cmdFail loc e = addFatalError $ PsError (PsErrParseErrorInCmd e) [] loc

instance DisambECP (HsExpr GhcPs) where
  type Body (HsExpr GhcPs) = HsExpr
  ecpFromCmd' (L l c) = do
    addError $ PsError (PsErrArrowCmdInExpr c) [] l
    return (L l hsHoleExpr)
  ecpFromExp' = return
  mkHsLamPV l mg = return $ L l (HsLam noExtField mg)
  mkHsLetPV l bs c = return $ L l (HsLet noExtField bs c)
  type InfixOp (HsExpr GhcPs) = HsExpr GhcPs
  superInfixOp m = m
  mkHsOpAppPV l e1 op e2 =
    return $ L l $ OpApp noExtField e1 op e2
  mkHsCasePV l e mg = return $ L l (HsCase noExtField e mg)
  mkHsLamCasePV l mg = return $ L l (HsLamCase noExtField mg)
  type FunArg (HsExpr GhcPs) = HsExpr GhcPs
  superFunArg m = m
  mkHsAppPV l e1 e2 = do
    checkExpBlockArguments e1
    checkExpBlockArguments e2
    return $ L l (HsApp noExtField e1 e2)
  mkHsAppTypePV l e t = do
    checkExpBlockArguments e
    return $ L l (HsAppType noExtField e (mkHsWildCardBndrs t))
  mkHsIfPV l c semi1 a semi2 b = do
    checkDoAndIfThenElse PsErrSemiColonsInCondExpr c semi1 a semi2 b
    return $ L l (mkHsIf c a b)
  mkHsDoPV l mod stmts = return $ L l (HsDo noExtField (DoExpr mod) stmts)
  mkHsParPV l e = return $ L l (HsPar noExtField e)
  mkHsVarPV v@(getLoc -> l) = return $ L l (HsVar noExtField v)
  mkHsLitPV (L l a) = return $ L l (HsLit noExtField a)
  mkHsOverLitPV (L l a) = return $ L l (HsOverLit noExtField a)
  mkHsWildCardPV l = return $ L l hsHoleExpr
  mkHsTySigPV l a sig = return $ L l (ExprWithTySig noExtField a (hsTypeToHsSigWcType sig))
  mkHsExplicitListPV l xs = return $ L l (ExplicitList noExtField Nothing xs)
  mkHsSplicePV sp = return $ mapLoc (HsSpliceE noExtField) sp
  mkHsRecordPV l lrec a (fbinds, ddLoc) = do
    r <- mkRecConstrOrUpdate a lrec (fbinds, ddLoc)
    checkRecordSyntax (L l r)
  mkHsNegAppPV l a = return $ L l (NegApp noExtField a noSyntaxExpr)
  mkHsSectionR_PV l op e = return $ L l (SectionR noExtField op e)
  mkHsViewPatPV l a b = addError (PsError (PsErrViewPatInExpr a b) [] l)
                        >> return (L l hsHoleExpr)
  mkHsAsPatPV l v e   = addError (PsError (PsErrTypeAppWithoutSpace (unLoc v) e) [] l)
                        >> return (L l hsHoleExpr)
  mkHsLazyPatPV l e   = addError (PsError (PsErrLazyPatWithoutSpace e) [] l)
                        >> return (L l hsHoleExpr)
  mkHsBangPatPV l e   = addError (PsError (PsErrBangPatWithoutSpace e) [] l)
                        >> return (L l hsHoleExpr)
  mkSumOrTuplePV = mkSumOrTupleExpr
  rejectPragmaPV (L _ (OpApp _ _ _ e)) =
    -- assuming left-associative parsing of operators
    rejectPragmaPV e
  rejectPragmaPV (L l (HsPragE _ prag _)) = addError $ PsError (PsErrUnallowedPragma prag) [] l
  rejectPragmaPV _                        = return ()

hsHoleExpr :: HsExpr GhcPs
hsHoleExpr = HsUnboundVar noExtField (mkVarOcc "_")

instance DisambECP (PatBuilder GhcPs) where
  type Body (PatBuilder GhcPs) = PatBuilder
  ecpFromCmd' (L l c)    = addFatalError $ PsError (PsErrArrowCmdInPat c) [] l
  ecpFromExp' (L l e)    = addFatalError $ PsError (PsErrArrowExprInPat e) [] l
  mkHsLamPV l _          = addFatalError $ PsError PsErrLambdaInPat [] l
  mkHsLetPV l _ _        = addFatalError $ PsError PsErrLetInPat [] l
  type InfixOp (PatBuilder GhcPs) = RdrName
  superInfixOp m = m
  mkHsOpAppPV l p1 op p2 = return $ L l $ PatBuilderOpApp p1 op p2
  mkHsCasePV l _ _       = addFatalError $ PsError PsErrCaseInPat [] l
  mkHsLamCasePV l _      = addFatalError $ PsError PsErrLambdaCaseInPat [] l
  type FunArg (PatBuilder GhcPs) = PatBuilder GhcPs
  superFunArg m = m
  mkHsAppPV l p1 p2     = return $ L l (PatBuilderApp p1 p2)
  mkHsAppTypePV l p t   = return $ L l (PatBuilderAppType p (mkHsPatSigType t))
  mkHsIfPV l _ _ _ _ _  = addFatalError $ PsError PsErrIfTheElseInPat [] l
  mkHsDoPV l _ _        = addFatalError $ PsError PsErrDoNotationInPat [] l
  mkHsParPV l p         = return $ L l (PatBuilderPar p)
  mkHsVarPV v@(getLoc -> l) = return $ L l (PatBuilderVar v)
  mkHsLitPV lit@(L l a) = do
    checkUnboxedStringLitPat lit
    return $ L l (PatBuilderPat (LitPat noExtField a))
  mkHsOverLitPV (L l a) = return $ L l (PatBuilderOverLit a)
  mkHsWildCardPV l = return $ L l (PatBuilderPat (WildPat noExtField))
  mkHsTySigPV l b sig = do
    p <- checkLPat b
    return $ L l (PatBuilderPat (SigPat noExtField p (mkHsPatSigType sig)))
  mkHsExplicitListPV l xs = do
    ps <- traverse checkLPat xs
    return (L l (PatBuilderPat (ListPat noExtField ps)))
  mkHsSplicePV (L l sp) = return $ L l (PatBuilderPat (SplicePat noExtField sp))
  mkHsRecordPV l _ a (fbinds, ddLoc) = do
    r <- mkPatRec a (mk_rec_fields fbinds ddLoc)
    checkRecordSyntax (L l r)
  mkHsNegAppPV l (L lp p) = do
    lit <- case p of
      PatBuilderOverLit pos_lit -> return (L lp pos_lit)
      _ -> patFail l (text "-" <> ppr p)
    return $ L l (PatBuilderPat (mkNPat lit (Just noSyntaxExpr)))
  mkHsSectionR_PV l op p = patFail l (pprInfixOcc (unLoc op) <> ppr p)
  mkHsViewPatPV l a b = do
    p <- checkLPat b
    return $ L l (PatBuilderPat (ViewPat noExtField a p))
  mkHsAsPatPV l v e = do
    p <- checkLPat e
    return $ L l (PatBuilderPat (AsPat noExtField v p))
  mkHsLazyPatPV l e = do
    p <- checkLPat e
    return $ L l (PatBuilderPat (LazyPat noExtField p))
  mkHsBangPatPV l e = do
    p <- checkLPat e
    let pb = BangPat noExtField p
    hintBangPat l pb
    return $ L l (PatBuilderPat pb)
  mkSumOrTuplePV = mkSumOrTuplePat
  rejectPragmaPV _ = return ()

checkUnboxedStringLitPat :: Located (HsLit GhcPs) -> PV ()
checkUnboxedStringLitPat (L loc lit) =
  case lit of
    HsStringPrim _ _  -- Trac #13260
      -> addFatalError $ PsError (PsErrIllegalUnboxedStringInPat lit) [] loc
    _ -> return ()

mkPatRec ::
  Located (PatBuilder GhcPs) ->
  HsRecFields GhcPs (Located (PatBuilder GhcPs)) ->
  PV (PatBuilder GhcPs)
mkPatRec (unLoc -> PatBuilderVar c) (HsRecFields fs dd)
  | isRdrDataCon (unLoc c)
  = do fs <- mapM checkPatField fs
       return $ PatBuilderPat $ ConPat
         { pat_con_ext = noExtField
         , pat_con = c
         , pat_args = RecCon (HsRecFields fs dd)
         }
mkPatRec p _ =
  addFatalError $ PsError (PsErrInvalidRecordCon (unLoc p)) [] (getLoc p)

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
  mkHsAppTyHeadPV :: LHsType GhcPs -> PV (Located b)
  -- | Disambiguate @f x@ (function application or prefix data constructor).
  mkHsAppTyPV :: Located b -> LHsType GhcPs -> PV (Located b)
  -- | Disambiguate @f \@t@ (visible kind application)
  mkHsAppKindTyPV :: Located b -> SrcSpan -> LHsType GhcPs -> PV (Located b)
  -- | Disambiguate @f \# x@ (infix operator)
  mkHsOpTyPV :: LHsType GhcPs -> Located RdrName -> LHsType GhcPs -> PV (Located b)
  -- | Disambiguate @{-\# UNPACK \#-} t@ (unpack/nounpack pragma)
  mkUnpackednessPV :: Located UnpackednessPragma -> Located b -> PV (Located b)

instance DisambTD (HsType GhcPs) where
  mkHsAppTyHeadPV = return
  mkHsAppTyPV t1 t2 = return (mkHsAppTy t1 t2)
  mkHsAppKindTyPV t l_at ki = return (mkHsAppKindTy l' t ki)
    where l' = combineSrcSpans l_at (getLoc ki)
  mkHsOpTyPV t1 op t2 = return (mkLHsOpTy t1 op t2)
  mkUnpackednessPV = addUnpackednessP

dataConBuilderCon :: DataConBuilder -> Located RdrName
dataConBuilderCon (PrefixDataConBuilder _ dc) = dc
dataConBuilderCon (InfixDataConBuilder _ dc _) = dc

dataConBuilderDetails :: DataConBuilder -> HsConDeclH98Details GhcPs

-- Detect when the record syntax is used:
--   data T = MkT { ... }
dataConBuilderDetails (PrefixDataConBuilder flds _)
  | [L l_t (HsRecTy _ fields)] <- toList flds
  = RecCon (L l_t fields)

-- Normal prefix constructor, e.g.  data T = MkT A B C
dataConBuilderDetails (PrefixDataConBuilder flds _)
  = PrefixCon noTypeArgs (map hsLinear (toList flds))

-- Infix constructor, e.g. data T = Int :! Bool
dataConBuilderDetails (InfixDataConBuilder lhs _ rhs)
  = InfixCon (hsLinear lhs) (hsLinear rhs)

instance DisambTD DataConBuilder where
  mkHsAppTyHeadPV = tyToDataConBuilder

  mkHsAppTyPV (L l (PrefixDataConBuilder flds fn)) t =
    return $
      L (combineSrcSpans l (getLoc t))
        (PrefixDataConBuilder (flds `snocOL` t) fn)
  mkHsAppTyPV (L _ InfixDataConBuilder{}) _ =
    -- This case is impossible because of the way
    -- the grammar in Parser.y is written (see infixtype/ftype).
    panic "mkHsAppTyPV: InfixDataConBuilder"

  mkHsAppKindTyPV lhs l_at ki =
    addFatalError $ PsError (PsErrUnexpectedKindAppInDataCon (unLoc lhs) (unLoc ki)) [] l_at

  mkHsOpTyPV lhs (L l_tc tc) rhs = do
      check_no_ops (unLoc rhs)  -- check the RHS because parsing type operators is right-associative
      data_con <- eitherToP $ tyConToDataCon l_tc tc
      return $ L l (InfixDataConBuilder lhs data_con rhs)
    where
      l = combineLocs lhs rhs
      check_no_ops (HsBangTy _ _ t) = check_no_ops (unLoc t)
      check_no_ops (HsOpTy{}) =
        addError $ PsError (PsErrInvalidInfixDataCon (unLoc lhs) tc (unLoc rhs)) [] l
      check_no_ops _ = return ()

  mkUnpackednessPV unpk constr_stuff
    | L _ (InfixDataConBuilder lhs data_con rhs) <- constr_stuff
    = -- When the user writes  data T = {-# UNPACK #-} Int :+ Bool
      --   we apply {-# UNPACK #-} to the LHS
      do lhs' <- addUnpackednessP unpk lhs
         let l = combineLocs unpk constr_stuff
         return $ L l (InfixDataConBuilder lhs' data_con rhs)
    | otherwise =
      do addError $ PsError PsErrUnpackDataCon [] (getLoc unpk)
         return constr_stuff

tyToDataConBuilder :: LHsType GhcPs -> PV (Located DataConBuilder)
tyToDataConBuilder (L l (HsTyVar _ NotPromoted (L _ v))) = do
  data_con <- eitherToP $ tyConToDataCon l v
  return $ L l (PrefixDataConBuilder nilOL data_con)
tyToDataConBuilder (L l (HsTupleTy _ HsBoxedOrConstraintTuple ts)) = do
  let data_con = L l (getRdrName (tupleDataCon Boxed (length ts)))
  return $ L l (PrefixDataConBuilder (toOL ts) data_con)
tyToDataConBuilder t =
  addFatalError $ PsError (PsErrInvalidDataCon (unLoc t)) [] (getLoc t)

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

  alts :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

We abstract over LHsExpr GhcPs, and it becomes:

  alts :: { forall b. DisambECP b => PV (Located ([AddAnn],[LMatch GhcPs (Located b)])) }
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
  | FrameDo (HsStmtContext GhcRn) [LFrameStmt]
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

  alts :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

Under the new scheme, we have to completely duplicate its type signature and
each reduction rule:

  alts :: { ( PV (Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)])) -- as an expression
            , PV (Located ([AddAnn],[LMatch GhcPs (LHsCmd GhcPs)]))  -- as a command
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

  alts :: { Located ([AddAnn],[LMatch GhcPs (LHsExpr GhcPs)]) }
    : alts1     { sL1 $1 (fst $ unLoc $1,snd $ unLoc $1) }
    | ';' alts  { sLL $1 $> ((mj AnnSemi $1:(fst $ unLoc $2)),snd $ unLoc $2) }

We abstract over LHsExpr, and it becomes:

  alts :: { forall b. ExpCmdG b -> PV (Located ([AddAnn],[LMatch GhcPs (Located (b GhcPs))])) }
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

  alts :: { forall b. ExpCmdI b => PV (Located ([AddAnn],[LMatch GhcPs (Located (b GhcPs))])) }
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
    | PatBuilderApp (Located (PatBuilder p)) (Located (PatBuilder p))
    | PatBuilderOpApp (Located (PatBuilder p)) (Located RdrName) (Located (PatBuilder p))
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
        :: Located (SourceText,Int)             -- ^ precedence
        -> Located (OrdList (Located RdrName))  -- ^ operators
        -> P ()
checkPrecP (L l (_,i)) (L _ ol)
 | 0 <= i, i <= maxPrecedence = pure ()
 | all specialOp ol = pure ()
 | otherwise = addFatalError $ PsError (PsErrPrecedenceOutOfRange i) [] l
  where
    -- If you change this, consider updating Note [Fixity of (->)] in GHC/Types.hs
    specialOp op = unLoc op `elem` [ eqTyCon_RDR
                                   , getRdrName unrestrictedFunTyCon ]

mkRecConstrOrUpdate
        :: LHsExpr GhcPs
        -> SrcSpan
        -> ([LHsRecField GhcPs (LHsExpr GhcPs)], Maybe SrcSpan)
        -> PV (HsExpr GhcPs)

mkRecConstrOrUpdate (L l (HsVar _ (L _ c))) _ (fs,dd)
  | isRdrDataCon c
  = return (mkRdrRecordCon (L l c) (mk_rec_fields fs dd))
mkRecConstrOrUpdate exp _ (fs,dd)
  | Just dd_loc <- dd = addFatalError $ PsError PsErrDotsInRecordUpdate [] dd_loc
  | otherwise = return (mkRdrRecordUpd exp (map (fmap mk_rec_upd_field) fs))

mkRdrRecordUpd :: LHsExpr GhcPs -> [LHsRecUpdField GhcPs] -> HsExpr GhcPs
mkRdrRecordUpd exp flds
  = RecordUpd { rupd_ext  = noExtField
              , rupd_expr = exp
              , rupd_flds = flds }

mkRdrRecordCon :: Located RdrName -> HsRecordBinds GhcPs -> HsExpr GhcPs
mkRdrRecordCon con flds
  = RecordCon { rcon_ext = noExtField, rcon_con = con, rcon_flds = flds }

mk_rec_fields :: [Located (HsRecField (GhcPass p) arg)] -> Maybe SrcSpan -> HsRecFields (GhcPass p) arg
mk_rec_fields fs Nothing = HsRecFields { rec_flds = fs, rec_dotdot = Nothing }
mk_rec_fields fs (Just s)  = HsRecFields { rec_flds = fs
                                     , rec_dotdot = Just (L s (length fs)) }

mk_rec_upd_field :: HsRecField GhcPs (LHsExpr GhcPs) -> HsRecUpdField GhcPs
mk_rec_upd_field (HsRecField (L loc (FieldOcc _ rdr)) arg pun)
  = HsRecField (L loc (Unambiguous noExtField rdr)) arg pun

mkInlinePragma :: SourceText -> (InlineSpec, RuleMatchInfo) -> Maybe Activation
               -> InlinePragma
-- The (Maybe Activation) is because the user can omit
-- the activation spec (and usually does)
mkInlinePragma src (inl, match_info) mb_act
  = InlinePragma { inl_src = src -- Note [Pragma source text] in GHC.Types.SourceText
                 , inl_inline = inl
                 , inl_sat    = Nothing
                 , inl_act    = act
                 , inl_rule   = match_info }
  where
    act = case mb_act of
            Just act -> act
            Nothing  -> -- No phase specified
                        case inl of
                          NoInline -> NeverActive
                          _other   -> AlwaysActive

-----------------------------------------------------------------------------
-- utilities for foreign declarations

-- construct a foreign import declaration
--
mkImport :: Located CCallConv
         -> Located Safety
         -> (Located StringLiteral, Located RdrName, LHsSigType GhcPs)
         -> P (HsDecl GhcPs)
mkImport cconv safety (L loc (StringLiteral esrc entity), v, ty) =
    case unLoc cconv of
      CCallConv          -> mkCImport
      CApiConv           -> mkCImport
      StdCallConv        -> mkCImport
      PrimCallConv       -> mkOtherImport
      JavaScriptCallConv -> mkOtherImport
  where
    -- Parse a C-like entity string of the following form:
    --   "[static] [chname] [&] [cid]" | "dynamic" | "wrapper"
    -- If 'cid' is missing, the function name 'v' is used instead as symbol
    -- name (cf section 8.5.1 in Haskell 2010 report).
    mkCImport = do
      let e = unpackFS entity
      case parseCImport cconv safety (mkExtName (unLoc v)) e (L loc esrc) of
        Nothing         -> addFatalError $ PsError PsErrMalformedEntityString [] loc
        Just importSpec -> returnSpec importSpec

    -- currently, all the other import conventions only support a symbol name in
    -- the entity string. If it is missing, we use the function name instead.
    mkOtherImport = returnSpec importSpec
      where
        entity'    = if nullFS entity
                        then mkExtName (unLoc v)
                        else entity
        funcTarget = CFunction (StaticTarget esrc entity' Nothing True)
        importSpec = CImport cconv safety Nothing funcTarget (L loc esrc)

    returnSpec spec = return $ ForD noExtField $ ForeignImport
          { fd_i_ext  = noExtField
          , fd_name   = v
          , fd_sig_ty = ty
          , fd_fi     = spec
          }



-- the string "foo" is ambiguous: either a header or a C identifier.  The
-- C identifier case comes first in the alternatives below, so we pick
-- that one.
parseCImport :: Located CCallConv -> Located Safety -> FastString -> String
             -> Located SourceText
             -> Maybe ForeignImport
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
                  mk (Just (Header (SourceText h) (mkFastString h)))
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

   mk h n = CImport cconv safety h n sourceText

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
         -> (Located StringLiteral, Located RdrName, LHsSigType GhcPs)
         -> P (HsDecl GhcPs)
mkExport (L lc cconv) (L le (StringLiteral esrc entity), v, ty)
 = return $ ForD noExtField $
   ForeignExport { fd_e_ext = noExtField, fd_name = v, fd_sig_ty = ty
                 , fd_fe = CExport (L lc (CExportStatic esrc entity' cconv))
                                   (L le esrc) }
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
mkExtName rdrNm = mkFastString (occNameString (rdrNameOcc rdrNm))

--------------------------------------------------------------------------------
-- Help with module system imports/exports

data ImpExpSubSpec = ImpExpAbs
                   | ImpExpAll
                   | ImpExpList [Located ImpExpQcSpec]
                   | ImpExpAllWith [Located ImpExpQcSpec]

data ImpExpQcSpec = ImpExpQcName (Located RdrName)
                  | ImpExpQcType (Located RdrName)
                  | ImpExpQcWildcard

mkModuleImpExp :: Located ImpExpQcSpec -> ImpExpSubSpec -> P (IE GhcPs)
mkModuleImpExp (L l specname) subs =
  case subs of
    ImpExpAbs
      | isVarNameSpace (rdrNameSpace name)
                       -> return $ IEVar noExtField (L l (ieNameFromSpec specname))
      | otherwise      -> IEThingAbs noExtField . L l <$> nameT
    ImpExpAll          -> IEThingAll noExtField . L l <$> nameT
    ImpExpList xs      ->
      (\newName -> IEThingWith noExtField (L l newName)
        NoIEWildcard (wrapped xs) []) <$> nameT
    ImpExpAllWith xs                       ->
      do allowed <- getBit PatternSynonymsBit
         if allowed
          then
            let withs = map unLoc xs
                pos   = maybe NoIEWildcard IEWildcard
                          (findIndex isImpExpQcWildcard withs)
                ies   = wrapped $ filter (not . isImpExpQcWildcard . unLoc) xs
            in (\newName
                        -> IEThingWith noExtField (L l newName) pos ies [])
               <$> nameT
          else addFatalError $ PsError PsErrIllegalPatSynExport [] l
  where
    name = ieNameVal specname
    nameT =
      if isVarNameSpace (rdrNameSpace name)
        then addFatalError $ PsError (PsErrVarForTyCon name) [] l
        else return $ ieNameFromSpec specname

    ieNameVal (ImpExpQcName ln)  = unLoc ln
    ieNameVal (ImpExpQcType ln)  = unLoc ln
    ieNameVal (ImpExpQcWildcard) = panic "ieNameVal got wildcard"

    ieNameFromSpec (ImpExpQcName ln)  = IEName ln
    ieNameFromSpec (ImpExpQcType ln)  = IEType ln
    ieNameFromSpec (ImpExpQcWildcard) = panic "ieName got wildcard"

    wrapped = map (mapLoc ieNameFromSpec)

mkTypeImpExp :: Located RdrName   -- TcCls or Var name space
             -> P (Located RdrName)
mkTypeImpExp name =
  do allowed <- getBit ExplicitNamespacesBit
     unless allowed $ addError $ PsError PsErrIllegalExplicitNamespace [] (getLoc name)
     return (fmap (`setRdrNameSpace` tcClsName) name)

checkImportSpec :: Located [LIE GhcPs] -> P (Located [LIE GhcPs])
checkImportSpec ie@(L _ specs) =
    case [l | (L l (IEThingWith _ _ (IEWildcard _) _ _)) <- specs] of
      [] -> return ie
      (l:_) -> importSpecError l
  where
    importSpecError l =
      addFatalError $ PsError PsErrIllegalImportBundleForm [] l

-- In the correct order
mkImpExpSubSpec :: [Located ImpExpQcSpec] -> P ([AddAnn], ImpExpSubSpec)
mkImpExpSubSpec [] = return ([], ImpExpList [])
mkImpExpSubSpec [L _ ImpExpQcWildcard] =
  return ([], ImpExpAll)
mkImpExpSubSpec xs =
  if (any (isImpExpQcWildcard . unLoc) xs)
    then return $ ([], ImpExpAllWith xs)
    else return $ ([], ImpExpList xs)

isImpExpQcWildcard :: ImpExpQcSpec -> Bool
isImpExpQcWildcard ImpExpQcWildcard = True
isImpExpQcWildcard _                = False

-----------------------------------------------------------------------------
-- Warnings and failures

warnPrepositiveQualifiedModule :: SrcSpan -> P ()
warnPrepositiveQualifiedModule span =
  addWarning Opt_WarnPrepositiveQualifiedModule (PsWarnImportPreQualified span)

failOpNotEnabledImportQualifiedPost :: SrcSpan -> P ()
failOpNotEnabledImportQualifiedPost loc = addError $ PsError PsErrImportPostQualified [] loc

failOpImportQualifiedTwice :: SrcSpan -> P ()
failOpImportQualifiedTwice loc = addError $ PsError PsErrImportQualifiedTwice [] loc

warnStarIsType :: SrcSpan -> P ()
warnStarIsType span = addWarning Opt_WarnStarIsType (PsWarnStarIsType span)

failOpFewArgs :: MonadP m => Located RdrName -> m a
failOpFewArgs (L loc op) =
  do { star_is_type <- getBit StarIsTypeBit
     ; addFatalError $ PsError (PsErrOpFewArgs (StarIsType star_is_type) op) [] loc }

-----------------------------------------------------------------------------
-- Misc utils

data PV_Context =
  PV_Context
    { pv_options :: ParserOpts
    , pv_hints   :: [Hint]  -- See Note [Parser-Validator Hint]
    }

data PV_Accum =
  PV_Accum
    { pv_warnings :: Bag PsWarning
    , pv_errors   :: Bag PsError
    , pv_annotations :: [(ApiAnnKey,[RealSrcSpan])]
    , pv_comment_q :: [RealLocated AnnotationComment]
    , pv_annotations_comments :: [(RealSrcSpan,[RealLocated AnnotationComment])]
    }

data PV_Result a = PV_Ok PV_Accum a | PV_Failed PV_Accum

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

instance Functor PV where
  fmap = liftM

instance Applicative PV where
  pure a = a `seq` PV (\_ acc -> PV_Ok acc a)
  (<*>) = ap

instance Monad PV where
  m >>= f = PV $ \ctx acc ->
    case unPV m ctx acc of
      PV_Ok acc' a -> unPV (f a) ctx acc'
      PV_Failed acc' -> PV_Failed acc'

runPV :: PV a -> P a
runPV = runPV_hints []

runPV_hints :: [Hint] -> PV a -> P a
runPV_hints hints m =
  P $ \s ->
    let
      pv_ctx = PV_Context
        { pv_options = options s
        , pv_hints   = hints }
      pv_acc = PV_Accum
        { pv_warnings = warnings s
        , pv_errors   = errors s
        , pv_annotations = annotations s
        , pv_comment_q = comment_q s
        , pv_annotations_comments = annotations_comments s }
      mkPState acc' =
        s { warnings = pv_warnings acc'
          , errors   = pv_errors acc'
          , annotations = pv_annotations acc'
          , comment_q = pv_comment_q acc'
          , annotations_comments = pv_annotations_comments acc' }
    in
      case unPV m pv_ctx pv_acc of
        PV_Ok acc' a -> POk (mkPState acc') a
        PV_Failed acc' -> PFailed (mkPState acc')

add_hint :: Hint -> PV a -> PV a
add_hint hint m =
  let modifyHint ctx = ctx{pv_hints = pv_hints ctx ++ [hint]} in
  PV (\ctx acc -> unPV m (modifyHint ctx) acc)

instance MonadP PV where
  addError err@(PsError e hints loc) =
    PV $ \ctx acc ->
      let err' | null (pv_hints ctx) = err
               | otherwise           = PsError e (hints ++ pv_hints ctx) loc
      in PV_Ok acc{pv_errors = err' `consBag` pv_errors acc} ()
  addWarning option w =
    PV $ \ctx acc ->
      if warnopt option (pv_options ctx)
         then PV_Ok acc{pv_warnings= w `consBag` pv_warnings acc} ()
         else PV_Ok acc ()
  addFatalError err =
    addError err >> PV (const PV_Failed)
  getBit ext =
    PV $ \ctx acc ->
      let b = ext `xtest` pExtsBitmap (pv_options ctx) in
      PV_Ok acc $! b
  addAnnotation (RealSrcSpan l _) a (RealSrcSpan v _) =
    PV $ \_ acc ->
      let
        (comment_q', new_ann_comments) = allocateComments l (pv_comment_q acc)
        annotations_comments' = new_ann_comments ++ pv_annotations_comments acc
        annotations' = ((l,a), [v]) : pv_annotations acc
        acc' = acc
          { pv_annotations = annotations'
          , pv_comment_q = comment_q'
          , pv_annotations_comments = annotations_comments' }
      in
        PV_Ok acc' ()
  addAnnotation _ _ _ = return ()

{- Note [Parser-Validator Hint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A PV computation is parametrized by a hint for error messages, which can be set
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

We attempt to detect such cases and add a hint to the error messages:

  T984.hs:6:9:
    Parse error in pattern: case () of { _ -> result }
    Possibly caused by a missing 'do'?

The "Possibly caused by a missing 'do'?" suggestion is the hint that is passed
as the 'pv_hints' field 'PV_Context'. When validating in a context other than
'bindpat' (a pattern to the left of <-), we set the hint to 'empty' and it has
no effect on the error messages.

-}

-- | Hint about bang patterns, assuming @BangPatterns@ is off.
hintBangPat :: SrcSpan -> Pat GhcPs -> PV ()
hintBangPat span e = do
    bang_on <- getBit BangPatBit
    unless bang_on $
      addError $ PsError (PsErrIllegalBangPattern e) [] span

mkSumOrTupleExpr :: SrcSpan -> Boxity -> SumOrTuple (HsExpr GhcPs) -> PV (LHsExpr GhcPs)

-- Tuple
mkSumOrTupleExpr l boxity (Tuple es) =
    return $ L l (ExplicitTuple noExtField (map toTupArg es) boxity)
  where
    toTupArg :: Located (Maybe (LHsExpr GhcPs)) -> LHsTupArg GhcPs
    toTupArg = mapLoc (maybe missingTupArg (Present noExtField))

-- Sum
mkSumOrTupleExpr l Unboxed (Sum alt arity e) =
    return $ L l (ExplicitSum noExtField alt arity e)
mkSumOrTupleExpr l Boxed a@Sum{} =
    addFatalError $ PsError (PsErrUnsupportedBoxedSumExpr a) [] l

mkSumOrTuplePat :: SrcSpan -> Boxity -> SumOrTuple (PatBuilder GhcPs) -> PV (Located (PatBuilder GhcPs))

-- Tuple
mkSumOrTuplePat l boxity (Tuple ps) = do
  ps' <- traverse toTupPat ps
  return $ L l (PatBuilderPat (TuplePat noExtField ps' boxity))
  where
    toTupPat :: Located (Maybe (Located (PatBuilder GhcPs))) -> PV (LPat GhcPs)
    toTupPat (L l p) = case p of
      Nothing -> addFatalError $ PsError PsErrTupleSectionInPat [] l
      Just p' -> checkLPat p'

-- Sum
mkSumOrTuplePat l Unboxed (Sum alt arity p) = do
   p' <- checkLPat p
   return $ L l (PatBuilderPat (SumPat noExtField p' alt arity))
mkSumOrTuplePat l Boxed a@Sum{} =
    addFatalError $ PsError (PsErrUnsupportedBoxedSumPat a) [] l

mkLHsOpTy :: LHsType GhcPs -> Located RdrName -> LHsType GhcPs -> LHsType GhcPs
mkLHsOpTy x op y =
  let loc = getLoc x `combineSrcSpans` getLoc op `combineSrcSpans` getLoc y
  in L loc (mkHsOpTy x op y)

mkMultTy :: IsUnicodeSyntax -> Located Token -> LHsType GhcPs -> (HsArrow GhcPs, AddAnn)
mkMultTy u tok t@(L _ (HsTyLit _ (HsNumTy (SourceText "1") 1)))
  -- See #18888 for the use of (SourceText "1") above
  = (HsLinearArrow u, AddAnn AnnPercentOne (combineLocs tok t))
mkMultTy u tok t = (HsExplicitMult u t, AddAnn AnnPercent (getLoc tok))

-----------------------------------------------------------------------------
-- Token symbols

starSym :: Bool -> String
starSym True = "★"
starSym False = "*"
