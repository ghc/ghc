--
--  (c) The University of Glasgow 2002-2006
--

-- Functions over HsSyn specialised to RdrName.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module   RdrHsSyn (
        mkHsOpApp,
        mkHsIntegral, mkHsFractional, mkHsIsString,
        mkHsDo, mkSpliceDecl,
        mkRoleAnnotDecl,
        mkClassDecl,
        mkTyData, mkDataFamInst,
        mkTySynonym, mkTyFamInstEqn,
        mkTyFamInst,
        mkFamDecl, mkLHsSigType,
        splitCon, mkInlinePragma,
        mkPatSynMatchGroup,
        mkRecConstrOrUpdate, -- HsExp -> [HsFieldUpdate] -> P HsExp
        mkTyClD, mkInstD,
        mkRdrRecordCon, mkRdrRecordUpd,
        setRdrNameSpace,

        cvBindGroup,
        cvBindsAndSigs,
        cvTopDecls,
        placeHolderPunRhs,

        -- Stuff to do with Foreign declarations
        mkImport,
        parseCImport,
        mkExport,
        mkExtName,           -- RdrName -> CLabelString
        mkGadtDecl,          -- [Located RdrName] -> LHsType RdrName -> ConDecl RdrName
        mkConDeclH98,
        mkATDefault,

        -- Bunch of functions in the parser monad for
        -- checking and constructing values
        checkPrecP,           -- Int -> P Int
        checkContext,         -- HsType -> P HsContext
        checkPattern,         -- HsExp -> P HsPat
        bang_RDR,
        checkPatterns,        -- SrcLoc -> [HsExp] -> P [HsPat]
        checkMonadComp,       -- P (HsStmtContext RdrName)
        checkCommand,         -- LHsExpr RdrName -> P (LHsCmd RdrName)
        checkValDef,          -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
        checkValSigLhs,
        checkDoAndIfThenElse,
        checkRecordSyntax,
        parseErrorSDoc,
        splitTilde, splitTildeApps,

        -- Help with processing exports
        ImpExpSubSpec(..),
        ImpExpQcSpec(..),
        mkModuleImpExp,
        mkTypeImpExp,
        mkImpExpSubSpec,
        checkImportSpec,

        SumOrTuple (..), mkSumOrTuple

    ) where

import HsSyn            -- Lots of it
import Class            ( FunDep )
import TyCon            ( TyCon, isTupleTyCon, tyConSingleDataCon_maybe )
import DataCon          ( DataCon, dataConTyCon )
import ConLike          ( ConLike(..) )
import CoAxiom          ( Role, fsFromRole )
import RdrName
import Name
import BasicTypes
import TcEvidence       ( idHsWrapper )
import Lexer
import Lexeme           ( isLexCon )
import Type             ( TyThing(..) )
import TysWiredIn       ( cTupleTyConName, tupleTyCon, tupleDataCon,
                          nilDataConName, nilDataConKey,
                          listTyConName, listTyConKey,
                          starKindTyConName, unicodeStarKindTyConName )
import ForeignCall
import PrelNames        ( forall_tv_RDR, eqTyCon_RDR, allNameStrings )
import SrcLoc
import Unique           ( hasKey )
import OrdList          ( OrdList, fromOL )
import Bag              ( emptyBag, consBag )
import Outputable
import FastString
import Maybes
import Util
import ApiAnnotation
import Data.List
import qualified GHC.LanguageExtensions as LangExt
import MonadUtils

import Control.Monad
import Text.ParserCombinators.ReadP as ReadP
import Data.Char

import Data.Data       ( dataTypeOf, fromConstr, dataTypeConstrs )

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

--         *** See Note [The Naming story] in HsDecls ****

mkTyClD :: LTyClDecl n -> LHsDecl n
mkTyClD (L loc d) = L loc (TyClD d)

mkInstD :: LInstDecl n -> LHsDecl n
mkInstD (L loc d) = L loc (InstD d)

mkClassDecl :: SrcSpan
            -> Located (Maybe (LHsContext RdrName), LHsType RdrName)
            -> Located (a,[Located (FunDep (Located RdrName))])
            -> OrdList (LHsDecl RdrName)
            -> P (LTyClDecl RdrName)

mkClassDecl loc (L _ (mcxt, tycl_hdr)) fds where_cls
  = do { (binds, sigs, ats, at_insts, _, docs) <- cvBindsAndSigs where_cls
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; (cls, tparams, fixity, ann) <- checkTyClHdr True tycl_hdr
       ; mapM_ (\a -> a loc) ann -- Add any API Annotations to the top SrcSpan
       ; tyvars <- checkTyVarsP (text "class") whereDots cls tparams
       ; at_defs <- mapM (eitherToP . mkATDefault) at_insts
       ; return (L loc (ClassDecl { tcdCtxt = cxt, tcdLName = cls, tcdTyVars = tyvars
                                  , tcdFixity = fixity
                                  , tcdFDs = snd (unLoc fds)
                                  , tcdSigs = mkClassOpSigs sigs
                                  , tcdMeths = binds
                                  , tcdATs = ats, tcdATDefs = at_defs, tcdDocs  = docs
                                  , tcdFVs = placeHolderNames })) }

mkATDefault :: LTyFamInstDecl RdrName
            -> Either (SrcSpan, SDoc) (LTyFamDefltEqn RdrName)
-- Take a type-family instance declaration and turn it into
-- a type-family default equation for a class declaration
-- We parse things as the former and use this function to convert to the latter
--
-- We use the Either monad because this also called
-- from Convert.hs
mkATDefault (L loc (TyFamInstDecl { tfid_eqn = L _ e }))
      | TyFamEqn { tfe_tycon = tc, tfe_pats = pats, tfe_fixity = fixity
                 , tfe_rhs = rhs } <- e
      = do { tvs <- checkTyVars (text "default") equalsDots tc (hsib_body pats)
           ; return (L loc (TyFamEqn { tfe_tycon = tc
                                     , tfe_pats = tvs
                                     , tfe_fixity = fixity
                                     , tfe_rhs = rhs })) }

mkTyData :: SrcSpan
         -> NewOrData
         -> Maybe (Located CType)
         -> Located (Maybe (LHsContext RdrName), LHsType RdrName)
         -> Maybe (LHsKind RdrName)
         -> [LConDecl RdrName]
         -> HsDeriving RdrName
         -> P (LTyClDecl RdrName)
mkTyData loc new_or_data cType (L _ (mcxt, tycl_hdr)) ksig data_cons maybe_deriv
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False tycl_hdr
       ; mapM_ (\a -> a loc) ann -- Add any API Annotations to the top SrcSpan
       ; tyvars <- checkTyVarsP (ppr new_or_data) equalsDots tc tparams
       ; defn <- mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
       ; return (L loc (DataDecl { tcdLName = tc, tcdTyVars = tyvars,
                                   tcdFixity = fixity,
                                   tcdDataDefn = defn,
                                   tcdDataCusk = PlaceHolder,
                                   tcdFVs = placeHolderNames })) }

mkDataDefn :: NewOrData
           -> Maybe (Located CType)
           -> Maybe (LHsContext RdrName)
           -> Maybe (LHsKind RdrName)
           -> [LConDecl RdrName]
           -> HsDeriving RdrName
           -> P (HsDataDefn RdrName)
mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
  = do { checkDatatypeContext mcxt
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; return (HsDataDefn { dd_ND = new_or_data, dd_cType = cType
                            , dd_ctxt = cxt
                            , dd_cons = data_cons
                            , dd_kindSig = ksig
                            , dd_derivs = maybe_deriv }) }


mkTySynonym :: SrcSpan
            -> LHsType RdrName  -- LHS
            -> LHsType RdrName  -- RHS
            -> P (LTyClDecl RdrName)
mkTySynonym loc lhs rhs
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; mapM_ (\a -> a loc) ann -- Add any API Annotations to the top SrcSpan
       ; tyvars <- checkTyVarsP (text "type") equalsDots tc tparams
       ; return (L loc (SynDecl { tcdLName = tc, tcdTyVars = tyvars
                                , tcdFixity = fixity
                                , tcdRhs = rhs, tcdFVs = placeHolderNames })) }

mkTyFamInstEqn :: LHsType RdrName
               -> LHsType RdrName
               -> P (TyFamInstEqn RdrName,[AddAnn])
mkTyFamInstEqn lhs rhs
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; return (TyFamEqn { tfe_tycon = tc
                          , tfe_pats  = mkHsImplicitBndrs tparams
                          , tfe_fixity = fixity
                          , tfe_rhs   = rhs },
                 ann) }

mkDataFamInst :: SrcSpan
              -> NewOrData
              -> Maybe (Located CType)
              -> Located (Maybe (LHsContext RdrName), LHsType RdrName)
              -> Maybe (LHsKind RdrName)
              -> [LConDecl RdrName]
              -> HsDeriving RdrName
              -> P (LInstDecl RdrName)
mkDataFamInst loc new_or_data cType (L _ (mcxt, tycl_hdr)) ksig data_cons maybe_deriv
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False tycl_hdr
       ; mapM_ (\a -> a loc) ann -- Add any API Annotations to the top SrcSpan
       ; defn <- mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
       ; return (L loc (DataFamInstD (
                  DataFamInstDecl { dfid_tycon = tc
                                  , dfid_pats = mkHsImplicitBndrs tparams
                                  , dfid_fixity = fixity
                                  , dfid_defn = defn, dfid_fvs = placeHolderNames }))) }

mkTyFamInst :: SrcSpan
            -> LTyFamInstEqn RdrName
            -> P (LInstDecl RdrName)
mkTyFamInst loc eqn
  = return (L loc (TyFamInstD (TyFamInstDecl { tfid_eqn  = eqn
                                             , tfid_fvs  = placeHolderNames })))

mkFamDecl :: SrcSpan
          -> FamilyInfo RdrName
          -> LHsType RdrName                   -- LHS
          -> Located (FamilyResultSig RdrName) -- Optional result signature
          -> Maybe (LInjectivityAnn RdrName)   -- Injectivity annotation
          -> P (LTyClDecl RdrName)
mkFamDecl loc info lhs ksig injAnn
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; mapM_ (\a -> a loc) ann -- Add any API Annotations to the top SrcSpan
       ; tyvars <- checkTyVarsP (ppr info) equals_or_where tc tparams
       ; return (L loc (FamDecl (FamilyDecl{ fdInfo      = info, fdLName = tc
                                           , fdTyVars    = tyvars
                                           , fdFixity    = fixity
                                           , fdResultSig = ksig
                                           , fdInjectivityAnn = injAnn }))) }
  where
    equals_or_where = case info of
                        DataFamily          -> empty
                        OpenTypeFamily      -> empty
                        ClosedTypeFamily {} -> whereDots

mkSpliceDecl :: LHsExpr RdrName -> HsDecl RdrName
-- If the user wrote
--      [pads| ... ]   then return a QuasiQuoteD
--      $(e)           then return a SpliceD
-- but if she wrote, say,
--      f x            then behave as if she'd written $(f x)
--                     ie a SpliceD
--
-- Typed splices are not allowed at the top level, thus we do not represent them
-- as spliced declaration.  See #10945
mkSpliceDecl lexpr@(L loc expr)
  | HsSpliceE splice@(HsUntypedSplice {}) <- expr
  = SpliceD (SpliceDecl (L loc splice) ExplicitSplice)

  | HsSpliceE splice@(HsQuasiQuote {}) <- expr
  = SpliceD (SpliceDecl (L loc splice) ExplicitSplice)

  | otherwise
  = SpliceD (SpliceDecl (L loc (mkUntypedSplice NoParens lexpr)) ImplicitSplice)

mkRoleAnnotDecl :: SrcSpan
                -> LEmbellished RdrName              -- type being annotated
                -> [Located (Maybe FastString)]      -- roles
                -> P (LRoleAnnotDecl RdrName)
mkRoleAnnotDecl loc tycon roles
  = do { roles' <- mapM parse_role roles
       ; return $ L loc $ RoleAnnotDecl tycon roles' }
  where
    role_data_type = dataTypeOf (undefined :: Role)
    all_roles = map fromConstr $ dataTypeConstrs role_data_type
    possible_roles = [(fsFromRole role, role) | role <- all_roles]

    parse_role (L loc_role Nothing) = return $ L loc_role Nothing
    parse_role (L loc_role (Just role))
      = case lookup role possible_roles of
          Just found_role -> return $ L loc_role $ Just found_role
          Nothing         ->
            let nearby = fuzzyLookup (unpackFS role) (mapFst unpackFS possible_roles) in
            parseErrorSDoc loc_role
              (text "Illegal role name" <+> quotes (ppr role) $$
               suggestions nearby)

    suggestions []   = empty
    suggestions [r]  = text "Perhaps you meant" <+> quotes (ppr r)
      -- will this last case ever happen??
    suggestions list = hang (text "Perhaps you meant one of these:")
                       2 (pprWithCommas (quotes . ppr) list)

{- **********************************************************************

  #cvBinds-etc# Converting to @HsBinds@, etc.

  ********************************************************************* -}

-- | Function definitions are restructured here. Each is assumed to be recursive
-- initially, and non recursive definitions are discovered by the dependency
-- analyser.


--  | Groups together bindings for a single function
cvTopDecls :: OrdList (LHsDecl RdrName) -> [LHsDecl RdrName]
cvTopDecls decls = go (fromOL decls)
  where
    go :: [LHsDecl RdrName] -> [LHsDecl RdrName]
    go []                   = []
    go (L l (ValD b) : ds)  = L l' (ValD b') : go ds'
                            where (L l' b', ds') = getMonoBind (L l b) ds
    go (d : ds)             = d : go ds

-- Declaration list may only contain value bindings and signatures.
cvBindGroup :: OrdList (LHsDecl RdrName) -> P (HsValBinds RdrName)
cvBindGroup binding
  = do { (mbs, sigs, fam_ds, tfam_insts, dfam_insts, _) <- cvBindsAndSigs binding
       ; ASSERT( null fam_ds && null tfam_insts && null dfam_insts)
         return $ ValBindsIn mbs sigs }

cvBindsAndSigs :: OrdList (LHsDecl RdrName)
  -> P (LHsBinds RdrName, [LSig RdrName], [LFamilyDecl RdrName]
          , [LTyFamInstDecl RdrName], [LDataFamInstDecl RdrName], [LDocDecl])
-- Input decls contain just value bindings and signatures
-- and in case of class or instance declarations also
-- associated type declarations. They might also contain Haddock comments.
cvBindsAndSigs fb = go (fromOL fb)
  where
    go []              = return (emptyBag, [], [], [], [], [])
    go (L l (ValD b) : ds)
      = do { (bs, ss, ts, tfis, dfis, docs) <- go ds'
           ; return (b' `consBag` bs, ss, ts, tfis, dfis, docs) }
      where
        (b', ds') = getMonoBind (L l b) ds
    go (L l decl : ds)
      = do { (bs, ss, ts, tfis, dfis, docs) <- go ds
           ; case decl of
               SigD s
                 -> return (bs, L l s : ss, ts, tfis, dfis, docs)
               TyClD (FamDecl t)
                 -> return (bs, ss, L l t : ts, tfis, dfis, docs)
               InstD (TyFamInstD { tfid_inst = tfi })
                 -> return (bs, ss, ts, L l tfi : tfis, dfis, docs)
               InstD (DataFamInstD { dfid_inst = dfi })
                 -> return (bs, ss, ts, tfis, L l dfi : dfis, docs)
               DocD d
                 -> return (bs, ss, ts, tfis, dfis, L l d : docs)
               SpliceD d
                 -> parseErrorSDoc l $
                    hang (text "Declaration splices are allowed only" <+>
                          text "at the top level:")
                       2 (ppr d)
               _ -> pprPanic "cvBindsAndSigs" (ppr decl) }

-----------------------------------------------------------------------------
-- Group function bindings into equation groups

getMonoBind :: LHsBind RdrName -> [LHsDecl RdrName]
  -> (LHsBind RdrName, [LHsDecl RdrName])
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

getMonoBind (L loc1 (FunBind { fun_id = fun_id1@(L _ f1),
                               fun_matches
                                 = MG { mg_alts = L _ mtchs1 } })) binds
  | has_args mtchs1
  = go mtchs1 loc1 binds []
  where
    go mtchs loc
       (L loc2 (ValD (FunBind { fun_id = L _ f2,
                                fun_matches
                                  = MG { mg_alts = L _ mtchs2 } })) : binds) _
        | f1 == f2 = go (mtchs2 ++ mtchs)
                        (combineSrcSpans loc loc2) binds []
    go mtchs loc (doc_decl@(L loc2 (DocD _)) : binds) doc_decls
        = let doc_decls' = doc_decl : doc_decls
          in go mtchs (combineSrcSpans loc loc2) binds doc_decls'
    go mtchs loc binds doc_decls
        = ( L loc (makeFunBind fun_id1 (reverse mtchs))
          , (reverse doc_decls) ++ binds)
        -- Reverse the final matches, to get it back in the right order
        -- Do the same thing with the trailing doc comments

getMonoBind bind binds = (bind, binds)

has_args :: [LMatch RdrName (LHsExpr RdrName)] -> Bool
has_args []                           = panic "RdrHsSyn:has_args"
has_args ((L _ (Match _ args _ _)) : _) = not (null args)
        -- Don't group together FunBinds if they have
        -- no arguments.  This is necessary now that variable bindings
        -- with no arguments are now treated as FunBinds rather
        -- than pattern bindings (tests/rename/should_fail/rnfail002).

{- **********************************************************************

  #PrefixToHS-utils# Utilities for conversion

  ********************************************************************* -}

{- Note [Parsing data constructors is hard]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We parse the RHS of the constructor declaration
     data T = C t1 t2
as a btype_no_ops (treating C as a type constructor) and then convert C to be
a data constructor.  Reason: it might continue like this:
     data T = C t1 t2 :% D Int
in which case C really /would/ be a type constructor.  We can't resolve this
ambiguity till we come across the constructor oprerator :% (or not, more usually)

So the plan is:

* Parse the data constructor declration as a type (actually btype_no_ops)

* Use 'splitCon' to rejig it into the data constructor and the args

* In doing so, we use 'tyConToDataCon' to convert the RdrName for
  the data con, which has been parsed as a tycon, back to a datacon.
  This is more than just adjusting the name space; for operators we
  need to check that it begins with a colon.  E.g.
     data T = (+++)
  will parse ok (since tycons can be operators), but we should reject
  it (Trac #12051).
-}

splitCon :: LHsType RdrName
      -> P (LEmbellished RdrName, HsConDeclDetails RdrName)
-- See Note [Parsing data constructors is hard]
-- This gets given a "type" that should look like
--      C Int Bool
-- or   C { x::Int, y::Bool }
-- and returns the pieces
splitCon ty
 = split ty []
 where
   -- This is used somewhere where HsAppsTy is not used
   split (L _ (HsAppTy t u)) ts       = split t (u : ts)
   split (L l (HsTyVar _ (L _ tc)))  ts
     = do data_con <- tyConToDataCon l tc
          return (data_con, mk_rest ts)
   split (L l (HsTupleTy HsBoxedOrConstraintTuple ts)) []
      = return (L l (EName $ getRdrName (tupleDataCon Boxed (length ts)))
               , PrefixCon ts)
   split (L l _) _ = parseErrorSDoc l (text "Cannot parse data constructor in a data/newtype declaration:" <+> ppr ty)

   mk_rest [L l (HsRecTy flds)] = RecCon (L l flds)
   mk_rest ts                   = PrefixCon ts

tyConToDataCon :: SrcSpan -> Embellished RdrName -> P (LEmbellished RdrName)
-- See Note [Parsing data constructors is hard]
-- Data constructor RHSs are parsed as types
tyConToDataCon loc tc
  | isTcOcc occ
  , isLexCon (occNameFS occ)
  -- = return (L loc (setRdrNameSpace tc srcDataName))
  = return (L loc $ fmap (\n -> setRdrNameSpace n srcDataName) tc)

  | otherwise
  = parseErrorSDoc loc (msg $$ extra)
  where
    occ = rdrNameOcc $ unEmb tc

    msg = text "Not a data constructor:" <+> quotes (ppr tc)
    extra | unEmb tc == forall_tv_RDR
          = text "Perhaps you intended to use ExistentialQuantification"
          | otherwise = empty

mkPatSynMatchGroup :: LEmbellished RdrName
                   -> Located (OrdList (LHsDecl RdrName))
                   -> P (MatchGroup RdrName (LHsExpr RdrName))
mkPatSynMatchGroup (L loc patsyn_name) (L _ decls) =
    do { matches <- mapM fromDecl (fromOL decls)
       ; when (null matches) (wrongNumberErr loc)
       ; return $ mkMatchGroup FromSource matches }
  where
    fromDecl (L loc decl@(ValD (PatBind pat@(L _ (ConPatIn ln@(L _ name) details)) rhs _ _ _))) =
        do { unless (name == unEmb patsyn_name) $
               wrongNameBindingErr loc decl
           ; match <- case details of
               PrefixCon pats ->
                        return $ Match (FunRhs ln Prefix) pats Nothing rhs
               InfixCon pat1 pat2 ->
                       return $ Match (FunRhs ln Infix) [pat1, pat2] Nothing rhs
               RecCon{} -> recordPatSynErr loc pat
           ; return $ L loc match }
    fromDecl (L loc decl) = extraDeclErr loc decl

    extraDeclErr loc decl =
        parseErrorSDoc loc $
        text "pattern synonym 'where' clause must contain a single binding:" $$
        ppr decl

    wrongNameBindingErr loc decl =
        parseErrorSDoc loc $
        text "pattern synonym 'where' clause must bind the pattern synonym's name" <+>
        quotes (ppr patsyn_name) $$ ppr decl

    wrongNumberErr loc =
      parseErrorSDoc loc $
      text "pattern synonym 'where' clause cannot be empty" $$
      text "In the pattern synonym declaration for: " <+> ppr (patsyn_name)

recordPatSynErr :: SrcSpan -> LPat RdrName -> P a
recordPatSynErr loc pat =
    parseErrorSDoc loc $
    text "record syntax not supported for pattern synonym declarations:" $$
    ppr pat

mkConDeclH98 :: LEmbellished RdrName -> Maybe [LHsTyVarBndr RdrName]
                -> LHsContext RdrName -> HsConDeclDetails RdrName
                -> ConDecl RdrName

mkConDeclH98 name mb_forall cxt details
  = ConDeclH98 { con_name     = name
               , con_qvars    = fmap mkHsQTvs mb_forall
               , con_cxt      = Just cxt
                             -- AZ:TODO: when can cxt be Nothing?
                             --          remembering that () is a valid context.
               , con_details  = details
               , con_doc      = Nothing }

mkGadtDecl :: [LEmbellished RdrName]
           -> LHsSigType RdrName     -- Always a HsForAllTy
           -> ConDecl RdrName
mkGadtDecl names ty = ConDeclGADT { con_names = names
                                  , con_type  = ty
                                  , con_doc   = Nothing }

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

-- | Note [Sorting out the result type]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In a GADT declaration which is not a record, we put the whole constr type
-- into the res_ty for a ConDeclGADT for now; the renamer will unravel it once
-- it has sorted out operator fixities. Consider for example
--      C :: a :*: b -> a :*: b -> a :+: b
-- Initially this type will parse as
--       a :*: (b -> (a :*: (b -> (a :+: b))))
--
-- so it's hard to split up the arguments until we've done the precedence
-- resolution (in the renamer). On the other hand, for a record
--         { x,y :: Int } -> a :*: b
-- there is no doubt.  AND we need to sort records out so that
-- we can bring x,y into scope.  So:
--    * For PrefixCon we keep all the args in the res_ty
--    * For RecCon we do not

checkTyVarsP :: SDoc -> SDoc -> Located RdrName -> [LHsType RdrName] -> P (LHsQTyVars RdrName)
-- Same as checkTyVars, but in the P monad
checkTyVarsP pp_what equals_or_where tc tparms
  = eitherToP $ checkTyVars pp_what equals_or_where tc tparms

eitherToP :: Either (SrcSpan, SDoc) a -> P a
-- Adapts the Either monad to the P monad
eitherToP (Left (loc, doc)) = parseErrorSDoc loc doc
eitherToP (Right thing)     = return thing

checkTyVars :: SDoc -> SDoc -> Located RdrName -> [LHsType RdrName]
            -> Either (SrcSpan, SDoc) (LHsQTyVars RdrName)
-- Check whether the given list of type parameters are all type variables
-- (possibly with a kind signature)
-- We use the Either monad because it's also called (via mkATDefault) from
-- Convert.hs
checkTyVars pp_what equals_or_where tc tparms
  = do { tvs <- mapM chk tparms
       ; return (mkHsQTvs tvs) }
  where

    chk (L _ (HsParTy ty)) = chk ty
    chk (L _ (HsAppsTy [L _ (HsAppPrefix ty)])) = chk ty

        -- Check that the name space is correct!
    chk (L l (HsKindSig
            (L _ (HsAppsTy [L _ (HsAppPrefix (L lv (HsTyVar _ (L _ tv))))])) k))
        | isRdrTyVar $ unEmb tv = return (L l (KindedTyVar (L lv $ unEmb tv) k))
    chk (L l (HsTyVar _ (L ltv tv)))
        | isRdrTyVar $ unEmb tv = return (L l (UserTyVar (L ltv $ unEmb tv)))
    chk t@(L loc _)
        = Left (loc,
                vcat [ text "Unexpected type" <+> quotes (ppr t)
                     , text "In the" <+> pp_what <+> ptext (sLit "declaration for") <+> quotes (ppr tc)
                     , vcat[ (text "A" <+> pp_what <+> ptext (sLit "declaration should have form"))
                     , nest 2 (pp_what <+> ppr tc
                                       <+> hsep (map text (takeList tparms allNameStrings))
                                       <+> equals_or_where) ] ])

whereDots, equalsDots :: SDoc
-- Second argument to checkTyVars
whereDots  = text "where ..."
equalsDots = text "= ..."

checkDatatypeContext :: Maybe (LHsContext RdrName) -> P ()
checkDatatypeContext Nothing = return ()
checkDatatypeContext (Just (L loc c))
    = do allowed <- extension datatypeContextsEnabled
         unless allowed $
             parseErrorSDoc loc
                 (text "Illegal datatype context (use DatatypeContexts):" <+>
                  pprHsContext c)

checkRecordSyntax :: Outputable a => Located a -> P (Located a)
checkRecordSyntax lr@(L loc r)
    = do allowed <- extension traditionalRecordSyntaxEnabled
         if allowed
             then return lr
             else parseErrorSDoc loc
                      (text "Illegal record syntax (use TraditionalRecordSyntax):" <+>
                       ppr r)

checkTyClHdr :: Bool               -- True  <=> class header
                                   -- False <=> type header
             -> LHsType RdrName
             -> P (Located RdrName,          -- the head symbol (type or class name)
                   [LHsType RdrName],        -- parameters of head symbol
                   LexicalFixity,         -- the declaration is in infix format
                   [AddAnn]) -- API Annotation for HsParTy when stripping parens
-- Well-formedness check and decomposition of type and class heads.
-- Decomposes   T ty1 .. tyn   into    (T, [ty1, ..., tyn])
--              Int :*: Bool   into    (:*:, [Int, Bool])
-- returning the pieces
checkTyClHdr is_cls ty
  = goL ty [] [] Prefix
  where
    goL (L l ty) acc ann fix = go l ty acc ann fix

    go l (HsTyVar _ (L _ tc)) acc ann fix
      | isRdrTc $ unEmb tc       = return (L l $ unEmb tc, acc, fix, ann)
    go _ (HsOpTy t1 ltc@(L _ tc) t2) acc ann _fix
      | isRdrTc tc               = return (ltc, t1:t2:acc, Infix, ann)
    go l (HsParTy ty)    acc ann fix = goL ty acc (ann ++ mkParensApiAnn l) fix
    go _ (HsAppTy t1 t2) acc ann fix = goL t1 (t2:acc) ann fix
    go _ (HsAppsTy ts)   acc ann _fix
      | Just (head, args, fixity) <- getAppsTyHead_maybe ts
      = goL head (args ++ acc) ann fixity

    go _ (HsAppsTy [L _ (HsAppInfix (L loc star))]) [] ann fix
      | occNameFS (rdrNameOcc $ unEmb star) == fsLit "*"
      = return (L loc (nameRdrName starKindTyConName), [], fix, ann)
      | occNameFS (rdrNameOcc $ unEmb star) == fsLit "★"
      = return (L loc (nameRdrName unicodeStarKindTyConName), [], fix, ann)

    go l (HsTupleTy HsBoxedOrConstraintTuple ts) [] ann fix
      = return (L l (nameRdrName tup_name), ts, fix, ann)
      where
        arity = length ts
        tup_name | is_cls    = cTupleTyConName arity
                 | otherwise = getName (tupleTyCon Boxed arity)
                 -- See Note [Unit tuples] in HsTypes  (TODO: is this still relevant?)
    go l _ _ _ _
      = parseErrorSDoc l (text "Malformed head of type or class declaration:"
                          <+> ppr ty)

checkContext :: LHsType RdrName -> P ([AddAnn],LHsContext RdrName)
checkContext (L l orig_t)
  = check [] (L l orig_t)
 where
  check anns (L lp (HsTupleTy _ ts))   -- (Eq a, Ord b) shows up as a tuple type
    = return (anns ++ mkParensApiAnn lp,L l ts)                -- Ditto ()

    -- don't let HsAppsTy get in the way
  check anns (L _ (HsAppsTy [L _ (HsAppPrefix ty)]))
    = check anns ty

  check anns (L lp1 (HsParTy ty))-- to be sure HsParTy doesn't get into the way
       = check anns' ty
         where anns' = if l == lp1 then anns
                                   else (anns ++ mkParensApiAnn lp1)

  check _anns _
    = return ([],L l [L l orig_t]) -- no need for anns, returning original

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: SDoc -> LHsExpr RdrName -> P (LPat RdrName)
checkPattern msg e = checkLPat msg e

checkPatterns :: SDoc -> [LHsExpr RdrName] -> P [LPat RdrName]
checkPatterns msg es = mapM (checkPattern msg) es

checkLPat :: SDoc -> LHsExpr RdrName -> P (LPat RdrName)
checkLPat msg e@(L l _) = checkPat msg l e []

checkPat :: SDoc -> SrcSpan -> LHsExpr RdrName -> [LPat RdrName]
         -> P (LPat RdrName)
checkPat _ loc (L l (HsVar (L _ c))) args
  | isRdrDataCon $ unEmb c
    = return (L loc (ConPatIn (L l $ unEmb c) (PrefixCon args)))
checkPat msg loc e args     -- OK to let this happen even if bang-patterns
                        -- are not enabled, because there is no valid
                        -- non-bang-pattern parse of (C ! e)
  | Just (e', args') <- splitBang e
  = do  { args'' <- checkPatterns msg args'
        ; checkPat msg loc e' (args'' ++ args) }
checkPat msg loc (L _ (HsApp f e)) args
  = do p <- checkLPat msg e
       checkPat msg loc f (p : args)
checkPat msg loc (L _ e) []
  = do p <- checkAPat msg loc e
       return (L loc p)
checkPat msg loc e _
  = patFail msg loc (unLoc e)

checkAPat :: SDoc -> SrcSpan -> HsExpr RdrName -> P (Pat RdrName)
checkAPat msg loc e0 = do
 pState <- getPState
 let opts = options pState
 case e0 of
   EWildPat       -> return (WildPat placeHolderType)
   HsVar (L l x)  -> return (VarPat (L l $ unEmb x))
   HsLit l        -> return (LitPat l)

   -- Overloaded numeric patterns (e.g. f 0 x = x)
   -- Negation is recorded separately, so that the literal is zero or +ve
   -- NB. Negative *primitive* literals are already handled by the lexer
   HsOverLit pos_lit          -> return (mkNPat (L loc pos_lit) Nothing)
   NegApp (L l (HsOverLit pos_lit)) _
                        -> return (mkNPat (L l pos_lit) (Just noSyntaxExpr))

   SectionR (L lb (HsVar (L _ bang))) e    -- (! x)
        | unEmb bang == bang_RDR
        -> do { bang_on <- extension bangPatEnabled
              ; if bang_on then do { e' <- checkLPat msg e
                                   ; addAnnotation loc AnnBang lb
                                   ; return  (BangPat e') }
                else parseErrorSDoc loc (text "Illegal bang-pattern (use BangPatterns):" $$ ppr e0) }

   ELazyPat e         -> checkLPat msg e >>= (return . LazyPat)
   EAsPat n e         -> checkLPat msg e >>= (return . AsPat n)
   -- view pattern is well-formed if the pattern is
   EViewPat expr patE  -> checkLPat msg patE >>=
                            (return . (\p -> ViewPat expr p placeHolderType))
   ExprWithTySig e t   -> do e <- checkLPat msg e
                             return (SigPatIn e t)

   -- n+k patterns
   OpApp (L nloc (HsVar (L _ n))) (L _ (HsVar (L _ plus))) _
         (L lloc (HsOverLit lit@(OverLit {ol_val = HsIntegral {}})))
                      | extopt LangExt.NPlusKPatterns opts &&
                          (unEmb plus == plus_RDR)
                      -> return (mkNPlusKPat (L nloc $ unEmb n) (L lloc lit))

   OpApp l op _fix r  -> do l <- checkLPat msg l
                            r <- checkLPat msg r
                            case op of
                               L cl (HsVar (L _ c))
                                 | isDataOcc (rdrNameOcc $ unEmb c)
                                      -> return (ConPatIn (L cl $ unEmb c)
                                                          (InfixCon l r))
                               _ -> patFail msg loc e0

   HsPar e            -> checkLPat msg e >>= (return . ParPat)
   ExplicitList _ _ es  -> do ps <- mapM (checkLPat msg) es
                              return (ListPat ps placeHolderType Nothing)
   ExplicitPArr _ es  -> do ps <- mapM (checkLPat msg) es
                            return (PArrPat ps placeHolderType)

   ExplicitTuple es b
     | all tupArgPresent es  -> do ps <- mapM (checkLPat msg)
                                              [e | L _ (Present e) <- es]
                                   return (TuplePat ps b [])
     | otherwise -> parseErrorSDoc loc (text "Illegal tuple section in pattern:" $$ ppr e0)

   ExplicitSum alt arity expr _ -> do
     p <- checkLPat msg expr
     return (SumPat p alt arity placeHolderType)

   RecordCon { rcon_con_name = c, rcon_flds = HsRecFields fs dd }
                        -> do fs <- mapM (checkPatField msg) fs
                              return (ConPatIn c (RecCon (HsRecFields fs dd)))
   HsSpliceE s | not (isTypedSplice s)
               -> return (SplicePat s)
   _           -> patFail msg loc e0

placeHolderPunRhs :: LHsExpr RdrName
-- The RHS of a punned record field will be filled in by the renamer
-- It's better not to make it an error, in case we want to print it when debugging
placeHolderPunRhs = noLoc (HsVar (noEmb pun_RDR))

plus_RDR, bang_RDR, pun_RDR :: RdrName
plus_RDR = mkUnqual varName (fsLit "+") -- Hack
bang_RDR = mkUnqual varName (fsLit "!") -- Hack
pun_RDR  = mkUnqual varName (fsLit "pun-right-hand-side")

checkPatField :: SDoc -> LHsRecField RdrName (LHsExpr RdrName)
              -> P (LHsRecField RdrName (LPat RdrName))
checkPatField msg (L l fld) = do p <- checkLPat msg (hsRecFieldArg fld)
                                 return (L l (fld { hsRecFieldArg = p }))

patFail :: SDoc -> SrcSpan -> HsExpr RdrName -> P a
patFail msg loc e = parseErrorSDoc loc err
    where err = text "Parse error in pattern:" <+> ppr e
             $$ msg


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SDoc
            -> LHsExpr RdrName
            -> Maybe (LHsType RdrName)
            -> Located (a,GRHSs RdrName (LHsExpr RdrName))
            -> P ([AddAnn],HsBind RdrName)

checkValDef msg lhs (Just sig) grhss
        -- x :: ty = rhs  parses as a *pattern* binding
  = checkPatBind msg (L (combineLocs lhs sig)
                        (ExprWithTySig lhs (mkLHsSigWcType sig))) grhss

checkValDef msg lhs opt_sig g@(L l (_,grhss))
  = do  { mb_fun <- isFunLhs lhs
        ; case mb_fun of
            Just (fun, is_infix, pats, ann) ->
              checkFunBind msg ann (getLoc lhs)
                           fun is_infix pats opt_sig (L l grhss)
            Nothing -> checkPatBind msg lhs g }

checkFunBind :: SDoc
             -> [AddAnn]
             -> SrcSpan
             -> Located RdrName
             -> LexicalFixity
             -> [LHsExpr RdrName]
             -> Maybe (LHsType RdrName)
             -> Located (GRHSs RdrName (LHsExpr RdrName))
             -> P ([AddAnn],HsBind RdrName)
checkFunBind msg ann lhs_loc fun is_infix pats opt_sig (L rhs_span grhss)
  = do  ps <- checkPatterns msg pats
        let match_span = combineSrcSpans lhs_loc rhs_span
        -- Add back the annotations stripped from any HsPar values in the lhs
        -- mapM_ (\a -> a match_span) ann
        return (ann, makeFunBind fun
                  [L match_span (Match { m_ctxt = FunRhs fun is_infix
                                       , m_pats = ps
                                       , m_type = opt_sig
                                       , m_grhss = grhss })])
        -- The span of the match covers the entire equation.
        -- That isn't quite right, but it'll do for now.

makeFunBind :: Located RdrName -> [LMatch RdrName (LHsExpr RdrName)]
            -> HsBind RdrName
-- Like HsUtils.mkFunBind, but we need to be able to set the fixity too
makeFunBind fn ms
  = FunBind { fun_id = fn,
              fun_matches = mkMatchGroup FromSource ms,
              fun_co_fn = idHsWrapper,
              bind_fvs = placeHolderNames,
              fun_tick = [] }

checkPatBind :: SDoc
             -> LHsExpr RdrName
             -> Located (a,GRHSs RdrName (LHsExpr RdrName))
             -> P ([AddAnn],HsBind RdrName)
checkPatBind msg lhs (L _ (_,grhss))
  = do  { lhs <- checkPattern msg lhs
        ; return ([],PatBind lhs grhss placeHolderType placeHolderNames
                    ([],[])) }

checkValSigLhs :: LHsExpr RdrName -> P (LEmbellished RdrName)
checkValSigLhs (L _ (HsVar (L l v)))
  | isUnqual $ unEmb v
  , not (isDataOcc (rdrNameOcc $ unEmb v))
  = return (L l v)

checkValSigLhs lhs@(L l _)
  = parseErrorSDoc l ((text "Invalid type signature:" <+>
                       ppr lhs <+> text ":: ...")
                      $$ text hint)
  where
    hint | foreign_RDR `looks_like` lhs
         = "Perhaps you meant to use ForeignFunctionInterface?"
         | default_RDR `looks_like` lhs
         = "Perhaps you meant to use DefaultSignatures?"
         | pattern_RDR `looks_like` lhs
         = "Perhaps you meant to use PatternSynonyms?"
         | otherwise
         = "Should be of form <variable> :: <type>"

    -- A common error is to forget the ForeignFunctionInterface flag
    -- so check for that, and suggest.  cf Trac #3805
    -- Sadly 'foreign import' still barfs 'parse error' because 'import' is a keyword
    looks_like s (L _ (HsVar (L _ v))) = unEmb v == s
    looks_like s (L _ (HsApp lhs _))   = looks_like s lhs
    looks_like _ _                     = False

    foreign_RDR = mkUnqual varName (fsLit "foreign")
    default_RDR = mkUnqual varName (fsLit "default")
    pattern_RDR = mkUnqual varName (fsLit "pattern")


checkDoAndIfThenElse :: LHsExpr RdrName
                     -> Bool
                     -> LHsExpr RdrName
                     -> Bool
                     -> LHsExpr RdrName
                     -> P ()
checkDoAndIfThenElse guardExpr semiThen thenExpr semiElse elseExpr
 | semiThen || semiElse
    = do pState <- getPState
         unless (extopt LangExt.DoAndIfThenElse (options pState)) $ do
             parseErrorSDoc (combineLocs guardExpr elseExpr)
                            (text "Unexpected semi-colons in conditional:"
                          $$ nest 4 expr
                          $$ text "Perhaps you meant to use DoAndIfThenElse?")
 | otherwise            = return ()
    where pprOptSemi True  = semi
          pprOptSemi False = empty
          expr = text "if"   <+> ppr guardExpr <> pprOptSemi semiThen <+>
                 text "then" <+> ppr thenExpr  <> pprOptSemi semiElse <+>
                 text "else" <+> ppr elseExpr


        -- The parser left-associates, so there should
        -- not be any OpApps inside the e's
splitBang :: LHsExpr RdrName -> Maybe (LHsExpr RdrName, [LHsExpr RdrName])
-- Splits (f ! g a b) into (f, [(! g), a, b])
splitBang (L _ (OpApp l_arg bang@(L _ (HsVar (L _ op))) _ r_arg))
  | unEmb op == bang_RDR = Just (l_arg, L l' (SectionR bang arg1) : argns)
  where
    l' = combineLocs bang arg1
    (arg1,argns) = split_bang r_arg []
    split_bang (L _ (HsApp f e)) es = split_bang f (e:es)
    split_bang e                 es = (e,es)
splitBang _ = Nothing

isFunLhs :: LHsExpr RdrName
      -> P (Maybe (Located RdrName, LexicalFixity, [LHsExpr RdrName],[AddAnn]))
-- A variable binding is parsed as a FunBind.
-- Just (fun, is_infix, arg_pats) if e is a function LHS
--
-- The whole LHS is parsed as a single expression.
-- Any infix operators on the LHS will parse left-associatively
-- E.g.         f !x y !z
--      will parse (rather strangely) as
--              (f ! x y) ! z
--      It's up to isFunLhs to sort out the mess
--
-- a .!. !b

isFunLhs e = go e [] []
 where
   go (L loc (HsVar (L _ f))) es ann
        | not (isRdrDataCon $ unEmb f)
                    = return (Just (L loc (unEmb f), Prefix, es, ann))
   go (L _ (HsApp f e)) es       ann = go f (e:es) ann
   go (L l (HsPar e))   es@(_:_) ann = go e es (ann ++ mkParensApiAnn l)

        -- For infix function defns, there should be only one infix *function*
        -- (though there may be infix *datacons* involved too).  So we don't
        -- need fixity info to figure out which function is being defined.
        --      a `K1` b `op` c `K2` d
        -- must parse as
        --      (a `K1` b) `op` (c `K2` d)
        -- The renamer checks later that the precedences would yield such a parse.
        --
        -- There is a complication to deal with bang patterns.
        --
        -- ToDo: what about this?
        --              x + 1 `op` y = ...

   go e@(L loc (OpApp l (L loc' (HsVar (L _ op))) fix r)) es ann
        | Just (e',es') <- splitBang e
        = do { bang_on <- extension bangPatEnabled
             ; if bang_on then go e' (es' ++ es) ann
               else return (Just (L loc' (unEmb op), Infix, (l:r:es), ann)) }
                -- No bangs; behave just like the next case
        | not (isRdrDataCon $ unEmb op)         -- We have found the function!
        = return (Just (L loc' (unEmb op), Infix, (l:r:es), ann))
        | otherwise                     -- Infix data con; keep going
        = do { mb_l <- go l es ann
             ; case mb_l of
                 Just (op', Infix, j : k : es', ann')
                   -> return (Just (op', Infix, j : op_app : es', ann'))
                   where
                     op_app = L loc (OpApp k (L loc' (HsVar (L loc' op))) fix r)
                 _ -> return Nothing }
   go _ _ _ = return Nothing


-- | Transform btype_no_ops with strict_mark's into HsEqTy's
-- (((~a) ~b) c) ~d ==> ((~a) ~ (b c)) ~ d
splitTilde :: LHsType RdrName -> P (LHsType RdrName)
splitTilde t = go t
  where go (L loc (HsAppTy t1 t2))
          | L lo (HsBangTy (HsSrcBang NoSourceText NoSrcUnpack SrcLazy) t2')
                                                                          <- t2
          = do
              moveAnnotations lo loc
              t1' <- go t1
              return (L loc (HsEqTy t1' t2'))
          | otherwise
          = do
              t1' <- go t1
              case t1' of
                (L lo (HsEqTy tl tr)) -> do
                  let lr = combineLocs tr t2
                  moveAnnotations lo loc
                  return (L loc (HsEqTy tl (L lr (HsAppTy tr t2))))
                t -> do
                  return (L loc (HsAppTy t t2))

        go t = return t


-- | Transform tyapps with strict_marks into uses of twiddle
-- [~a, ~b, c, ~d] ==> (~a) ~ b c ~ d
splitTildeApps :: [LHsAppType RdrName] -> P [LHsAppType RdrName]
splitTildeApps []         = return []
splitTildeApps (t : rest) = do
  rest' <- concatMapM go rest
  return (t : rest')
  where go (L l (HsAppPrefix
            (L loc (HsBangTy
                    (HsSrcBang NoSourceText NoSrcUnpack SrcLazy)
                    ty))))
          = addAnnotation l AnnTilde tilde_loc >>
            return
              [L tilde_loc (HsAppInfix (L tilde_loc $ EName eqTyCon_RDR)),
               L l (HsAppPrefix ty)]
               -- NOTE: no annotation is attached to an HsAppPrefix, so the
               --       surrounding SrcSpan is not critical
          where
            tilde_loc = srcSpanFirstCharacter loc

        go t = return [t]



---------------------------------------------------------------------------
-- Check for monad comprehensions
--
-- If the flag MonadComprehensions is set, return a `MonadComp' context,
-- otherwise use the usual `ListComp' context

checkMonadComp :: P (HsStmtContext Name)
checkMonadComp = do
    pState <- getPState
    return $ if extopt LangExt.MonadComprehensions (options pState)
                then MonadComp
                else ListComp

-- -------------------------------------------------------------------------
-- Checking arrow syntax.

-- We parse arrow syntax as expressions and check for valid syntax below,
-- converting the expression into a pattern at the same time.

checkCommand :: LHsExpr RdrName -> P (LHsCmd RdrName)
checkCommand lc = locMap checkCmd lc

locMap :: (SrcSpan -> a -> P b) -> Located a -> P (Located b)
locMap f (L l a) = f l a >>= (\b -> return $ L l b)

checkCmd :: SrcSpan -> HsExpr RdrName -> P (HsCmd RdrName)
checkCmd _ (HsArrApp e1 e2 ptt haat b) =
    return $ HsCmdArrApp e1 e2 ptt haat b
checkCmd _ (HsArrForm e mf args) =
    return $ HsCmdArrForm e Prefix mf args
checkCmd _ (HsApp e1 e2) =
    checkCommand e1 >>= (\c -> return $ HsCmdApp c e2)
checkCmd _ (HsLam mg) =
    checkCmdMatchGroup mg >>= (\mg' -> return $ HsCmdLam mg')
checkCmd _ (HsPar e) =
    checkCommand e >>= (\c -> return $ HsCmdPar c)
checkCmd _ (HsCase e mg) =
    checkCmdMatchGroup mg >>= (\mg' -> return $ HsCmdCase e mg')
checkCmd _ (HsIf cf ep et ee) = do
    pt <- checkCommand et
    pe <- checkCommand ee
    return $ HsCmdIf cf ep pt pe
checkCmd _ (HsLet lb e) =
    checkCommand e >>= (\c -> return $ HsCmdLet lb c)
checkCmd _ (HsDo DoExpr (L l stmts) ty) =
    mapM checkCmdLStmt stmts >>= (\ss -> return $ HsCmdDo (L l ss) ty)

checkCmd _ (OpApp eLeft op _fixity eRight) = do
    -- OpApp becomes a HsCmdArrForm with a (Just fixity) in it
    c1 <- checkCommand eLeft
    c2 <- checkCommand eRight
    let arg1 = L (getLoc c1) $ HsCmdTop c1 placeHolderType placeHolderType []
        arg2 = L (getLoc c2) $ HsCmdTop c2 placeHolderType placeHolderType []
    return $ HsCmdArrForm op Infix Nothing [arg1, arg2]

checkCmd l e = cmdFail l e

checkCmdLStmt :: ExprLStmt RdrName -> P (CmdLStmt RdrName)
checkCmdLStmt = locMap checkCmdStmt

checkCmdStmt :: SrcSpan -> ExprStmt RdrName -> P (CmdStmt RdrName)
checkCmdStmt _ (LastStmt e s r) =
    checkCommand e >>= (\c -> return $ LastStmt c s r)
checkCmdStmt _ (BindStmt pat e b f t) =
    checkCommand e >>= (\c -> return $ BindStmt pat c b f t)
checkCmdStmt _ (BodyStmt e t g ty) =
    checkCommand e >>= (\c -> return $ BodyStmt c t g ty)
checkCmdStmt _ (LetStmt bnds) = return $ LetStmt bnds
checkCmdStmt _ stmt@(RecStmt { recS_stmts = stmts }) = do
    ss <- mapM checkCmdLStmt stmts
    return $ stmt { recS_stmts = ss }
checkCmdStmt l stmt = cmdStmtFail l stmt

checkCmdMatchGroup :: MatchGroup RdrName (LHsExpr RdrName) -> P (MatchGroup RdrName (LHsCmd RdrName))
checkCmdMatchGroup mg@(MG { mg_alts = L l ms }) = do
    ms' <- mapM (locMap $ const convert) ms
    return $ mg { mg_alts = L l ms' }
    where convert (Match mf pat mty grhss) = do
            grhss' <- checkCmdGRHSs grhss
            return $ Match mf pat mty grhss'

checkCmdGRHSs :: GRHSs RdrName (LHsExpr RdrName) -> P (GRHSs RdrName (LHsCmd RdrName))
checkCmdGRHSs (GRHSs grhss binds) = do
    grhss' <- mapM checkCmdGRHS grhss
    return $ GRHSs grhss' binds

checkCmdGRHS :: LGRHS RdrName (LHsExpr RdrName) -> P (LGRHS RdrName (LHsCmd RdrName))
checkCmdGRHS = locMap $ const convert
  where
    convert (GRHS stmts e) = do
        c <- checkCommand e
--        cmdStmts <- mapM checkCmdLStmt stmts
        return $ GRHS {- cmdStmts -} stmts c


cmdFail :: SrcSpan -> HsExpr RdrName -> P a
cmdFail loc e = parseErrorSDoc loc (text "Parse error in command:" <+> ppr e)
cmdStmtFail :: SrcSpan -> Stmt RdrName (LHsExpr RdrName) -> P a
cmdStmtFail loc e = parseErrorSDoc loc
                    (text "Parse error in command statement:" <+> ppr e)

---------------------------------------------------------------------------
-- Miscellaneous utilities

checkPrecP :: Located (SourceText,Int) -> P (Located (SourceText,Int))
checkPrecP (L l (src,i))
 | 0 <= i && i <= maxPrecedence = return (L l (src,i))
 | otherwise
    = parseErrorSDoc l (text ("Precedence out of range: " ++ show i))

mkRecConstrOrUpdate
        :: LHsExpr RdrName
        -> SrcSpan
        -> ([LHsRecField RdrName (LHsExpr RdrName)], Bool)
        -> P (HsExpr RdrName)

mkRecConstrOrUpdate (L l (HsVar (L _ c))) _ (fs,dd)
  | isRdrDataCon $ unEmb c
  = return (mkRdrRecordCon (L l $ unEmb c) (mk_rec_fields fs dd))
mkRecConstrOrUpdate exp@(L l _) _ (fs,dd)
  | dd        = parseErrorSDoc l (text "You cannot use `..' in a record update")
  | otherwise = return (mkRdrRecordUpd exp (map (fmap mk_rec_upd_field) fs))

mkRdrRecordUpd :: LHsExpr RdrName -> [LHsRecUpdField RdrName] -> HsExpr RdrName
mkRdrRecordUpd exp flds
  = RecordUpd { rupd_expr = exp
              , rupd_flds = flds
              , rupd_cons    = PlaceHolder, rupd_in_tys  = PlaceHolder
              , rupd_out_tys = PlaceHolder, rupd_wrap    = PlaceHolder }

mkRdrRecordCon :: Located RdrName -> HsRecordBinds RdrName -> HsExpr RdrName
mkRdrRecordCon con flds
  = RecordCon { rcon_con_name = con, rcon_flds = flds
              , rcon_con_expr = noPostTcExpr, rcon_con_like = PlaceHolder }

mk_rec_fields :: [LHsRecField id arg] -> Bool -> HsRecFields id arg
mk_rec_fields fs False = HsRecFields { rec_flds = fs, rec_dotdot = Nothing }
mk_rec_fields fs True  = HsRecFields { rec_flds = fs, rec_dotdot = Just (length fs) }

mk_rec_upd_field :: HsRecField RdrName (LHsExpr RdrName) -> HsRecUpdField RdrName
mk_rec_upd_field (HsRecField (L loc (FieldOcc rdr _)) arg pun)
  = HsRecField (L loc (Unambiguous rdr PlaceHolder)) arg pun

mkInlinePragma :: SourceText -> (InlineSpec, RuleMatchInfo) -> Maybe Activation
               -> InlinePragma
-- The (Maybe Activation) is because the user can omit
-- the activation spec (and usually does)
mkInlinePragma src (inl, match_info) mb_act
  = InlinePragma { inl_src = src -- Note [Pragma source text] in BasicTypes
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
         -> (Located StringLiteral, Located RdrName, LHsSigType RdrName)
         -> P (HsDecl RdrName)
mkImport cconv safety (L loc (StringLiteral esrc entity), v, ty) =
    case cconv of
      L _ CCallConv          -> mkCImport
      L _ CApiConv           -> mkCImport
      L _ StdCallConv        -> mkCImport
      L _ PrimCallConv       -> mkOtherImport
      L _ JavaScriptCallConv -> mkOtherImport
  where
    -- Parse a C-like entity string of the following form:
    --   "[static] [chname] [&] [cid]" | "dynamic" | "wrapper"
    -- If 'cid' is missing, the function name 'v' is used instead as symbol
    -- name (cf section 8.5.1 in Haskell 2010 report).
    mkCImport = do
      let e = unpackFS entity
      case parseCImport cconv safety (mkExtName (unLoc v)) e (L loc esrc) of
        Nothing         -> parseErrorSDoc loc (text "Malformed entity string")
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

    returnSpec spec = return $ ForD $ ForeignImport
          { fd_name   = v
          , fd_sig_ty = ty
          , fd_co     = noForeignImportCoercionYet
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

   hdr_char c = not (isSpace c) -- header files are filenames, which can contain
                                -- pretty much any char (depending on the platform),
                                -- so just accept any non-space character
   id_first_char c = isAlpha    c || c == '_'
   id_char       c = isAlphaNum c || c == '_'

   cimp nm = (ReadP.char '&' >> skipSpaces >> CLabel <$> cid)
             +++ (do isFun <- case cconv of
                              L _ CApiConv ->
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
         -> (Located StringLiteral, Located RdrName, LHsSigType RdrName)
         -> P (HsDecl RdrName)
mkExport (L lc cconv) (L le (StringLiteral esrc entity), v, ty)
 = return $ ForD $
   ForeignExport { fd_name = v, fd_sig_ty = ty
                 , fd_co = noForeignExportCoercionYet
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

mkModuleImpExp :: Located ImpExpQcSpec -> ImpExpSubSpec -> P (IE RdrName)
mkModuleImpExp (L l specname) subs =
  case subs of
    ImpExpAbs
      | isVarNameSpace (rdrNameSpace name)
                               -> return $ IEVar (L l (ieNameFromSpec specname))
      | otherwise              -> IEThingAbs . L l <$> nameT
    ImpExpAll                  -> IEThingAll . L l <$> nameT
    ImpExpList xs              ->
      (\newName -> IEThingWith (L l newName) NoIEWildcard (wrapped xs) [])
        <$> nameT
    ImpExpAllWith xs                       ->
      do allowed <- extension patternSynonymsEnabled
         if allowed
          then
            let withs = map unLoc xs
                pos   = maybe NoIEWildcard IEWildcard
                          (findIndex isImpExpQcWildcard withs)
                ies   = wrapped $ filter (not . isImpExpQcWildcard . unLoc) xs
            in (\newName -> IEThingWith (L l newName) pos ies []) <$> nameT
          else parseErrorSDoc l
            (text "Illegal export form (use PatternSynonyms to enable)")
  where
    name = ieNameVal specname
    nameT =
      if isVarNameSpace (rdrNameSpace name)
        then parseErrorSDoc l
              (text "Expecting a type constructor but found a variable,"
               <+> quotes (ppr name) <> text "."
              $$ if isSymOcc $ rdrNameOcc name
                   then text "If" <+> quotes (ppr name) <+> text "is a type constructor"
                    <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
                   else empty)
        else return $ ieNameFromSpec specname

    ieNameVal (ImpExpQcName ln)  = unLoc ln
    ieNameVal (ImpExpQcType ln)  = unLoc ln
    ieNameVal (ImpExpQcWildcard) = panic "ieNameVal got wildcard"

    ieNameFromSpec (ImpExpQcName ln)  = IEName ln
    ieNameFromSpec (ImpExpQcType ln)  = IEType ln
    ieNameFromSpec (ImpExpQcWildcard) = panic "ieName got wildcard"

    wrapped = map (\(L l x) -> L l (ieNameFromSpec x))

mkTypeImpExp :: Located RdrName   -- TcCls or Var name space
             -> P (Located RdrName)
mkTypeImpExp name =
  do allowed <- extension explicitNamespacesEnabled
     if allowed
       then return (fmap (`setRdrNameSpace` tcClsName) name)
       else parseErrorSDoc (getLoc name)
              (text "Illegal keyword 'type' (use ExplicitNamespaces to enable)")

checkImportSpec :: Located [LIE RdrName] -> P (Located [LIE RdrName])
checkImportSpec ie@(L _ specs) =
    case [l | (L l (IEThingWith _ (IEWildcard _) _ _)) <- specs] of
      [] -> return ie
      (l:_) -> importSpecError l
  where
    importSpecError l =
      parseErrorSDoc l
        (text "Illegal import form, this syntax can only be used to bundle"
        $+$ text "pattern synonyms with types in module exports.")

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
-- Misc utils

parseErrorSDoc :: SrcSpan -> SDoc -> P a
parseErrorSDoc span s = failSpanMsgP span s

data SumOrTuple
  = Sum ConTag Arity (LHsExpr RdrName)
  | Tuple [LHsTupArg RdrName]

mkSumOrTuple :: Boxity -> SrcSpan -> SumOrTuple -> P (HsExpr RdrName)

-- Tuple
mkSumOrTuple boxity _ (Tuple es) = return (ExplicitTuple es boxity)

-- Sum
mkSumOrTuple Unboxed _ (Sum alt arity e) =
    return (ExplicitSum alt arity e PlaceHolder)
mkSumOrTuple Boxed l (Sum alt arity (L _ e)) =
    parseErrorSDoc l (hang (text "Boxed sums not supported:") 2 (ppr_boxed_sum alt arity e))
  where
    ppr_boxed_sum :: ConTag -> Arity -> HsExpr RdrName -> SDoc
    ppr_boxed_sum alt arity e =
      text "(" <+> ppr_bars (alt - 1) <+> ppr e <+> ppr_bars (arity - alt) <+> text ")"

    ppr_bars n = hsep (replicate n (Outputable.char '|'))
