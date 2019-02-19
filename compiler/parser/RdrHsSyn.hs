--
--  (c) The University of Glasgow 2002-2006
--

-- Functions over HsSyn specialised to RdrName.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}

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
        mkInlinePragma,
        mkPatSynMatchGroup,
        mkRecConstrOrUpdate, -- HsExp -> [HsFieldUpdate] -> P HsExp
        mkTyClD, mkInstD,
        mkRdrRecordCon, mkRdrRecordUpd,
        setRdrNameSpace,
        filterCTuple,

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
        mkATDefault,

        -- Bunch of functions in the parser monad for
        -- checking and constructing values
        checkBlockArguments,
        checkPrecP,           -- Int -> P Int
        checkContext,         -- HsType -> P HsContext
        checkPattern,         -- HsExp -> P HsPat
        bang_RDR,
        isBangRdr,
        checkPatterns,        -- SrcLoc -> [HsExp] -> P [HsPat]
        checkMonadComp,       -- P (HsStmtContext RdrName)
        checkCommand,         -- LHsExpr RdrName -> P (LHsCmd RdrName)
        checkValDef,          -- (SrcLoc, HsExp, HsRhs, [HsDecl]) -> P HsDecl
        checkValSigLhs,
        checkDoAndIfThenElse,
        LRuleTyTmVar, RuleTyTmVar(..),
        mkRuleBndrs, mkRuleTyVarBndrs,
        checkRuleTyVarBndrNames,
        checkRecordSyntax,
        checkEmptyGADTs,
        parseErrorSDoc, hintBangPat,
        TyEl(..), mergeOps, mergeDataCon,

        -- Help with processing exports
        ImpExpSubSpec(..),
        ImpExpQcSpec(..),
        mkModuleImpExp,
        mkTypeImpExp,
        mkImpExpSubSpec,
        checkImportSpec,

        -- Token symbols
        forallSym,
        starSym,

        -- Warnings and errors
        warnStarIsType,
        failOpFewArgs,

        SumOrTuple (..), mkSumOrTuple

    ) where

import GhcPrelude
import HsSyn            -- Lots of it
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
import Type             ( TyThing(..), funTyCon )
import TysWiredIn       ( cTupleTyConName, tupleTyCon, tupleDataCon,
                          nilDataConName, nilDataConKey,
                          listTyConName, listTyConKey, eqTyCon_RDR,
                          tupleTyConName, cTupleTyConNameArity_maybe )
import ForeignCall
import PrelNames        ( allNameStrings )
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
import DynFlags ( WarningFlag(..) )

import Control.Monad
import Text.ParserCombinators.ReadP as ReadP
import Data.Char
import qualified Data.Monoid as Monoid
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

mkTyClD :: LTyClDecl (GhcPass p) -> LHsDecl (GhcPass p)
mkTyClD (dL->L loc d) = cL loc (TyClD noExt d)

mkInstD :: LInstDecl (GhcPass p) -> LHsDecl (GhcPass p)
mkInstD (dL->L loc d) = cL loc (InstD noExt d)

mkClassDecl :: SrcSpan
            -> Located (Maybe (LHsContext GhcPs), LHsType GhcPs)
            -> Located (a,[LHsFunDep GhcPs])
            -> OrdList (LHsDecl GhcPs)
            -> P (LTyClDecl GhcPs)

mkClassDecl loc (dL->L _ (mcxt, tycl_hdr)) fds where_cls
  = do { (binds, sigs, ats, at_insts, _, docs) <- cvBindsAndSigs where_cls
       ; let cxt = fromMaybe (noLoc []) mcxt
       ; (cls, tparams, fixity, ann) <- checkTyClHdr True tycl_hdr
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars,annst) <- checkTyVarsP (text "class") whereDots cls tparams
       ; addAnnsAt loc annst -- Add any API Annotations to the top SrcSpan
       ; (at_defs, annsi) <- mapAndUnzipM (eitherToP . mkATDefault) at_insts
       ; sequence_ annsi
       ; return (cL loc (ClassDecl { tcdCExt = noExt, tcdCtxt = cxt
                                   , tcdLName = cls, tcdTyVars = tyvars
                                   , tcdFixity = fixity
                                   , tcdFDs = snd (unLoc fds)
                                   , tcdSigs = mkClassOpSigs sigs
                                   , tcdMeths = binds
                                   , tcdATs = ats, tcdATDefs = at_defs
                                   , tcdDocs  = docs })) }

mkATDefault :: LTyFamInstDecl GhcPs
            -> Either (SrcSpan, SDoc) (LTyFamDefltEqn GhcPs, P ())
-- ^ Take a type-family instance declaration and turn it into
-- a type-family default equation for a class declaration.
-- We parse things as the former and use this function to convert to the latter
--
-- We use the Either monad because this also called from "Convert".
--
-- The @P ()@ we return corresponds represents an action which will add
-- some necessary paren annotations to the parsing context. Naturally, this
-- is not something that the "Convert" use cares about.
mkATDefault (dL->L loc (TyFamInstDecl { tfid_eqn = HsIB { hsib_body = e }}))
      | FamEqn { feqn_tycon = tc, feqn_bndrs = bndrs, feqn_pats = pats
               , feqn_fixity = fixity, feqn_rhs = rhs } <- e
      = do { (tvs, anns) <- checkTyVars (text "default") equalsDots tc pats
           ; let f = cL loc (FamEqn { feqn_ext    = noExt
                                    , feqn_tycon  = tc
                                    , feqn_bndrs  = ASSERT( isNothing bndrs )
                                                    Nothing
                                    , feqn_pats   = tvs
                                    , feqn_fixity = fixity
                                    , feqn_rhs    = rhs })
           ; pure (f, addAnnsAt loc anns) }
mkATDefault (dL->L _ (TyFamInstDecl (HsIB _ (XFamEqn _)))) = panic "mkATDefault"
mkATDefault (dL->L _ (TyFamInstDecl (XHsImplicitBndrs _))) = panic "mkATDefault"
mkATDefault _ = panic "mkATDefault: Impossible Match"
                                -- due to #15884

mkTyData :: SrcSpan
         -> NewOrData
         -> Maybe (Located CType)
         -> Located (Maybe (LHsContext GhcPs), LHsType GhcPs)
         -> Maybe (LHsKind GhcPs)
         -> [LConDecl GhcPs]
         -> HsDeriving GhcPs
         -> P (LTyClDecl GhcPs)
mkTyData loc new_or_data cType (dL->L _ (mcxt, tycl_hdr))
         ksig data_cons maybe_deriv
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False tycl_hdr
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars, anns) <- checkTyVarsP (ppr new_or_data) equalsDots tc tparams
       ; addAnnsAt loc anns -- Add any API Annotations to the top SrcSpan
       ; defn <- mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
       ; return (cL loc (DataDecl { tcdDExt = noExt,
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
       ; return (HsDataDefn { dd_ext = noExt
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
       ; (tyvars, anns) <- checkTyVarsP (text "type") equalsDots tc tparams
       ; addAnnsAt loc anns -- Add any API Annotations to the top SrcSpan
       ; return (cL loc (SynDecl { tcdSExt = noExt
                                 , tcdLName = tc, tcdTyVars = tyvars
                                 , tcdFixity = fixity
                                 , tcdRhs = rhs })) }

mkTyFamInstEqn :: Maybe [LHsTyVarBndr GhcPs]
               -> LHsType GhcPs
               -> LHsType GhcPs
               -> P (TyFamInstEqn GhcPs,[AddAnn])
mkTyFamInstEqn bndrs lhs rhs
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; return (mkHsImplicitBndrs
                  (FamEqn { feqn_ext    = noExt
                          , feqn_tycon  = tc
                          , feqn_bndrs  = bndrs
                          , feqn_pats   = tparams
                          , feqn_fixity = fixity
                          , feqn_rhs    = rhs }),
                 ann) }

mkDataFamInst :: SrcSpan
              -> NewOrData
              -> Maybe (Located CType)
              -> (Maybe ( LHsContext GhcPs), Maybe [LHsTyVarBndr GhcPs]
                        , LHsType GhcPs)
              -> Maybe (LHsKind GhcPs)
              -> [LConDecl GhcPs]
              -> HsDeriving GhcPs
              -> P (LInstDecl GhcPs)
mkDataFamInst loc new_or_data cType (mcxt, bndrs, tycl_hdr)
              ksig data_cons maybe_deriv
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False tycl_hdr
       ; mapM_ (\a -> a loc) ann -- Add any API Annotations to the top SrcSpan
       ; defn <- mkDataDefn new_or_data cType mcxt ksig data_cons maybe_deriv
       ; return (cL loc (DataFamInstD noExt (DataFamInstDecl (mkHsImplicitBndrs
                  (FamEqn { feqn_ext    = noExt
                          , feqn_tycon  = tc
                          , feqn_bndrs  = bndrs
                          , feqn_pats   = tparams
                          , feqn_fixity = fixity
                          , feqn_rhs    = defn }))))) }

mkTyFamInst :: SrcSpan
            -> TyFamInstEqn GhcPs
            -> P (LInstDecl GhcPs)
mkTyFamInst loc eqn
  = return (cL loc (TyFamInstD noExt (TyFamInstDecl eqn)))

mkFamDecl :: SrcSpan
          -> FamilyInfo GhcPs
          -> LHsType GhcPs                   -- LHS
          -> Located (FamilyResultSig GhcPs) -- Optional result signature
          -> Maybe (LInjectivityAnn GhcPs)   -- Injectivity annotation
          -> P (LTyClDecl GhcPs)
mkFamDecl loc info lhs ksig injAnn
  = do { (tc, tparams, fixity, ann) <- checkTyClHdr False lhs
       ; addAnnsAt loc ann -- Add any API Annotations to the top SrcSpan
       ; (tyvars, anns) <- checkTyVarsP (ppr info) equals_or_where tc tparams
       ; addAnnsAt loc anns -- Add any API Annotations to the top SrcSpan
       ; return (cL loc (FamDecl noExt (FamilyDecl
                                           { fdExt       = noExt
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
-- but if she wrote, say,
--      f x            then behave as if she'd written $(f x)
--                     ie a SpliceD
--
-- Typed splices are not allowed at the top level, thus we do not represent them
-- as spliced declaration.  See #10945
mkSpliceDecl lexpr@(dL->L loc expr)
  | HsSpliceE _ splice@(HsUntypedSplice {}) <- expr
  = SpliceD noExt (SpliceDecl noExt (cL loc splice) ExplicitSplice)

  | HsSpliceE _ splice@(HsQuasiQuote {}) <- expr
  = SpliceD noExt (SpliceDecl noExt (cL loc splice) ExplicitSplice)

  | otherwise
  = SpliceD noExt (SpliceDecl noExt (cL loc (mkUntypedSplice NoParens lexpr))
                              ImplicitSplice)

mkRoleAnnotDecl :: SrcSpan
                -> Located RdrName                -- type being annotated
                -> [Located (Maybe FastString)]      -- roles
                -> P (LRoleAnnotDecl GhcPs)
mkRoleAnnotDecl loc tycon roles
  = do { roles' <- mapM parse_role roles
       ; return $ cL loc $ RoleAnnotDecl noExt tycon roles' }
  where
    role_data_type = dataTypeOf (undefined :: Role)
    all_roles = map fromConstr $ dataTypeConstrs role_data_type
    possible_roles = [(fsFromRole role, role) | role <- all_roles]

    parse_role (dL->L loc_role Nothing) = return $ cL loc_role Nothing
    parse_role (dL->L loc_role (Just role))
      = case lookup role possible_roles of
          Just found_role -> return $ cL loc_role $ Just found_role
          Nothing         ->
            let nearby = fuzzyLookup (unpackFS role)
                  (mapFst unpackFS possible_roles)
            in
            parseErrorSDoc loc_role
              (text "Illegal role name" <+> quotes (ppr role) $$
               suggestions nearby)
    parse_role _ = panic "parse_role: Impossible Match"
                                -- due to #15884

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
cvTopDecls :: OrdList (LHsDecl GhcPs) -> [LHsDecl GhcPs]
cvTopDecls decls = go (fromOL decls)
  where
    go :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
    go []                     = []
    go ((dL->L l (ValD x b)) : ds)
      = cL l' (ValD x b') : go ds'
        where (dL->L l' b', ds') = getMonoBind (cL l b) ds
    go (d : ds)                    = d : go ds

-- Declaration list may only contain value bindings and signatures.
cvBindGroup :: OrdList (LHsDecl GhcPs) -> P (HsValBinds GhcPs)
cvBindGroup binding
  = do { (mbs, sigs, fam_ds, tfam_insts
         , dfam_insts, _) <- cvBindsAndSigs binding
       ; ASSERT( null fam_ds && null tfam_insts && null dfam_insts)
         return $ ValBinds noExt mbs sigs }

cvBindsAndSigs :: OrdList (LHsDecl GhcPs)
  -> P (LHsBinds GhcPs, [LSig GhcPs], [LFamilyDecl GhcPs]
          , [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs], [LDocDecl])
-- Input decls contain just value bindings and signatures
-- and in case of class or instance declarations also
-- associated type declarations. They might also contain Haddock comments.
cvBindsAndSigs fb = go (fromOL fb)
  where
    go []              = return (emptyBag, [], [], [], [], [])
    go ((dL->L l (ValD _ b)) : ds)
      = do { (bs, ss, ts, tfis, dfis, docs) <- go ds'
           ; return (b' `consBag` bs, ss, ts, tfis, dfis, docs) }
      where
        (b', ds') = getMonoBind (cL l b) ds
    go ((dL->L l decl) : ds)
      = do { (bs, ss, ts, tfis, dfis, docs) <- go ds
           ; case decl of
               SigD _ s
                 -> return (bs, cL l s : ss, ts, tfis, dfis, docs)
               TyClD _ (FamDecl _ t)
                 -> return (bs, ss, cL l t : ts, tfis, dfis, docs)
               InstD _ (TyFamInstD { tfid_inst = tfi })
                 -> return (bs, ss, ts, cL l tfi : tfis, dfis, docs)
               InstD _ (DataFamInstD { dfid_inst = dfi })
                 -> return (bs, ss, ts, tfis, cL l dfi : dfis, docs)
               DocD _ d
                 -> return (bs, ss, ts, tfis, dfis, cL l d : docs)
               SpliceD _ d
                 -> parseErrorSDoc l $
                    hang (text "Declaration splices are allowed only" <+>
                          text "at the top level:")
                       2 (ppr d)
               _ -> pprPanic "cvBindsAndSigs" (ppr decl) }

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

getMonoBind (dL->L loc1 (FunBind { fun_id = fun_id1@(dL->L _ f1)
                                 , fun_matches =
                                   MG { mg_alts = (dL->L _ mtchs1) } }))
            binds
  | has_args mtchs1
  = go mtchs1 loc1 binds []
  where
    go mtchs loc
       ((dL->L loc2 (ValD _ (FunBind { fun_id = (dL->L _ f2)
                                    , fun_matches =
                                        MG { mg_alts = (dL->L _ mtchs2) } })))
         : binds) _
        | f1 == f2 = go (mtchs2 ++ mtchs)
                        (combineSrcSpans loc loc2) binds []
    go mtchs loc (doc_decl@(dL->L loc2 (DocD {})) : binds) doc_decls
        = let doc_decls' = doc_decl : doc_decls
          in go mtchs (combineSrcSpans loc loc2) binds doc_decls'
    go mtchs loc binds doc_decls
        = ( cL loc (makeFunBind fun_id1 (reverse mtchs))
          , (reverse doc_decls) ++ binds)
        -- Reverse the final matches, to get it back in the right order
        -- Do the same thing with the trailing doc comments

getMonoBind bind binds = (bind, binds)

has_args :: [LMatch GhcPs (LHsExpr GhcPs)] -> Bool
has_args []                                    = panic "RdrHsSyn:has_args"
has_args ((dL->L _ (Match { m_pats = args })) : _) = not (null args)
        -- Don't group together FunBinds if they have
        -- no arguments.  This is necessary now that variable bindings
        -- with no arguments are now treated as FunBinds rather
        -- than pattern bindings (tests/rename/should_fail/rnfail002).
has_args ((dL->L _ (XMatch _)) : _) = panic "has_args"
has_args (_ : _) = panic "has_args:Impossible Match" -- due to #15884

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

To further complicate matters, the interpretation of (!) and (~) is different
in constructors and types:

  (b1)   type T = C ! D
  (b2)   data T = C ! D
  (b3)   data T = C ! D => E

In (b1) and (b3), (!) is a type operator with two arguments: 'C' and 'D'. At
the same time, in (b2) it is a strictness annotation: 'C' is a data constructor
with a single strict argument 'D'. For the programmer, these cases are usually
easy to tell apart due to whitespace conventions:

  (b2)   data T = C !D         -- no space after the bang hints that
                               -- it is a strictness annotation

For the parser, on the other hand, this whitespace does not matter. We cannot
tell apart (b2) from (b3) until we encounter (=>), so it requires unlimited
lookahead.

The solution that accounts for all of these issues is to initially parse data
declarations and types as a reversed list of TyEl:

  data TyEl = TyElOpr RdrName
            | TyElOpd (HsType GhcPs)
            | TyElBang | TyElTilde
            | ...

For example, both occurences of (C ! D) in the following example are parsed
into equal lists of TyEl:

  data T = C ! D => C ! D   results in   [ TyElOpd (HsTyVar "D")
                                         , TyElBang
                                         , TyElOpd (HsTyVar "C") ]

Note that elements are in reverse order. Also, 'C' is parsed as a type
constructor (HsTyVar) even when it is a data constructor. We fix this in
`tyConToDataCon`.

By the time the list of TyEl is assembled, we have looked ahead enough to
decide whether to reduce using `mergeOps` (for types) or `mergeDataCon` (for
data constructors). These functions are where the actual job of parsing is
done.

-}

-- | Reinterpret a type constructor, including type operators, as a data
--   constructor.
-- See Note [Parsing data constructors is hard]
tyConToDataCon :: SrcSpan -> RdrName -> Either (SrcSpan, SDoc) (Located RdrName)
tyConToDataCon loc tc
  | isTcOcc occ || isDataOcc occ
  , isLexCon (occNameFS occ)
  = return (cL loc (setRdrNameSpace tc srcDataName))

  | otherwise
  = Left (loc, msg)
  where
    occ = rdrNameOcc tc
    msg = text "Not a data constructor:" <+> quotes (ppr tc)

mkPatSynMatchGroup :: Located RdrName
                   -> Located (OrdList (LHsDecl GhcPs))
                   -> P (MatchGroup GhcPs (LHsExpr GhcPs))
mkPatSynMatchGroup (dL->L loc patsyn_name) (dL->L _ decls) =
    do { matches <- mapM fromDecl (fromOL decls)
       ; when (null matches) (wrongNumberErr loc)
       ; return $ mkMatchGroup FromSource matches }
  where
    fromDecl (dL->L loc decl@(ValD _ (PatBind _
                             pat@(dL->L _ (ConPatIn ln@(dL->L _ name) details))
                                   rhs _))) =
        do { unless (name == patsyn_name) $
               wrongNameBindingErr loc decl
           ; match <- case details of
               PrefixCon pats -> return $ Match { m_ext = noExt
                                                , m_ctxt = ctxt, m_pats = pats
                                                , m_grhss = rhs }
                   where
                     ctxt = FunRhs { mc_fun = ln
                                   , mc_fixity = Prefix
                                   , mc_strictness = NoSrcStrict }

               InfixCon p1 p2 -> return $ Match { m_ext = noExt
                                                , m_ctxt = ctxt
                                                , m_pats = [p1, p2]
                                                , m_grhss = rhs }
                   where
                     ctxt = FunRhs { mc_fun = ln
                                   , mc_fixity = Infix
                                   , mc_strictness = NoSrcStrict }

               RecCon{} -> recordPatSynErr loc pat
           ; return $ cL loc match }
    fromDecl (dL->L loc decl) = extraDeclErr loc decl

    extraDeclErr loc decl =
        parseErrorSDoc loc $
        text "pattern synonym 'where' clause must contain a single binding:" $$
        ppr decl

    wrongNameBindingErr loc decl =
      parseErrorSDoc loc $
      text "pattern synonym 'where' clause must bind the pattern synonym's name"
      <+> quotes (ppr patsyn_name) $$ ppr decl

    wrongNumberErr loc =
      parseErrorSDoc loc $
      text "pattern synonym 'where' clause cannot be empty" $$
      text "In the pattern synonym declaration for: " <+> ppr (patsyn_name)

recordPatSynErr :: SrcSpan -> LPat GhcPs -> P a
recordPatSynErr loc pat =
    parseErrorSDoc loc $
    text "record syntax not supported for pattern synonym declarations:" $$
    ppr pat

mkConDeclH98 :: Located RdrName -> Maybe [LHsTyVarBndr GhcPs]
                -> Maybe (LHsContext GhcPs) -> HsConDeclDetails GhcPs
                -> ConDecl GhcPs

mkConDeclH98 name mb_forall mb_cxt args
  = ConDeclH98 { con_ext    = noExt
               , con_name   = name
               , con_forall = noLoc $ isJust mb_forall
               , con_ex_tvs = mb_forall `orElse` []
               , con_mb_cxt = mb_cxt
               , con_args   = args'
               , con_doc    = Nothing }
  where
    args' = nudgeHsSrcBangs args

mkGadtDecl :: [Located RdrName]
           -> LHsType GhcPs     -- Always a HsForAllTy
           -> (ConDecl GhcPs, [AddAnn])
mkGadtDecl names ty
  = (ConDeclGADT { con_g_ext  = noExt
                 , con_names  = names
                 , con_forall = cL l $ isLHsForAllTy ty'
                 , con_qvars  = mkHsQTvs tvs
                 , con_mb_cxt = mcxt
                 , con_args   = args'
                 , con_res_ty = res_ty
                 , con_doc    = Nothing }
    , anns1 ++ anns2)
  where
    (ty'@(dL->L l _),anns1) = peel_parens ty []
    (tvs, rho) = splitLHsForAllTy ty'
    (mcxt, tau, anns2) = split_rho rho []

    split_rho (dL->L _ (HsQualTy { hst_ctxt = cxt, hst_body = tau })) ann
      = (Just cxt, tau, ann)
    split_rho (dL->L l (HsParTy _ ty)) ann
      = split_rho ty (ann++mkParensApiAnn l)
    split_rho tau                  ann
      = (Nothing, tau, ann)

    (args, res_ty) = split_tau tau
    args' = nudgeHsSrcBangs args

    -- See Note [GADT abstract syntax] in HsDecls
    split_tau (dL->L _ (HsFunTy _ (dL->L loc (HsRecTy _ rf)) res_ty))
      = (RecCon (cL loc rf), res_ty)
    split_tau tau
      = (PrefixCon [], tau)

    peel_parens (dL->L l (HsParTy _ ty)) ann = peel_parens ty
                                                       (ann++mkParensApiAnn l)
    peel_parens ty                   ann = (ty, ann)

nudgeHsSrcBangs :: HsConDeclDetails GhcPs -> HsConDeclDetails GhcPs
-- ^ This function ensures that fields with strictness or packedness
-- annotations put these annotations on an outer 'HsBangTy'.
--
-- The problem is that in the parser, strictness and packedness annotations
-- bind more tightly that docstrings. However, the expectation downstream of
-- the parser (by functions such as 'getBangType' and 'getBangStrictness')
-- is that docstrings bind more tightly so that 'HsBangTy' may end up as the
-- top-level type.
--
-- See #15206
nudgeHsSrcBangs details
  = case details of
      PrefixCon as -> PrefixCon (map go as)
      RecCon r -> RecCon r
      InfixCon a1 a2 -> InfixCon (go a1) (go a2)
  where
    go (dL->L l (HsDocTy _ (dL->L _ (HsBangTy _ s lty)) lds)) =
      cL l (HsBangTy noExt s (addCLoc lty lds (HsDocTy noExt lty lds)))
    go lty = lty


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

-- | Replaces constraint tuple names with corresponding boxed ones.
filterCTuple :: RdrName -> RdrName
filterCTuple (Exact n)
  | Just arity <- cTupleTyConNameArity_maybe n
  = Exact $ tupleTyConName BoxedTuple arity
filterCTuple rdr = rdr


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

checkTyVarsP :: SDoc -> SDoc -> Located RdrName -> [LHsTypeArg GhcPs]
             -> P (LHsQTyVars GhcPs, [AddAnn])
-- Same as checkTyVars, but in the P monad
checkTyVarsP pp_what equals_or_where tc tparms
  = do { let checkedTvs = checkTyVars pp_what equals_or_where tc tparms
       ; eitherToP checkedTvs }

eitherToP :: Either (SrcSpan, SDoc) a -> P a
-- Adapts the Either monad to the P monad
eitherToP (Left (loc, doc)) = parseErrorSDoc loc doc
eitherToP (Right thing)     = return thing

checkTyVars :: SDoc -> SDoc -> Located RdrName -> [LHsTypeArg GhcPs]
            -> Either (SrcSpan, SDoc)
                      ( LHsQTyVars GhcPs  -- the synthesized type variables
                      , [AddAnn] )        -- action which adds annotations
-- ^ Check whether the given list of type parameters are all type variables
-- (possibly with a kind signature).
-- We use the Either monad because it's also called (via 'mkATDefault') from
-- "Convert".
checkTyVars pp_what equals_or_where tc tparms
  = do { (tvs, anns) <- fmap unzip $ mapM check tparms
       ; return (mkHsQTvs tvs, concat anns) }
  where
    check (HsTypeArg _ ki@(L loc _))
                              = Left (loc,
                                      vcat [ text "Unexpected type application" <+>
                                            text "@" <> ppr ki
                                          , text "In the" <+> pp_what <+>
                                            ptext (sLit "declaration for") <+> quotes (ppr tc)])
    check (HsValArg ty) = chkParens [] ty
    check (HsArgPar sp) = Left (sp, vcat [text "Malformed" <+> pp_what
                           <+> text "declaration for" <+> quotes (ppr tc)])
        -- Keep around an action for adjusting the annotations of extra parens
    chkParens :: [AddAnn] -> LHsType GhcPs
              -> Either (SrcSpan, SDoc) (LHsTyVarBndr GhcPs, [AddAnn])
    chkParens acc (dL->L l (HsParTy _ ty)) = chkParens (mkParensApiAnn l
                                                        ++ acc) ty
    chkParens acc ty = case chk ty of
      Left err -> Left err
      Right tv -> Right (tv, reverse acc)

        -- Check that the name space is correct!
    chk :: LHsType GhcPs -> Either (SrcSpan, SDoc) (LHsTyVarBndr GhcPs)
    chk (dL->L l (HsKindSig _ (dL->L lv (HsTyVar _ _ (dL->L _ tv))) k))
        | isRdrTyVar tv    = return (cL l (KindedTyVar noExt (cL lv tv) k))
    chk (dL->L l (HsTyVar _ _ (dL->L ltv tv)))
        | isRdrTyVar tv    = return (cL l (UserTyVar noExt (cL ltv tv)))
    chk t@(dL->L loc _)
        = Left (loc,
                vcat [ text "Unexpected type" <+> quotes (ppr t)
                     , text "In the" <+> pp_what
                       <+> ptext (sLit "declaration for") <+> quotes tc'
                     , vcat[ (text "A" <+> pp_what
                              <+> ptext (sLit "declaration should have form"))
                     , nest 2
                       (pp_what
                        <+> tc'
                        <+> hsep (map text (takeList tparms allNameStrings))
                        <+> equals_or_where) ] ])

    -- Avoid printing a constraint tuple in the error message. Print
    -- a plain old tuple instead (since that's what the user probably
    -- wrote). See #14907
    tc' = ppr $ fmap filterCTuple tc



whereDots, equalsDots :: SDoc
-- Second argument to checkTyVars
whereDots  = text "where ..."
equalsDots = text "= ..."

checkDatatypeContext :: Maybe (LHsContext GhcPs) -> P ()
checkDatatypeContext Nothing = return ()
checkDatatypeContext (Just c)
    = do allowed <- getBit DatatypeContextsBit
         unless allowed $
             parseErrorSDoc (getLoc c)
                 (text "Illegal datatype context (use DatatypeContexts):"
                  <+> pprLHsContext c)

type LRuleTyTmVar = Located RuleTyTmVar
data RuleTyTmVar = RuleTyTmVar (Located RdrName) (Maybe (LHsType GhcPs))
-- ^ Essentially a wrapper for a @RuleBndr GhcPs@

-- turns RuleTyTmVars into RuleBnrs - this is straightforward
mkRuleBndrs :: [LRuleTyTmVar] -> [LRuleBndr GhcPs]
mkRuleBndrs = fmap (fmap cvt_one)
  where cvt_one (RuleTyTmVar v Nothing)    = RuleBndr    noExt v
        cvt_one (RuleTyTmVar v (Just sig)) =
          RuleBndrSig noExt v (mkLHsSigWcType sig)

-- turns RuleTyTmVars into HsTyVarBndrs - this is more interesting
mkRuleTyVarBndrs :: [LRuleTyTmVar] -> [LHsTyVarBndr GhcPs]
mkRuleTyVarBndrs = fmap (fmap cvt_one)
  where cvt_one (RuleTyTmVar v Nothing)    = UserTyVar   noExt (fmap tm_to_ty v)
        cvt_one (RuleTyTmVar v (Just sig))
          = KindedTyVar noExt (fmap tm_to_ty v) sig
    -- takes something in namespace 'varName' to something in namespace 'tvName'
        tm_to_ty (Unqual occ) = Unqual (setOccNameSpace tvName occ)
        tm_to_ty _ = panic "mkRuleTyVarBndrs"

-- See note [Parsing explicit foralls in Rules] in Parser.y
checkRuleTyVarBndrNames :: [LHsTyVarBndr GhcPs] -> P ()
checkRuleTyVarBndrNames = mapM_ (check . fmap hsTyVarName)
  where check (dL->L loc (Unqual occ)) = do
          when ((occNameString occ ==) `any` ["forall","family","role"])
               (parseErrorSDoc loc (text $ "parse error on input "
                                    ++ occNameString occ))
        check _ = panic "checkRuleTyVarBndrNames"

checkRecordSyntax :: Outputable a => Located a -> P (Located a)
checkRecordSyntax lr@(dL->L loc r)
    = do allowed <- getBit TraditionalRecordSyntaxBit
         if allowed
             then return lr
             else parseErrorSDoc loc
                   (text "Illegal record syntax (use TraditionalRecordSyntax):"
                    <+> ppr r)

-- | Check if the gadt_constrlist is empty. Only raise parse error for
-- `data T where` to avoid affecting existing error message, see #8258.
checkEmptyGADTs :: Located ([AddAnn], [LConDecl GhcPs])
                -> P (Located ([AddAnn], [LConDecl GhcPs]))
checkEmptyGADTs gadts@(dL->L span (_, []))           -- Empty GADT declaration.
    = do gadtSyntax <- getBit GadtSyntaxBit   -- GADTs implies GADTSyntax
         if gadtSyntax
            then return gadts
            else parseErrorSDoc span $ vcat
              [ text "Illegal keyword 'where' in data declaration"
              , text "Perhaps you intended to use GADTs or a similar language"
              , text "extension to enable syntax: data T where"
              ]
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
    goL (dL->L l ty) acc ann fix = go l ty acc ann fix

    -- workaround to define '*' despite StarIsType
    go lp (HsParTy _ (dL->L l (HsStarTy _ isUni))) acc ann fix
      = do { warnStarBndr l
           ; let name = mkOccName tcClsName (starSym isUni)
           ; return (cL l (Unqual name), acc, fix, (ann ++ mkParensApiAnn lp)) }

    go l (HsTyVar _ _ (dL->L _ tc)) acc ann fix
      | isRdrTc tc               = return (cL l tc, acc, fix, ann)
    go _ (HsOpTy _ t1 ltc@(dL->L _ tc) t2) acc ann _fix
      | isRdrTc tc               = return (ltc, HsValArg t1:HsValArg t2:acc, Infix, ann)
    go l (HsParTy _ ty)    acc ann fix = goL ty acc (ann ++mkParensApiAnn l) fix
    go _ (HsAppTy _ t1 t2) acc ann fix = goL t1 (HsValArg t2:acc) ann fix
    go _ (HsAppKindTy l ty ki) acc ann fix = goL ty (HsTypeArg l ki:acc) ann fix
    go l (HsTupleTy _ HsBoxedOrConstraintTuple ts) [] ann fix
      = return (cL l (nameRdrName tup_name), map HsValArg ts, fix, ann)
      where
        arity = length ts
        tup_name | is_cls    = cTupleTyConName arity
                 | otherwise = getName (tupleTyCon Boxed arity)
          -- See Note [Unit tuples] in HsTypes  (TODO: is this still relevant?)
    go l _ _ _ _
      = parseErrorSDoc l (text "Malformed head of type or class declaration:"
                          <+> ppr ty)

-- | Yield a parse error if we have a function applied directly to a do block
-- etc. and BlockArguments is not enabled.
checkBlockArguments :: LHsExpr GhcPs -> P ()
checkBlockArguments expr = case unLoc expr of
    HsDo _ DoExpr _ -> check "do block"
    HsDo _ MDoExpr _ -> check "mdo block"
    HsLam {} -> check "lambda expression"
    HsCase {} -> check "case expression"
    HsLamCase {} -> check "lambda-case expression"
    HsLet {} -> check "let expression"
    HsIf {} -> check "if expression"
    HsProc {} -> check "proc expression"
    _ -> return ()
  where
    check element = do
      blockArguments <- getBit BlockArgumentsBit
      unless blockArguments $
        parseErrorSDoc (getLoc expr) $
          text "Unexpected " <> text element <> text " in function application:"
           $$ nest 4 (ppr expr)
           $$ text "You could write it with parentheses"
           $$ text "Or perhaps you meant to enable BlockArguments?"

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
checkContext (dL->L l orig_t)
  = check [] (cL l orig_t)
 where
  check anns (dL->L lp (HsTupleTy _ HsBoxedOrConstraintTuple ts))
    -- (Eq a, Ord b) shows up as a tuple type. Only boxed tuples can
    -- be used as context constraints.
    = return (anns ++ mkParensApiAnn lp,cL l ts)                -- Ditto ()

  check anns (dL->L lp1 (HsParTy _ ty))
                                  -- to be sure HsParTy doesn't get into the way
       = check anns' ty
         where anns' = if l == lp1 then anns
                                   else (anns ++ mkParensApiAnn lp1)

  -- no need for anns, returning original
  check _anns t = checkNoDocs msg t *> return ([],cL l [cL l orig_t])

  msg = text "data constructor context"

-- | Check recursively if there are any 'HsDocTy's in the given type.
-- This only works on a subset of types produced by 'btype_no_ops'
checkNoDocs :: SDoc -> LHsType GhcPs -> P ()
checkNoDocs msg ty = go ty
  where
    go (dL->L _ (HsAppKindTy _ ty ki)) = go ty *> go ki
    go (dL->L _ (HsAppTy _ t1 t2)) = go t1 *> go t2
    go (dL->L l (HsDocTy _ t ds)) = parseErrorSDoc l $ hsep
                                  [ text "Unexpected haddock", quotes (ppr ds)
                                  , text "on", msg, quotes (ppr t) ]
    go _ = pure ()

-- -------------------------------------------------------------------------
-- Checking Patterns.

-- We parse patterns as expressions and check for valid patterns below,
-- converting the expression into a pattern at the same time.

checkPattern :: SDoc -> LHsExpr GhcPs -> P (LPat GhcPs)
checkPattern msg e = checkLPat msg e

checkPatterns :: SDoc -> [LHsExpr GhcPs] -> P [LPat GhcPs]
checkPatterns msg es = mapM (checkPattern msg) es

checkLPat :: SDoc -> LHsExpr GhcPs -> P (LPat GhcPs)
checkLPat msg e@(dL->L l _) = checkPat msg l e []

checkPat :: SDoc -> SrcSpan -> LHsExpr GhcPs -> [LPat GhcPs]
         -> P (LPat GhcPs)
checkPat _ loc (dL->L l e@(HsVar _ (dL->L _ c))) args
  | isRdrDataCon c = return (cL loc (ConPatIn (cL l c) (PrefixCon args)))
  | not (null args) && patIsRec c =
      patFail (text "Perhaps you intended to use RecursiveDo") l e
checkPat msg loc e args     -- OK to let this happen even if bang-patterns
                        -- are not enabled, because there is no valid
                        -- non-bang-pattern parse of (C ! e)
  | Just (e', args') <- splitBang e
  = do  { args'' <- checkPatterns msg args'
        ; checkPat msg loc e' (args'' ++ args) }
checkPat msg loc (dL->L _ (HsApp _ f e)) args
  = do p <- checkLPat msg e
       checkPat msg loc f (p : args)
checkPat msg loc (dL->L _ e) []
  = do p <- checkAPat msg loc e
       return (cL loc p)
checkPat msg loc e _
  = patFail msg loc (unLoc e)

checkAPat :: SDoc -> SrcSpan -> HsExpr GhcPs -> P (Pat GhcPs)
checkAPat msg loc e0 = do
 nPlusKPatterns <- getBit NPlusKPatternsBit
 case e0 of
   EWildPat _ -> return (WildPat noExt)
   HsVar _ x  -> return (VarPat noExt x)
   HsLit _ (HsStringPrim _ _) -- (#13260)
       -> parseErrorSDoc loc (text "Illegal unboxed string literal in pattern:"
                              $$ ppr e0)

   HsLit _ l  -> return (LitPat noExt l)

   -- Overloaded numeric patterns (e.g. f 0 x = x)
   -- Negation is recorded separately, so that the literal is zero or +ve
   -- NB. Negative *primitive* literals are already handled by the lexer
   HsOverLit _ pos_lit          -> return (mkNPat (cL loc pos_lit) Nothing)
   NegApp _ (dL->L l (HsOverLit _ pos_lit)) _
                        -> return (mkNPat (cL l pos_lit) (Just noSyntaxExpr))

   SectionR _ (dL->L lb (HsVar _ (dL->L _ bang))) e    -- (! x)
        | bang == bang_RDR
        -> do { hintBangPat loc e0
              ; e' <- checkLPat msg e
              ; addAnnotation loc AnnBang lb
              ; return  (BangPat noExt e') }

   ELazyPat _ e         -> checkLPat msg e >>= (return . (LazyPat noExt))
   EAsPat _ n e         -> checkLPat msg e >>= (return . (AsPat noExt) n)
   -- view pattern is well-formed if the pattern is
   EViewPat _ expr patE -> checkLPat msg patE >>=
                            (return . (\p -> ViewPat noExt expr p))
   ExprWithTySig _ e t  -> do e <- checkLPat msg e
                              return (SigPat noExt e t)

   -- n+k patterns
   OpApp _ (dL->L nloc (HsVar _ (dL->L _ n)))
           (dL->L _    (HsVar _ (dL->L _ plus)))
           (dL->L lloc (HsOverLit _ lit@(OverLit {ol_val = HsIntegral {}})))
                      | nPlusKPatterns && (plus == plus_RDR)
                      -> return (mkNPlusKPat (cL nloc n) (cL lloc lit))
   OpApp _ l (dL->L cl (HsVar _ (dL->L _ c))) r
     | isDataOcc (rdrNameOcc c) -> do
         l <- checkLPat msg l
         r <- checkLPat msg r
         return (ConPatIn (cL cl c) (InfixCon l r))

   OpApp {}           -> patFail msg loc e0

   ExplicitList _ _ es -> do ps <- mapM (checkLPat msg) es
                             return (ListPat noExt ps)

   HsPar _ e          -> checkLPat msg e >>= (return . (ParPat noExt))

   ExplicitTuple _ es b
     | all tupArgPresent es  -> do ps <- mapM (checkLPat msg)
                                           [e | (dL->L _ (Present _ e)) <- es]
                                   return (TuplePat noExt ps b)
     | otherwise -> parseErrorSDoc loc (text "Illegal tuple section in pattern:"
                                        $$ ppr e0)

   ExplicitSum _ alt arity expr -> do
     p <- checkLPat msg expr
     return (SumPat noExt p alt arity)

   RecordCon { rcon_con_name = c, rcon_flds = HsRecFields fs dd }
                        -> do fs <- mapM (checkPatField msg) fs
                              return (ConPatIn c (RecCon (HsRecFields fs dd)))
   HsSpliceE _ s | not (isTypedSplice s)
               -> return (SplicePat noExt s)
   _           -> patFail msg loc e0

placeHolderPunRhs :: LHsExpr GhcPs
-- The RHS of a punned record field will be filled in by the renamer
-- It's better not to make it an error, in case we want to print it when
-- debugging
placeHolderPunRhs = noLoc (HsVar noExt (noLoc pun_RDR))

plus_RDR, bang_RDR, pun_RDR :: RdrName
plus_RDR = mkUnqual varName (fsLit "+") -- Hack
bang_RDR = mkUnqual varName (fsLit "!") -- Hack
pun_RDR  = mkUnqual varName (fsLit "pun-right-hand-side")

isBangRdr :: RdrName -> Bool
isBangRdr (Unqual occ) = occNameFS occ == fsLit "!"
isBangRdr _ = False

checkPatField :: SDoc -> LHsRecField GhcPs (LHsExpr GhcPs)
              -> P (LHsRecField GhcPs (LPat GhcPs))
checkPatField msg (dL->L l fld) = do p <- checkLPat msg (hsRecFieldArg fld)
                                     return (cL l (fld { hsRecFieldArg = p }))

patFail :: SDoc -> SrcSpan -> HsExpr GhcPs -> P a
patFail msg loc e = parseErrorSDoc loc err
    where err = text "Parse error in pattern:" <+> ppr e
             $$ msg

patIsRec :: RdrName -> Bool
patIsRec e = e == mkUnqual varName (fsLit "rec")


---------------------------------------------------------------------------
-- Check Equation Syntax

checkValDef :: SDoc
            -> SrcStrictness
            -> LHsExpr GhcPs
            -> Maybe (LHsType GhcPs)
            -> Located (a,GRHSs GhcPs (LHsExpr GhcPs))
            -> P ([AddAnn],HsBind GhcPs)

checkValDef msg _strictness lhs (Just sig) grhss
        -- x :: ty = rhs  parses as a *pattern* binding
  = checkPatBind msg (cL (combineLocs lhs sig)
                        (ExprWithTySig noExt lhs (mkLHsSigWcType sig))) grhss

checkValDef msg strictness lhs Nothing g@(dL->L l (_,grhss))
  = do  { mb_fun <- isFunLhs lhs
        ; case mb_fun of
            Just (fun, is_infix, pats, ann) ->
              checkFunBind msg strictness ann (getLoc lhs)
                           fun is_infix pats (cL l grhss)
            Nothing -> checkPatBind msg lhs g }

checkFunBind :: SDoc
             -> SrcStrictness
             -> [AddAnn]
             -> SrcSpan
             -> Located RdrName
             -> LexicalFixity
             -> [LHsExpr GhcPs]
             -> Located (GRHSs GhcPs (LHsExpr GhcPs))
             -> P ([AddAnn],HsBind GhcPs)
checkFunBind msg strictness ann lhs_loc fun is_infix pats (dL->L rhs_span grhss)
  = do  ps <- checkPatterns msg pats
        let match_span = combineSrcSpans lhs_loc rhs_span
        -- Add back the annotations stripped from any HsPar values in the lhs
        -- mapM_ (\a -> a match_span) ann
        return (ann, makeFunBind fun
                  [cL match_span (Match { m_ext = noExt
                                        , m_ctxt = FunRhs
                                            { mc_fun    = fun
                                            , mc_fixity = is_infix
                                            , mc_strictness = strictness }
                                        , m_pats = ps
                                        , m_grhss = grhss })])
        -- The span of the match covers the entire equation.
        -- That isn't quite right, but it'll do for now.

makeFunBind :: Located RdrName -> [LMatch GhcPs (LHsExpr GhcPs)]
            -> HsBind GhcPs
-- Like HsUtils.mkFunBind, but we need to be able to set the fixity too
makeFunBind fn ms
  = FunBind { fun_ext = noExt,
              fun_id = fn,
              fun_matches = mkMatchGroup FromSource ms,
              fun_co_fn = idHsWrapper,
              fun_tick = [] }

checkPatBind :: SDoc
             -> LHsExpr GhcPs
             -> Located (a,GRHSs GhcPs (LHsExpr GhcPs))
             -> P ([AddAnn],HsBind GhcPs)
checkPatBind msg lhs (dL->L _ (_,grhss))
  = do  { lhs <- checkPattern msg lhs
        ; return ([],PatBind noExt lhs grhss
                    ([],[])) }

checkValSigLhs :: LHsExpr GhcPs -> P (Located RdrName)
checkValSigLhs (dL->L _ (HsVar _ lrdr@(dL->L _ v)))
  | isUnqual v
  , not (isDataOcc (rdrNameOcc v))
  = return lrdr

checkValSigLhs lhs@(dL->L l _)
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
    -- Sadly 'foreign import' still barfs 'parse error' because
    --  'import' is a keyword
    looks_like s (dL->L _ (HsVar _ (dL->L _ v))) = v == s
    looks_like s (dL->L _ (HsApp _ lhs _))   = looks_like s lhs
    looks_like _ _                       = False

    foreign_RDR = mkUnqual varName (fsLit "foreign")
    default_RDR = mkUnqual varName (fsLit "default")
    pattern_RDR = mkUnqual varName (fsLit "pattern")


checkDoAndIfThenElse :: LHsExpr GhcPs
                     -> Bool
                     -> LHsExpr GhcPs
                     -> Bool
                     -> LHsExpr GhcPs
                     -> P ()
checkDoAndIfThenElse guardExpr semiThen thenExpr semiElse elseExpr
 | semiThen || semiElse
    = do doAndIfThenElse <- getBit DoAndIfThenElseBit
         unless doAndIfThenElse $ do
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
splitBang :: LHsExpr GhcPs -> Maybe (LHsExpr GhcPs, [LHsExpr GhcPs])
-- Splits (f ! g a b) into (f, [(! g), a, b])
splitBang (dL->L _ (OpApp _ l_arg bang@(dL->L _ (HsVar _ (dL->L _ op))) r_arg))
  | op == bang_RDR = Just (l_arg, cL l' (SectionR noExt bang arg1) : argns)
  where
    l' = combineLocs bang arg1
    (arg1,argns) = split_bang r_arg []
    split_bang (dL->L _ (HsApp _ f e)) es = split_bang f (e:es)
    split_bang e                       es = (e,es)
splitBang _ = Nothing

-- See Note [isFunLhs vs mergeDataCon]
isFunLhs :: LHsExpr GhcPs
      -> P (Maybe (Located RdrName, LexicalFixity, [LHsExpr GhcPs],[AddAnn]))
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
   go (dL->L loc (HsVar _ (dL->L _ f))) es ann
       | not (isRdrDataCon f)        = return (Just (cL loc f, Prefix, es, ann))
   go (dL->L _ (HsApp _ f e)) es       ann = go f (e:es) ann
   go (dL->L l (HsPar _ e))   es@(_:_) ann = go e es (ann ++ mkParensApiAnn l)

        -- Things of the form `!x` are also FunBinds
        -- See Note [FunBind vs PatBind]
   go (dL->L _ (SectionR _ (dL->L _ (HsVar _ (dL->L _ bang)))
                (dL->L l (HsVar _ (L _ var))))) [] ann
        | bang == bang_RDR
        , not (isRdrDataCon var)     = return (Just (cL l var, Prefix, [], ann))

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

   go e@(L loc (OpApp _ l (dL->L loc' (HsVar _ (dL->L _ op))) r)) es ann
        | Just (e',es') <- splitBang e
        = do { bang_on <- getBit BangPatBit
             ; if bang_on then go e' (es' ++ es) ann
               else return (Just (cL loc' op, Infix, (l:r:es), ann)) }
                -- No bangs; behave just like the next case
        | not (isRdrDataCon op)         -- We have found the function!
        = return (Just (cL loc' op, Infix, (l:r:es), ann))
        | otherwise                     -- Infix data con; keep going
        = do { mb_l <- go l es ann
             ; case mb_l of
                 Just (op', Infix, j : k : es', ann')
                   -> return (Just (op', Infix, j : op_app : es', ann'))
                   where
                     op_app = cL loc (OpApp noExt k
                               (cL loc' (HsVar noExt (cL loc' op))) r)
                 _ -> return Nothing }
   go _ _ _ = return Nothing

-- | Either an operator or an operand.
data TyEl = TyElOpr RdrName | TyElOpd (HsType GhcPs)
          | TyElKindApp SrcSpan (LHsType GhcPs)
          -- See Note [TyElKindApp SrcSpan interpretation]
          | TyElTilde | TyElBang
          | TyElUnpackedness ([AddAnn], SourceText, SrcUnpackedness)
          | TyElDocPrev HsDocString


{- Note [TyElKindApp SrcSpan interpretation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A TyElKindApp captures type application written in haskell as

    @ Foo

where Foo is some type.

The SrcSpan reflects both elements, and there are AnnAt and AnnVal API
Annotations attached to this SrcSpan for the specific locations of
each within it.
-}

instance Outputable TyEl where
  ppr (TyElOpr name) = ppr name
  ppr (TyElOpd ty) = ppr ty
  ppr (TyElKindApp _ ki) = text "@" <> ppr ki
  ppr TyElTilde = text "~"
  ppr TyElBang = text "!"
  ppr (TyElUnpackedness (_, _, unpk)) = ppr unpk
  ppr (TyElDocPrev doc) = ppr doc

tyElStrictness :: TyEl -> Maybe (AnnKeywordId, SrcStrictness)
tyElStrictness TyElTilde = Just (AnnTilde, SrcLazy)
tyElStrictness TyElBang = Just (AnnBang, SrcStrict)
tyElStrictness _ = Nothing

-- | Extract a strictness/unpackedness annotation from the front of a reversed
-- 'TyEl' list.
pStrictMark
  :: [Located TyEl] -- reversed TyEl
  -> Maybe ( Located HsSrcBang {- a strictness/upnackedness marker -}
           , [AddAnn]
           , [Located TyEl] {- remaining TyEl -})
pStrictMark ((dL->L l1 x1) : (dL->L l2 x2) : xs)
  | Just (strAnnId, str) <- tyElStrictness x1
  , TyElUnpackedness (unpkAnns, prag, unpk) <- x2
  = Just ( cL (combineSrcSpans l1 l2) (HsSrcBang prag unpk str)
         , unpkAnns ++ [\s -> addAnnotation s strAnnId l1]
         , xs )
pStrictMark ((dL->L l x1) : xs)
  | Just (strAnnId, str) <- tyElStrictness x1
  = Just ( cL l (HsSrcBang NoSourceText NoSrcUnpack str)
         , [\s -> addAnnotation s strAnnId l]
         , xs )
pStrictMark ((dL->L l x1) : xs)
  | TyElUnpackedness (anns, prag, unpk) <- x1
  = Just ( cL l (HsSrcBang prag unpk NoSrcStrict)
         , anns
         , xs )
pStrictMark _ = Nothing

pBangTy
  :: LHsType GhcPs  -- a type to be wrapped inside HsBangTy
  -> [Located TyEl] -- reversed TyEl
  -> ( Bool           {- has a strict mark been consumed? -}
     , LHsType GhcPs  {- the resulting BangTy -}
     , P ()           {- add annotations -}
     , [Located TyEl] {- remaining TyEl -})
pBangTy lt@(dL->L l1 _) xs =
  case pStrictMark xs of
    Nothing -> (False, lt, pure (), xs)
    Just (dL->L l2 strictMark, anns, xs') ->
      let bl = combineSrcSpans l1 l2
          bt = HsBangTy noExt strictMark lt
      in (True, cL bl bt, addAnnsAt bl anns, xs')

-- | Merge a /reversed/ and /non-empty/ soup of operators and operands
--   into a type.
--
-- User input: @F x y + G a b * X@
-- Input to 'mergeOps': [X, *, b, a, G, +, y, x, F]
-- Output corresponds to what the user wrote assuming all operators are of the
-- same fixity and right-associative.
--
-- It's a bit silly that we're doing it at all, as the renamer will have to
-- rearrange this, and it'd be easier to keep things separate.
--
-- See Note [Parsing data constructors is hard]
mergeOps :: [Located TyEl] -> P (LHsType GhcPs)
mergeOps ((dL->L l1 (TyElOpd t)) : xs)
  | (_, t', addAnns, xs') <- pBangTy (cL l1 t) xs
  , null xs' -- We accept a BangTy only when there are no preceding TyEl.
  = addAnns >> return t'
mergeOps all_xs = go (0 :: Int) [] id all_xs
  where
    -- NB. When modifying clauses in 'go', make sure that the reasoning in
    -- Note [Non-empty 'acc' in mergeOps clause [end]] is still correct.

    -- clause [unpk]:
    -- handle (NO)UNPACK pragmas
    go k acc ops_acc ((dL->L l (TyElUnpackedness (anns, unpkSrc, unpk))):xs) =
      if not (null acc) && null xs
      then do { acc' <- eitherToP $ mergeOpsAcc acc
              ; let a = ops_acc acc'
                    strictMark = HsSrcBang unpkSrc unpk NoSrcStrict
                    bl = combineSrcSpans l (getLoc a)
                    bt = HsBangTy noExt strictMark a
              ; addAnnsAt bl anns
              ; return (cL bl bt) }
      else parseErrorSDoc l unpkError
      where
        unpkSDoc = case unpkSrc of
          NoSourceText -> ppr unpk
          SourceText str -> text str <> text " #-}"
        unpkError
          | not (null xs) = unpkSDoc <+> text "cannot appear inside a type."
          | null acc && k == 0 = unpkSDoc <+> text "must be applied to a type."
          | otherwise =
              -- See Note [Impossible case in mergeOps clause [unpk]]
              panic "mergeOps.UNPACK: impossible position"

    -- clause [doc]:
    -- we do not expect to encounter any docs
    go _ _ _ ((dL->L l (TyElDocPrev _)):_) =
      failOpDocPrev l

    -- to improve error messages, we do a bit of guesswork to determine if the
    -- user intended a '!' or a '~' as a strictness annotation
    go k acc ops_acc ((dL->L l x) : xs)
      | Just (_, str) <- tyElStrictness x
      , let guess [] = True
            guess ((dL->L _ (TyElOpd _)):_) = False
            guess ((dL->L _ (TyElOpr _)):_) = True
            guess ((dL->L _ (TyElKindApp _ _)):_) = False
            guess ((dL->L _ (TyElTilde)):_) = True
            guess ((dL->L _ (TyElBang)):_) = True
            guess ((dL->L _ (TyElUnpackedness _)):_) = True
            guess ((dL->L _ (TyElDocPrev _)):xs') = guess xs'
            guess _ = panic "mergeOps.go.guess: Impossible Match"
                      -- due to #15884
        in guess xs
      = if not (null acc) && (k > 1 || length acc > 1)
        then do { a <- eitherToP (mergeOpsAcc acc)
                ; failOpStrictnessCompound (cL l str) (ops_acc a) }
        else failOpStrictnessPosition (cL l str)

    -- clause [opr]:
    -- when we encounter an operator, we must have accumulated
    -- something for its rhs, and there must be something left
    -- to build its lhs.
    go k acc ops_acc ((dL->L l (TyElOpr op)):xs) =
      if null acc || null (filter isTyElOpd xs)
        then failOpFewArgs (cL l op)
        else do { acc' <- eitherToP (mergeOpsAcc acc)
                ; go (k + 1) [] (\c -> mkLHsOpTy c (cL l op) (ops_acc acc')) xs }
      where
        isTyElOpd (dL->L _ (TyElOpd _)) = True
        isTyElOpd _ = False

    -- clause [opr.1]: interpret 'TyElTilde' as an operator
    go k acc ops_acc ((dL->L l TyElTilde):xs) =
      let op = eqTyCon_RDR
      in go k acc ops_acc (cL l (TyElOpr op):xs)

    -- clause [opr.2]: interpret 'TyElBang' as an operator
    go k acc ops_acc ((dL->L l TyElBang):xs) =
      let op = mkUnqual tcClsName (fsLit "!")
      in go k acc ops_acc (cL l (TyElOpr op):xs)

    -- clause [opd]:
    -- whenever an operand is encountered, it is added to the accumulator
    go k acc ops_acc ((dL->L l (TyElOpd a)):xs) = go k (HsValArg (cL l a):acc) ops_acc xs

    -- clause [tyapp]:
    -- whenever a type application is encountered, it is added to the accumulator
    go k acc ops_acc ((dL->L _ (TyElKindApp l a)):xs) = go k (HsTypeArg l a:acc) ops_acc xs

    -- clause [end]
    -- See Note [Non-empty 'acc' in mergeOps clause [end]]
    go _ acc ops_acc [] = do { acc' <- eitherToP (mergeOpsAcc acc)
                             ; return (ops_acc acc') }

    go _ _ _ _ = panic "mergeOps.go: Impossible Match"
                        -- due to #15884

mergeOpsAcc :: [HsArg (LHsType GhcPs) (LHsKind GhcPs)]
         -> Either (SrcSpan, SDoc) (LHsType GhcPs)
mergeOpsAcc [] = panic "mergeOpsAcc: empty input"
mergeOpsAcc (HsTypeArg _ (L loc ki):_)
  = Left (loc, text "Unexpected type application:" <+> ppr ki)
mergeOpsAcc (HsValArg ty : xs) = go1 ty xs
  where
    go1 :: LHsType GhcPs
        -> [HsArg (LHsType GhcPs) (LHsKind GhcPs)]
        -> Either (SrcSpan, SDoc) (LHsType GhcPs)
    go1 lhs []     = Right lhs
    go1 lhs (x:xs) = case x of
        HsValArg ty -> go1 (mkHsAppTy lhs ty) xs
        HsTypeArg loc ki -> let ty = mkHsAppKindTy loc lhs ki
                            in go1 ty xs
        HsArgPar _ -> go1 lhs xs
mergeOpsAcc (HsArgPar _: xs) = mergeOpsAcc xs

{- Note [Impossible case in mergeOps clause [unpk]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This case should never occur. Let us consider all possible
variations of 'acc', 'xs', and 'k':

  acc          xs        k
==============================
  null   |    null       0      -- "must be applied to a type"
  null   |  not null     0      -- "must be applied to a type"
not null |    null       0      -- successful parse
not null |  not null     0      -- "cannot appear inside a type"
  null   |    null      >0      -- handled in clause [opr]
  null   |  not null    >0      -- "cannot appear inside a type"
not null |    null      >0      -- successful parse
not null |  not null    >0      -- "cannot appear inside a type"

The (null acc && null xs && k>0) case is handled in clause [opr]
by the following check:

    if ... || null (filter isTyElOpd xs)
     then failOpFewArgs (L l op)

We know that this check has been performed because k>0, and by
the time we reach the end of the list (null xs), the only way
for (null acc) to hold is that there was not a single TyElOpd
between the operator and the end of the list. But this case is
caught by the check and reported as 'failOpFewArgs'.
-}

{- Note [Non-empty 'acc' in mergeOps clause [end]]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In clause [end] we need to know that 'acc' is non-empty to call 'mergeAcc'
without a check.

Running 'mergeOps' with an empty input list is forbidden, so we do not consider
this possibility. This means we'll hit at least one other clause before we
reach clause [end].

* Clauses [unpk] and [doc] do not call 'go' recursively, so we cannot hit
  clause [end] from there.
* Clause [opd] makes 'acc' non-empty, so if we hit clause [end] after it, 'acc'
  will be non-empty.
* Clause [opr] checks that (filter isTyElOpd xs) is not null - so we are going
  to hit clause [opd] at least once before we reach clause [end], making 'acc'
  non-empty.
* There are no other clauses.

Therefore, it is safe to omit a check for non-emptiness of 'acc' in clause
[end].

-}

pInfixSide :: [Located TyEl] -> Maybe (LHsType GhcPs, P (), [Located TyEl])
pInfixSide ((dL->L l (TyElOpd t)):xs)
  | (True, t', addAnns, xs') <- pBangTy (cL l t) xs
  = Just (t', addAnns, xs')
pInfixSide (el:xs1)
  | Just t1 <- pLHsTypeArg el
  = go [t1] xs1
   where
     go :: [HsArg (LHsType GhcPs) (LHsKind GhcPs)]
        -> [Located TyEl] -> Maybe (LHsType GhcPs, P (), [Located TyEl])
     go acc (el:xs)
       | Just t <- pLHsTypeArg el
       = go (t:acc) xs
     go acc xs = case mergeOpsAcc acc of
       Left _ -> Nothing
       Right acc' -> Just (acc', pure (), xs)
pInfixSide _ = Nothing

pLHsTypeArg :: Located TyEl -> Maybe (HsArg (LHsType GhcPs) (LHsKind GhcPs))
pLHsTypeArg (dL->L l (TyElOpd a)) = Just (HsValArg (L l a))
pLHsTypeArg (dL->L _ (TyElKindApp l a)) = Just (HsTypeArg l a)
pLHsTypeArg _ = Nothing

pDocPrev :: [Located TyEl] -> (Maybe LHsDocString, [Located TyEl])
pDocPrev = go Nothing
  where
    go mTrailingDoc ((dL->L l (TyElDocPrev doc)):xs) =
      go (mTrailingDoc `mplus` Just (cL l doc)) xs
    go mTrailingDoc xs = (mTrailingDoc, xs)

orErr :: Maybe a -> b -> Either b a
orErr (Just a) _ = Right a
orErr Nothing b = Left b

{- Note [isFunLhs vs mergeDataCon]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When parsing a function LHS, we do not know whether to treat (!) as
a strictness annotation or an infix operator:

  f ! a = ...

Without -XBangPatterns, this parses as   (!) f a = ...
   with -XBangPatterns, this parses as   f (!a) = ...

So in function declarations we opted to always parse as if -XBangPatterns
were off, and then rejig in 'isFunLhs'.

There are two downsides to this approach:

1. It is not particularly elegant, as there's a point in our pipeline where
   the representation is awfully incorrect. For instance,
      f !a b !c = ...
   will be first parsed as
      (f ! a b) ! c = ...

2. There are cases that it fails to cover, for instance infix declarations:
      !a + !b = ...
   will trigger an error.

Unfortunately, we cannot define different productions in the 'happy' grammar
depending on whether -XBangPatterns are enabled.

When parsing data constructors, we face a similar issue:
  (a) data T1 = C ! D
  (b) data T2 = C ! D => ...

In (a) the first bang is a strictness annotation, but in (b) it is a type
operator. A 'happy'-based parser does not have unlimited lookahead to check for
=>, so we must first parse (C ! D) into a common representation.

If we tried to mirror the approach used in functions, we would parse both sides
of => as types, and then rejig. However, we take a different route and use an
intermediate data structure, a reversed list of 'TyEl'.
See Note [Parsing data constructors is hard] for details.

This approach does not suffer from the issues of 'isFunLhs':

1. A sequence of 'TyEl' is a dedicated intermediate representation, not an
   incorrectly parsed type. Therefore, we do not have confusing states in our
   pipeline. (Except for representing data constructors as type variables).

2. We can handle infix data constructors with strictness annotations:
    data T a b = !a :+ !b

-}


-- | Merge a /reversed/ and /non-empty/ soup of operators and operands
--   into a data constructor.
--
-- User input: @C !A B -- ^ doc@
-- Input to 'mergeDataCon': ["doc", B, !, A, C]
-- Output: (C, PrefixCon [!A, B], "doc")
--
-- See Note [Parsing data constructors is hard]
-- See Note [isFunLhs vs mergeDataCon]
mergeDataCon
      :: [Located TyEl]
      -> P ( Located RdrName         -- constructor name
           , HsConDeclDetails GhcPs  -- constructor field information
           , Maybe LHsDocString      -- docstring to go on the constructor
           )
mergeDataCon all_xs =
  do { (addAnns, a) <- eitherToP res
     ; addAnns
     ; return a }
  where
    -- We start by splitting off the trailing documentation comment,
    -- if any exists.
    (mTrailingDoc, all_xs') = pDocPrev all_xs

    -- Determine whether the trailing documentation comment exists and is the
    -- only docstring in this constructor declaration.
    --
    -- When true, it means that it applies to the constructor itself:
    --    data T = C
    --             A
    --             B -- ^ Comment on C (singleDoc == True)
    --
    -- When false, it means that it applies to the last field:
    --    data T = C -- ^ Comment on C
    --             A -- ^ Comment on A
    --             B -- ^ Comment on B (singleDoc == False)
    singleDoc = isJust mTrailingDoc &&
                null [ () | (dL->L _ (TyElDocPrev _)) <- all_xs' ]

    -- The result of merging the list of reversed TyEl into a
    -- data constructor, along with [AddAnn].
    res = goFirst all_xs'

    -- Take the trailing docstring into account when interpreting
    -- the docstring near the constructor.
    --
    --    data T = C -- ^ docstring right after C
    --             A
    --             B -- ^ trailing docstring
    --
    -- 'mkConDoc' must be applied to the docstring right after C, so that it
    -- falls back to the trailing docstring when appropriate (see singleDoc).
    mkConDoc mDoc | singleDoc = mDoc `mplus` mTrailingDoc
                  | otherwise = mDoc

    -- The docstring for the last field of a data constructor.
    trailingFieldDoc | singleDoc = Nothing
                     | otherwise = mTrailingDoc

    goFirst [ dL->L l (TyElOpd (HsTyVar _ _ (dL->L _ tc))) ]
      = do { data_con <- tyConToDataCon l tc
           ; return (pure (), (data_con, PrefixCon [], mTrailingDoc)) }
    goFirst ((dL->L l (TyElOpd (HsRecTy _ fields))):xs)
      | (mConDoc, xs') <- pDocPrev xs
      , [ dL->L l' (TyElOpd (HsTyVar _ _ (dL->L _ tc))) ] <- xs'
      = do { data_con <- tyConToDataCon l' tc
           ; let mDoc = mTrailingDoc `mplus` mConDoc
           ; return (pure (), (data_con, RecCon (cL l fields), mDoc)) }
    goFirst [dL->L l (TyElOpd (HsTupleTy _ HsBoxedOrConstraintTuple ts))]
      = return ( pure ()
               , ( cL l (getRdrName (tupleDataCon Boxed (length ts)))
                 , PrefixCon ts
                 , mTrailingDoc ) )
    goFirst ((dL->L l (TyElOpd t)):xs)
      | (_, t', addAnns, xs') <- pBangTy (cL l t) xs
      = go addAnns Nothing [mkLHsDocTyMaybe t' trailingFieldDoc] xs'
    goFirst (L l (TyElKindApp _ _):_)
      = goInfix Monoid.<> Left (l, kindAppErr)
    goFirst xs
      = go (pure ()) mTrailingDoc [] xs

    go addAnns mLastDoc ts [ dL->L l (TyElOpd (HsTyVar _ _ (dL->L _ tc))) ]
      = do { data_con <- tyConToDataCon l tc
           ; return (addAnns, (data_con, PrefixCon ts, mkConDoc mLastDoc)) }
    go addAnns mLastDoc ts ((dL->L l (TyElDocPrev doc)):xs) =
      go addAnns (mLastDoc `mplus` Just (cL l doc)) ts xs
    go addAnns mLastDoc ts ((dL->L l (TyElOpd t)):xs)
      | (_, t', addAnns', xs') <- pBangTy (cL l t) xs
      , t'' <- mkLHsDocTyMaybe t' mLastDoc
      = go (addAnns >> addAnns') Nothing (t'':ts) xs'
    go _ _ _ ((dL->L _ (TyElOpr _)):_) =
      -- Encountered an operator: backtrack to the beginning and attempt
      -- to parse as an infix definition.
      goInfix
    go _ _ _ (L l (TyElKindApp _ _):_) =  goInfix Monoid.<> Left (l, kindAppErr)
    go _ _ _ _ = Left malformedErr
      where
        malformedErr =
          ( foldr combineSrcSpans noSrcSpan (map getLoc all_xs')
          , text "Cannot parse data constructor" <+>
            text "in a data/newtype declaration:" $$
            nest 2 (hsep . reverse $ map ppr all_xs'))

    goInfix =
      do { let xs0 = all_xs'
         ; (rhs_t, rhs_addAnns, xs1) <- pInfixSide xs0 `orErr` malformedErr
         ; let (mOpDoc, xs2) = pDocPrev xs1
         ; (op, xs3) <- case xs2 of
              (dL->L l (TyElOpr op)) : xs3 ->
                do { data_con <- tyConToDataCon l op
                   ; return (data_con, xs3) }
              _ -> Left malformedErr
         ; let (mLhsDoc, xs4) = pDocPrev xs3
         ; (lhs_t, lhs_addAnns, xs5) <- pInfixSide xs4 `orErr` malformedErr
         ; unless (null xs5) (Left malformedErr)
         ; let rhs = mkLHsDocTyMaybe rhs_t trailingFieldDoc
               lhs = mkLHsDocTyMaybe lhs_t mLhsDoc
               addAnns = lhs_addAnns >> rhs_addAnns
         ; return (addAnns, (op, InfixCon lhs rhs, mkConDoc mOpDoc)) }
      where
        malformedErr =
          ( foldr combineSrcSpans noSrcSpan (map getLoc all_xs')
          , text "Cannot parse an infix data constructor" <+>
            text "in a data/newtype declaration:" $$
            nest 2 (hsep . reverse $ map ppr all_xs'))

    kindAppErr =
      text "Unexpected kind application" <+>
      text "in a data/newtype declaration:" $$
      nest 2 (hsep . reverse $ map ppr all_xs')

---------------------------------------------------------------------------
-- | Check for monad comprehensions
--
-- If the flag MonadComprehensions is set, return a 'MonadComp' context,
-- otherwise use the usual 'ListComp' context

checkMonadComp :: P (HsStmtContext Name)
checkMonadComp = do
    monadComprehensions <- getBit MonadComprehensionsBit
    return $ if monadComprehensions
                then MonadComp
                else ListComp

-- -------------------------------------------------------------------------
-- Checking arrow syntax.

-- We parse arrow syntax as expressions and check for valid syntax below,
-- converting the expression into a pattern at the same time.

checkCommand :: LHsExpr GhcPs -> P (LHsCmd GhcPs)
checkCommand lc = locMap checkCmd lc

locMap :: (SrcSpan -> a -> P b) -> Located a -> P (Located b)
locMap f (dL->L l a) = f l a >>= (\b -> return $ cL l b)

checkCmd :: SrcSpan -> HsExpr GhcPs -> P (HsCmd GhcPs)
checkCmd _ (HsArrApp _ e1 e2 haat b) =
    return $ HsCmdArrApp noExt e1 e2 haat b
checkCmd _ (HsArrForm _ e mf args) =
    return $ HsCmdArrForm noExt e Prefix mf args
checkCmd _ (HsApp _ e1 e2) =
    checkCommand e1 >>= (\c -> return $ HsCmdApp noExt c e2)
checkCmd _ (HsLam _ mg) =
    checkCmdMatchGroup mg >>= (\mg' -> return $ HsCmdLam noExt mg')
checkCmd _ (HsPar _ e) =
    checkCommand e >>= (\c -> return $ HsCmdPar noExt c)
checkCmd _ (HsCase _ e mg) =
    checkCmdMatchGroup mg >>= (\mg' -> return $ HsCmdCase noExt e mg')
checkCmd _ (HsIf _ cf ep et ee) = do
    pt <- checkCommand et
    pe <- checkCommand ee
    return $ HsCmdIf noExt cf ep pt pe
checkCmd _ (HsLet _ lb e) =
    checkCommand e >>= (\c -> return $ HsCmdLet noExt lb c)
checkCmd _ (HsDo _ DoExpr (dL->L l stmts)) =
    mapM checkCmdLStmt stmts >>=
    (\ss -> return $ HsCmdDo noExt (cL l ss) )

checkCmd _ (OpApp _ eLeft op eRight) = do
    -- OpApp becomes a HsCmdArrForm with a (Just fixity) in it
    c1 <- checkCommand eLeft
    c2 <- checkCommand eRight
    let arg1 = cL (getLoc c1) $ HsCmdTop noExt c1
        arg2 = cL (getLoc c2) $ HsCmdTop noExt c2
    return $ HsCmdArrForm noExt op Infix Nothing [arg1, arg2]

checkCmd l e = cmdFail l e

checkCmdLStmt :: ExprLStmt GhcPs -> P (CmdLStmt GhcPs)
checkCmdLStmt = locMap checkCmdStmt

checkCmdStmt :: SrcSpan -> ExprStmt GhcPs -> P (CmdStmt GhcPs)
checkCmdStmt _ (LastStmt x e s r) =
    checkCommand e >>= (\c -> return $ LastStmt x c s r)
checkCmdStmt _ (BindStmt x pat e b f) =
    checkCommand e >>= (\c -> return $ BindStmt x pat c b f)
checkCmdStmt _ (BodyStmt x e t g) =
    checkCommand e >>= (\c -> return $ BodyStmt x c t g)
checkCmdStmt _ (LetStmt x bnds) = return $ LetStmt x bnds
checkCmdStmt _ stmt@(RecStmt { recS_stmts = stmts }) = do
    ss <- mapM checkCmdLStmt stmts
    return $ stmt { recS_ext = noExt, recS_stmts = ss }
checkCmdStmt _ (XStmtLR _) = panic "checkCmdStmt"
checkCmdStmt l stmt = cmdStmtFail l stmt

checkCmdMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
                   -> P (MatchGroup GhcPs (LHsCmd GhcPs))
checkCmdMatchGroup mg@(MG { mg_alts = (dL->L l ms) }) = do
    ms' <- mapM (locMap $ const convert) ms
    return $ mg { mg_ext = noExt
                , mg_alts = cL l ms' }
    where convert match@(Match { m_grhss = grhss }) = do
            grhss' <- checkCmdGRHSs grhss
            return $ match { m_ext = noExt, m_grhss = grhss'}
          convert (XMatch _) = panic "checkCmdMatchGroup.XMatch"
checkCmdMatchGroup (XMatchGroup {}) = panic "checkCmdMatchGroup"

checkCmdGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> P (GRHSs GhcPs (LHsCmd GhcPs))
checkCmdGRHSs (GRHSs x grhss binds) = do
    grhss' <- mapM checkCmdGRHS grhss
    return $ GRHSs x grhss' binds
checkCmdGRHSs (XGRHSs _) = panic "checkCmdGRHSs"

checkCmdGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> P (LGRHS GhcPs (LHsCmd GhcPs))
checkCmdGRHS = locMap $ const convert
  where
    convert (GRHS x stmts e) = do
        c <- checkCommand e
--        cmdStmts <- mapM checkCmdLStmt stmts
        return $ GRHS x {- cmdStmts -} stmts c
    convert (XGRHS _) = panic "checkCmdGRHS"


cmdFail :: SrcSpan -> HsExpr GhcPs -> P a
cmdFail loc e = parseErrorSDoc loc (text "Parse error in command:" <+> ppr e)
cmdStmtFail :: SrcSpan -> Stmt GhcPs (LHsExpr GhcPs) -> P a
cmdStmtFail loc e = parseErrorSDoc loc
                    (text "Parse error in command statement:" <+> ppr e)

---------------------------------------------------------------------------
-- Miscellaneous utilities

-- | Check if a fixity is valid. We support bypassing the usual bound checks
-- for some special operators.
checkPrecP
        :: Located (SourceText,Int)             -- ^ precedence
        -> Located (OrdList (Located RdrName))  -- ^ operators
        -> P ()
checkPrecP (dL->L l (_,i)) (dL->L _ ol)
 | 0 <= i, i <= maxPrecedence = pure ()
 | all specialOp ol = pure ()
 | otherwise = parseErrorSDoc l (text ("Precedence out of range: " ++ show i))
  where
    specialOp op = unLoc op `elem` [ eqTyCon_RDR
                                   , getRdrName funTyCon ]

mkRecConstrOrUpdate
        :: LHsExpr GhcPs
        -> SrcSpan
        -> ([LHsRecField GhcPs (LHsExpr GhcPs)], Bool)
        -> P (HsExpr GhcPs)

mkRecConstrOrUpdate (dL->L l (HsVar _ (dL->L _ c))) _ (fs,dd)
  | isRdrDataCon c
  = return (mkRdrRecordCon (cL l c) (mk_rec_fields fs dd))
mkRecConstrOrUpdate exp@(dL->L l _) _ (fs,dd)
  | dd        = parseErrorSDoc l (text "You cannot use `..' in a record update")
  | otherwise = return (mkRdrRecordUpd exp (map (fmap mk_rec_upd_field) fs))

mkRdrRecordUpd :: LHsExpr GhcPs -> [LHsRecUpdField GhcPs] -> HsExpr GhcPs
mkRdrRecordUpd exp flds
  = RecordUpd { rupd_ext  = noExt
              , rupd_expr = exp
              , rupd_flds = flds }

mkRdrRecordCon :: Located RdrName -> HsRecordBinds GhcPs -> HsExpr GhcPs
mkRdrRecordCon con flds
  = RecordCon { rcon_ext = noExt, rcon_con_name = con, rcon_flds = flds }

mk_rec_fields :: [LHsRecField id arg] -> Bool -> HsRecFields id arg
mk_rec_fields fs False = HsRecFields { rec_flds = fs, rec_dotdot = Nothing }
mk_rec_fields fs True  = HsRecFields { rec_flds = fs
                                     , rec_dotdot = Just (length fs) }

mk_rec_upd_field :: HsRecField GhcPs (LHsExpr GhcPs) -> HsRecUpdField GhcPs
mk_rec_upd_field (HsRecField (dL->L loc (FieldOcc _ rdr)) arg pun)
  = HsRecField (L loc (Unambiguous noExt rdr)) arg pun
mk_rec_upd_field (HsRecField (dL->L _ (XFieldOcc _)) _ _)
  = panic "mk_rec_upd_field"
mk_rec_upd_field (HsRecField _ _ _)
  = panic "mk_rec_upd_field: Impossible Match" -- due to #15884

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
      case parseCImport cconv safety (mkExtName (unLoc v)) e (cL loc esrc) of
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
        importSpec = CImport cconv safety Nothing funcTarget (cL loc esrc)

    returnSpec spec = return $ ForD noExt $ ForeignImport
          { fd_i_ext  = noExt
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
mkExport (dL->L lc cconv) (dL->L le (StringLiteral esrc entity), v, ty)
 = return $ ForD noExt $
   ForeignExport { fd_e_ext = noExt, fd_name = v, fd_sig_ty = ty
                 , fd_fe = CExport (cL lc (CExportStatic esrc entity' cconv))
                                   (cL le esrc) }
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
mkModuleImpExp (dL->L l specname) subs =
  case subs of
    ImpExpAbs
      | isVarNameSpace (rdrNameSpace name)
                       -> return $ IEVar noExt (cL l (ieNameFromSpec specname))
      | otherwise      -> IEThingAbs noExt . cL l <$> nameT
    ImpExpAll          -> IEThingAll noExt . cL l <$> nameT
    ImpExpList xs      ->
      (\newName -> IEThingWith noExt (cL l newName)
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
                        -> IEThingWith noExt (cL l newName) pos ies [])
               <$> nameT
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
                   then text "If" <+> quotes (ppr name)
                        <+> text "is a type constructor"
           <+> text "then enable ExplicitNamespaces and use the 'type' keyword."
                   else empty)
        else return $ ieNameFromSpec specname

    ieNameVal (ImpExpQcName ln)  = unLoc ln
    ieNameVal (ImpExpQcType ln)  = unLoc ln
    ieNameVal (ImpExpQcWildcard) = panic "ieNameVal got wildcard"

    ieNameFromSpec (ImpExpQcName ln)  = IEName ln
    ieNameFromSpec (ImpExpQcType ln)  = IEType ln
    ieNameFromSpec (ImpExpQcWildcard) = panic "ieName got wildcard"

    wrapped = map (onHasSrcSpan ieNameFromSpec)

mkTypeImpExp :: Located RdrName   -- TcCls or Var name space
             -> P (Located RdrName)
mkTypeImpExp name =
  do allowed <- getBit ExplicitNamespacesBit
     if allowed
       then return (fmap (`setRdrNameSpace` tcClsName) name)
       else parseErrorSDoc (getLoc name)
              (text "Illegal keyword 'type' (use ExplicitNamespaces to enable)")

checkImportSpec :: Located [LIE GhcPs] -> P (Located [LIE GhcPs])
checkImportSpec ie@(dL->L _ specs) =
    case [l | (dL->L l (IEThingWith _ _ (IEWildcard _) _ _)) <- specs] of
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
mkImpExpSubSpec [dL->L _ ImpExpQcWildcard] =
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

warnStarIsType :: SrcSpan -> P ()
warnStarIsType span = addWarning Opt_WarnStarIsType span msg
  where
    msg =  text "Using" <+> quotes (text "*")
           <+> text "(or its Unicode variant) to mean"
           <+> quotes (text "Data.Kind.Type")
        $$ text "relies on the StarIsType extension, which will become"
        $$ text "deprecated in the future."
        $$ text "Suggested fix: use" <+> quotes (text "Type")
           <+> text "from" <+> quotes (text "Data.Kind") <+> text "instead."

warnStarBndr :: SrcSpan -> P ()
warnStarBndr span = addWarning Opt_WarnStarBinder span msg
  where
    msg =  text "Found binding occurrence of" <+> quotes (text "*")
           <+> text "yet StarIsType is enabled."
        $$ text "NB. To use (or export) this operator in"
           <+> text "modules with StarIsType,"
        $$ text "    including the definition module, you must qualify it."

failOpFewArgs :: Located RdrName -> P a
failOpFewArgs (dL->L loc op) =
  do { star_is_type <- getBit StarIsTypeBit
     ; let msg = too_few $$ starInfo star_is_type op
     ; parseErrorSDoc loc msg }
  where
    too_few = text "Operator applied to too few arguments:" <+> ppr op

failOpDocPrev :: SrcSpan -> P a
failOpDocPrev loc = parseErrorSDoc loc msg
  where
    msg = text "Unexpected documentation comment."

failOpStrictnessCompound :: Located SrcStrictness -> LHsType GhcPs -> P a
failOpStrictnessCompound (dL->L _ str) (dL->L loc ty) = parseErrorSDoc loc msg
  where
    msg = text "Strictness annotation applied to a compound type." $$
          text "Did you mean to add parentheses?" $$
          nest 2 (ppr str <> parens (ppr ty))

failOpStrictnessPosition :: Located SrcStrictness -> P a
failOpStrictnessPosition (dL->L loc _) = parseErrorSDoc loc msg
  where
    msg = text "Strictness annotation cannot appear in this position."

-----------------------------------------------------------------------------
-- Misc utils

parseErrorSDoc :: SrcSpan -> SDoc -> P a
parseErrorSDoc span s = failSpanMsgP span s

-- | Hint about bang patterns, assuming @BangPatterns@ is off.
hintBangPat :: SrcSpan -> HsExpr GhcPs -> P ()
hintBangPat span e = do
    bang_on <- getBit BangPatBit
    unless bang_on $
      parseErrorSDoc span
        (text "Illegal bang-pattern (use BangPatterns):" $$ ppr e)

data SumOrTuple
  = Sum ConTag Arity (LHsExpr GhcPs)
  | Tuple [LHsTupArg GhcPs]

mkSumOrTuple :: Boxity -> SrcSpan -> SumOrTuple -> P (HsExpr GhcPs)

-- Tuple
mkSumOrTuple boxity _ (Tuple es) = return (ExplicitTuple noExt es boxity)

-- Sum
mkSumOrTuple Unboxed _ (Sum alt arity e) =
    return (ExplicitSum noExt alt arity e)
mkSumOrTuple Boxed l (Sum alt arity (dL->L _ e)) =
    parseErrorSDoc l (hang (text "Boxed sums not supported:") 2
                      (ppr_boxed_sum alt arity e))
  where
    ppr_boxed_sum :: ConTag -> Arity -> HsExpr GhcPs -> SDoc
    ppr_boxed_sum alt arity e =
      text "(" <+> ppr_bars (alt - 1) <+> ppr e <+> ppr_bars (arity - alt)
      <+> text ")"

    ppr_bars n = hsep (replicate n (Outputable.char '|'))

mkLHsOpTy :: LHsType GhcPs -> Located RdrName -> LHsType GhcPs -> LHsType GhcPs
mkLHsOpTy x op y =
  let loc = getLoc x `combineSrcSpans` getLoc op `combineSrcSpans` getLoc y
  in cL loc (mkHsOpTy x op y)

mkLHsDocTy :: LHsType GhcPs -> LHsDocString -> LHsType GhcPs
mkLHsDocTy t doc =
  let loc = getLoc t `combineSrcSpans` getLoc doc
  in cL loc (HsDocTy noExt t doc)

mkLHsDocTyMaybe :: LHsType GhcPs -> Maybe LHsDocString -> LHsType GhcPs
mkLHsDocTyMaybe t = maybe t (mkLHsDocTy t)

-----------------------------------------------------------------------------
-- Token symbols

starSym :: Bool -> String
starSym True = "★"
starSym False = "*"

forallSym :: Bool -> String
forallSym True = "∀"
forallSym False = "forall"
