{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Haddock.Backends.Xhtml.Decl
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
module Haddock.Backends.Xhtml.Decl
  ( ppDecl
  , ppOrphanInstances
  ) where

import Data.Foldable (toList)
import Data.List (intersperse, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import GHC hiding (HsTypeGhcPsExt (..), LexicalFixity (..), fromMaybeContext)
import GHC.Core.Type (Specificity (..))
import GHC.Data.BooleanFormula
import GHC.Exts hiding (toList)
import GHC.Types.Name
import GHC.Types.Name.Reader (rdrNameOcc)
import Text.XHtml hiding (name, p, quote, title)

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.Doc (combineDocumentation)
import Haddock.GhcUtils
import Haddock.Types

-- | Pretty print a declaration
ppDecl
  :: Bool
  -- ^ print summary info only
  -> LinksInfo
  -- ^ link information
  -> LHsDecl DocNameI
  -- ^ declaration to print
  -> [(HsDecl DocNameI, DocForDecl DocName)]
  -- ^ relevant pattern synonyms
  -> DocForDecl DocName
  -- ^ documentation for this decl
  -> [DocInstance DocNameI]
  -- ^ relevant instances
  -> [(DocName, Fixity)]
  -- ^ relevant fixities
  -> [(DocName, DocForDecl DocName)]
  -- ^ documentation for all decls
  -> Splice
  -> Unicode
  -- ^ unicode output
  -> Maybe Package
  -> Qualification
  -> Html
ppDecl summ links (L loc decl) pats (mbDoc, fnArgsDoc) instances fixities subdocs splice unicode pkg qual = case decl of
  TyClD _ (FamDecl _ d) -> ppFamDecl summ False links instances fixities (locA loc) mbDoc d splice unicode pkg qual
  TyClD _ d@(DataDecl{}) -> ppDataDecl summ links instances fixities subdocs (locA loc) mbDoc d pats splice unicode pkg qual
  TyClD _ d@(SynDecl{}) -> ppTySyn summ links fixities (locA loc) (mbDoc, fnArgsDoc) d splice unicode pkg qual
  TyClD _ d@(ClassDecl{}) -> ppClassDecl summ links instances fixities (locA loc) mbDoc subdocs d splice unicode pkg qual
  SigD _ (TypeSig _ lnames lty) ->
    ppLFunSig
      summ
      links
      (locA loc)
      (mbDoc, fnArgsDoc)
      lnames
      (dropWildCardsI lty)
      fixities
      splice
      unicode
      pkg
      qual
  SigD _ (PatSynSig _ lnames lty) ->
    ppLPatSig
      summ
      links
      (locA loc)
      (mbDoc, fnArgsDoc)
      lnames
      lty
      fixities
      splice
      unicode
      pkg
      qual
  ForD _ d -> ppFor summ links (locA loc) (mbDoc, fnArgsDoc) d fixities splice unicode pkg qual
  InstD _ _ -> noHtml
  DerivD _ _ -> noHtml
  _ -> error "declaration not supported by ppDecl"

ppLFunSig
  :: Bool
  -> LinksInfo
  -> SrcSpan
  -> DocForDecl DocName
  -> [LocatedN DocName]
  -> LHsSigType DocNameI
  -> [(DocName, Fixity)]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppLFunSig summary links loc doc lnames lty fixities splice unicode pkg qual =
  ppFunSig
    summary
    links
    loc
    noHtml
    doc
    (map unLoc lnames)
    lty
    fixities
    splice
    unicode
    pkg
    qual

ppFunSig
  :: Bool
  -> LinksInfo
  -> SrcSpan
  -> Html
  -> DocForDecl DocName
  -> [DocName]
  -> LHsSigType DocNameI
  -> [(DocName, Fixity)]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppFunSig summary links loc leader doc docnames typ fixities splice unicode pkg qual =
  ppSigLike
    summary
    links
    loc
    leader
    doc
    docnames
    fixities
    (unLoc typ, pp_typ)
    splice
    unicode
    pkg
    qual
    HideEmptyContexts
  where
    pp_typ = ppLSigType unicode qual HideEmptyContexts typ

-- | Pretty print a pattern synonym
ppLPatSig
  :: Bool
  -> LinksInfo
  -> SrcSpan
  -> DocForDecl DocName
  -> [LocatedN DocName]
  -- ^ names of patterns in declaration
  -> LHsSigType DocNameI
  -- ^ type of patterns in declaration
  -> [(DocName, Fixity)]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppLPatSig summary links loc doc lnames typ fixities splice unicode pkg qual =
  ppSigLike
    summary
    links
    loc
    (keyword "pattern")
    doc
    (map unLoc lnames)
    fixities
    (unLoc typ, pp_typ)
    splice
    unicode
    pkg
    qual
    (patSigContext typ)
  where
    pp_typ = ppPatSigType unicode qual typ

ppSigLike
  :: Bool
  -> LinksInfo
  -> SrcSpan
  -> Html
  -> DocForDecl DocName
  -> [DocName]
  -> [(DocName, Fixity)]
  -> (HsSigType DocNameI, Html)
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> HideEmptyContexts
  -> Html
ppSigLike
  summary
  links
  loc
  leader
  doc
  docnames
  fixities
  (typ, pp_typ)
  splice
  unicode
  pkg
  qual
  emptyCtxts =
    ppTypeOrFunSig
      summary
      links
      loc
      docnames
      typ
      doc
      ( addFixities $ leader <+> ppTypeSig summary occnames pp_typ unicode
      , (leader <+>) . addFixities . concatHtml . punctuate comma $ map (ppBinder False) occnames
      , dcolon unicode
      )
      splice
      unicode
      pkg
      qual
      emptyCtxts
    where
      occnames = map (nameOccName . getName) docnames
      addFixities html
        | summary = html
        | otherwise = html <+> ppFixities fixities qual

ppTypeOrFunSig
  :: Bool
  -> LinksInfo
  -> SrcSpan
  -> [DocName]
  -> HsSigType DocNameI
  -> DocForDecl DocName
  -> (Html, Html, Html)
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> HideEmptyContexts
  -> Html
ppTypeOrFunSig
  summary
  links
  loc
  docnames
  typ
  (doc, argDocs)
  (pref1, pref2, sep)
  splice
  unicode
  pkg
  qual
  emptyCtxts
    | summary = pref1
    | Map.null argDocs = topDeclElem links loc splice docName pref1 +++ docSection curname pkg qual doc
    | otherwise =
        topDeclElem links loc splice docName pref2
          +++ subArguments pkg qual (ppSubSigLike unicode qual typ argDocs [] sep emptyCtxts)
          +++ docSection curname pkg qual doc
    where
      curname = getName <$> Maybe.listToMaybe docnames
      docName =
        case Maybe.listToMaybe docnames of
          Nothing -> error "No docnames. An invariant was broken. Please report this to the Haddock project"
          Just hd -> hd

-- | This splits up a type signature along @->@ and adds docs (when they exist)
-- to the arguments.
--
-- If one passes in a list of the available subdocs, any top-level `HsRecTy`
-- found will be expanded out into their fields.
ppSubSigLike
  :: Unicode
  -> Qualification
  -> HsSigType DocNameI
  -- ^ type signature
  -> FnArgsDoc DocName
  -- ^ docs to add
  -> [(DocName, DocForDecl DocName)]
  -- ^ all subdocs (useful when
  -- we expand an `HsRecTy`)
  -> Html
  -> HideEmptyContexts
  -> [SubDecl]
ppSubSigLike unicode qual typ argDocs subdocs sep emptyCtxts = do_sig_args 0 sep typ
  where
    do_sig_args :: Int -> Html -> HsSigType DocNameI -> [SubDecl]
    do_sig_args n leader (HsSig{sig_bndrs = outer_bndrs, sig_body = ltype}) =
      case outer_bndrs of
        HsOuterExplicit{hso_bndrs = bndrs} -> do_largs n (leader' bndrs) ltype
        HsOuterImplicit{} -> do_largs n leader ltype
      where
        leader' bndrs = leader <+> ppForAllPart unicode qual (mkHsForAllInvisTeleI bndrs)

    argDoc n = Map.lookup n argDocs

    do_largs :: Int -> Html -> LHsType DocNameI -> [SubDecl]
    do_largs n leader (L _ t) = do_args n leader t

    do_args :: Int -> Html -> HsType DocNameI -> [SubDecl]
    do_args n leader (HsForAllTy _ tele ltype) =
      do_largs n leader' ltype
      where
        leader' = leader <+> ppForAllPart unicode qual tele
    do_args n leader (HsQualTy _ lctxt ltype)
      | null (unLoc lctxt) =
          do_largs n leader ltype
      | otherwise =
          (leader <+> ppLContextNoArrow lctxt unicode qual emptyCtxts, Nothing, [])
            : do_largs n (darrow unicode) ltype
    do_args n leader (HsFunTy _ _w (L _ (XHsType (HsRecTy fields))) r) =
      [ (ldr <+> html, mdoc, subs)
      | (L _ field, ldr) <- zip fields (leader <+> gadtOpen : repeat gadtComma)
      , let (html, mdoc, subs) = ppSideBySideField subdocs unicode qual field
      ]
        ++ do_largs (n + 1) (gadtEnd <+> arrow unicode) r
    do_args n leader (HsFunTy _ _w lt r) =
      (leader <+> ppLFunLhType unicode qual emptyCtxts lt, argDoc n, [])
        : do_largs (n + 1) (arrow unicode) r
    do_args n leader t =
      [(leader <+> ppType unicode qual emptyCtxts t, argDoc n, [])]

    -- FIXME: this should be done more elegantly
    --
    -- We need 'gadtComma' and 'gadtEnd' to line up with the `{` from
    -- 'gadtOpen', so we add 3 spaces to cover for `-> `/`:: ` (3 in unicode
    -- mode since `->` and `::` are rendered as single characters.
    gadtComma = concatHtml (replicate (if unicode then 2 else 3) spaceHtml) <> toHtml ","
    gadtEnd = concatHtml (replicate (if unicode then 2 else 3) spaceHtml) <> toHtml "}"
    gadtOpen = toHtml "{"

ppFixities :: [(DocName, Fixity)] -> Qualification -> Html
ppFixities [] _ = noHtml
ppFixities fs qual = foldr1 (+++) (map ppFix uniq_fs) +++ rightEdge
  where
    ppFix (ns, p, d) =
      thespan
        ! [theclass "fixity"]
        << (toHtml d <+> toHtml (show p) <+> ppNames ns)

    ppDir InfixR = "infixr"
    ppDir InfixL = "infixl"
    ppDir InfixN = "infix"

    ppNames = case fs of
      _ : [] -> const noHtml -- Don't display names for fixities on single names
      _ -> concatHtml . intersperse (stringToHtml ", ") . map (ppDocName qual Infix False)

    uniq_fs =
      [ (n, the p, the d')
      | (n, Fixity p d) <- fs
      , let d' = ppDir d
      , then group by
          Down (p, d')
        using
          groupWith
      ]

    rightEdge = thespan ! [theclass "rightedge"] << noHtml

-- | Pretty-print type variables.
ppTyVars
  :: RenderableBndrFlag flag
  => Unicode
  -> Qualification
  -> [LHsTyVarBndr flag DocNameI]
  -> [Html]
ppTyVars unicode qual tvs = map (ppHsTyVarBndr unicode qual . unLoc) tvs

ppFor
  :: Bool
  -> LinksInfo
  -> SrcSpan
  -> DocForDecl DocName
  -> ForeignDecl DocNameI
  -> [(DocName, Fixity)]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppFor
  summary
  links
  loc
  doc
  (ForeignImport _ (L _ name) typ _)
  fixities
  splice
  unicode
  pkg
  qual =
    ppFunSig summary links loc noHtml doc [name] typ fixities splice unicode pkg qual
ppFor _ _ _ _ _ _ _ _ _ _ = error "ppFor"

-- we skip type patterns for now
ppTySyn
  :: Bool
  -> LinksInfo
  -> [(DocName, Fixity)]
  -> SrcSpan
  -> DocForDecl DocName
  -> TyClDecl DocNameI
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppTySyn
  summary
  links
  fixities
  loc
  doc
  ( SynDecl
      { tcdLName = L _ name
      , tcdTyVars = ltyvars
      , tcdRhs = ltype
      }
    )
  splice
  unicode
  pkg
  qual =
    ppTypeOrFunSig
      summary
      links
      loc
      [name]
      sig_type
      doc
      (full <+> fixs, hdr <+> fixs, spaceHtml +++ equals)
      splice
      unicode
      pkg
      qual
      ShowEmptyToplevelContexts
    where
      sig_type = mkHsImplicitSigTypeI ltype
      hdr =
        hsep
          ( [keyword "type", ppBinder summary occ]
              ++ ppTyVars unicode qual (hsQTvExplicit ltyvars)
          )
      full = hdr <+> def
      def = case unLoc ltype of
        XHsType (HsRedacted k) ->
          dcolon unicode <+> ppType unicode qual HideEmptyContexts k
        _ -> equals <+> ppPatSigType unicode qual (noLocA sig_type)
      occ = nameOccName . getName $ name
      fixs
        | summary = noHtml
        | otherwise = ppFixities fixities qual
ppTySyn _ _ _ _ _ _ _ _ _ _ = error "declaration not supported by ppTySyn"

ppTypeSig :: Bool -> [OccName] -> Html -> Unicode -> Html
ppTypeSig summary nms pp_ty unicode =
  concatHtml htmlNames <+> dcolon unicode <+> pp_ty
  where
    htmlNames = intersperse (stringToHtml ", ") $ map (ppBinder summary) nms

ppSimpleSig
  :: LinksInfo
  -> Splice
  -> Unicode
  -> Qualification
  -> HideEmptyContexts
  -> SrcSpan
  -> [DocName]
  -> HsSigType DocNameI
  -> Html
ppSimpleSig links splice unicode qual emptyCtxts loc names typ =
  topDeclElem' docName $ ppTypeSig True occNames ppTyp unicode
  where
    topDeclElem' = topDeclElem links loc splice
    ppTyp = ppSigType unicode qual emptyCtxts typ
    occNames = map getOccName names
    docName =
      case Maybe.listToMaybe names of
        Nothing -> error "No names. An invariant was broken. Please report this to the Haddock project"
        Just hd -> hd

--------------------------------------------------------------------------------

-- * Type families

--------------------------------------------------------------------------------

-- | Print a data\/type family declaration
ppFamDecl
  :: Bool
  -- ^ is a summary
  -> Bool
  -- ^ is an associated type
  -> LinksInfo
  -> [DocInstance DocNameI]
  -- ^ relevant instances
  -> [(DocName, Fixity)]
  -- ^ relevant fixities
  -> SrcSpan
  -> Documentation DocName
  -- ^ this decl's documentation
  -> FamilyDecl DocNameI
  -- ^ this decl
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppFamDecl summary associated links instances fixities loc doc decl splice unicode pkg qual
  | summary = ppFamHeader True associated decl unicode qual
  | otherwise = header_ +++ docSection curname pkg qual doc +++ instancesBit
  where
    docname = unLoc $ fdLName decl
    curname = Just $ getName docname

    header_ =
      topDeclElem links loc splice docname $
        ppFamHeader summary associated decl unicode qual <+> ppFixities fixities qual

    instancesBit
      | FamilyDecl{fdInfo = ClosedTypeFamily mb_eqns} <- decl
      , not summary =
          subEquations pkg qual $ map (ppFamDeclEqn . unLoc) $ Maybe.fromMaybe [] mb_eqns
      | otherwise =
          ppInstances links (OriginFamily docname) instances splice unicode pkg qual

    -- Individual equation of a closed type family
    ppFamDeclEqn :: TyFamInstEqn DocNameI -> SubDecl
    ppFamDeclEqn
      ( FamEqn
          { feqn_tycon = L _ n
          , feqn_rhs = rhs
          , feqn_pats = ts
          }
        ) =
        ( ppAppNameTypeArgs n ts unicode qual
            <+> equals
            <+> ppType unicode qual HideEmptyContexts (unLoc rhs)
        , Nothing
        , []
        )

-- | Print the LHS of a type\/data family declaration
ppFamHeader
  :: Bool
  -- ^ is a summary
  -> Bool
  -- ^ is an associated type
  -> FamilyDecl DocNameI
  -- ^ family declaration
  -> Unicode
  -> Qualification
  -> Html
ppFamHeader
  summary
  associated
  ( FamilyDecl
      { fdInfo = info
      , fdResultSig = L _ result
      , fdInjectivityAnn = injectivity
      , fdLName = L _ name
      , fdTyVars = tvs
      }
    )
  unicode
  qual =
    hsep
      [ ppFamilyLeader associated info
      , ppAppDocNameTyVarBndrs summary unicode qual name (hsq_explicit tvs)
      , ppResultSig result unicode qual
      , injAnn
      , whereBit
      ]
    where
      whereBit = case info of
        ClosedTypeFamily _ -> keyword "where ..."
        _ -> noHtml

      injAnn = case injectivity of
        Nothing -> noHtml
        Just (L _ (InjectivityAnn _ lhs rhs)) ->
          hsep
            ( keyword "|"
                : ppLDocName qual Raw lhs
                : arrow unicode
                : map (ppLDocName qual Raw) rhs
            )
        Just _ -> error "ppFamHeader:XInjectivityAnn"

-- | Print the keywords that begin the family declaration
ppFamilyLeader :: Bool -> FamilyInfo DocNameI -> Html
ppFamilyLeader assoc info = keyword (typ ++ if assoc then "" else " family")
  where
    typ = case info of
      OpenTypeFamily -> "type"
      ClosedTypeFamily _ -> "type"
      DataFamily -> "data"

-- | Print the signature attached to a family
ppResultSig :: FamilyResultSig DocNameI -> Unicode -> Qualification -> Html
ppResultSig result unicode qual = case result of
  NoSig _ -> noHtml
  KindSig _ kind -> dcolon unicode <+> ppLKind unicode qual kind
  TyVarSig _ (L _ bndr) -> equals <+> ppHsTyVarBndr unicode qual bndr

--------------------------------------------------------------------------------

-- * Associated Types

--------------------------------------------------------------------------------

ppAssocType
  :: Bool
  -> LinksInfo
  -> DocForDecl DocName
  -> LFamilyDecl DocNameI
  -> [(DocName, Fixity)]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppAssocType summ links doc (L loc decl) fixities splice unicode pkg qual =
  ppFamDecl summ True links [] fixities loc (fst doc) decl splice unicode pkg qual

--------------------------------------------------------------------------------

-- * Type applications

--------------------------------------------------------------------------------

ppAppDocNameTyVarBndrs
  :: RenderableBndrFlag flag
  => Bool
  -> Unicode
  -> Qualification
  -> DocName
  -> [LHsTyVarBndr flag DocNameI]
  -> Html
ppAppDocNameTyVarBndrs summ unicode qual n vs =
  ppTypeApp n vs ppDN (ppHsTyVarBndr unicode qual . unLoc)
  where
    ppDN notation = ppBinderFixity notation summ . nameOccName . getName
    ppBinderFixity Infix = ppBinderInfix
    ppBinderFixity _ = ppBinder

-- | Print an application of a 'DocName' to its list of 'HsType's
ppAppNameTypes :: DocName -> [HsType DocNameI] -> Unicode -> Qualification -> Html
ppAppNameTypes n ts unicode qual =
  ppTypeApp n ts (\p -> ppDocName qual p True) (ppParendType unicode qual HideEmptyContexts)

ppAppNameTypeArgs :: DocName -> [LHsTypeArg DocNameI] -> Unicode -> Qualification -> Html
ppAppNameTypeArgs n args@(HsValArg _ _ : HsValArg _ _ : _) u q =
  ppTypeApp n args (\p -> ppDocName q p True) (ppLHsTypeArg u q HideEmptyContexts)
ppAppNameTypeArgs n args u q =
  (ppDocName q Prefix True n) <+> hsep (map (ppLHsTypeArg u q HideEmptyContexts) args)

-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (Notation -> DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n (t1 : t2 : rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN Infix n <+> ppT t2
ppTypeApp n ts ppDN ppT = ppDN Prefix n <+> hsep (map ppT ts)

-------------------------------------------------------------------------------

-- * Contexts

-------------------------------------------------------------------------------

ppLContext
  :: Maybe (LHsContext DocNameI)
  -> Unicode
  -> Qualification
  -> HideEmptyContexts
  -> Html
ppLContext Nothing u q h = ppContext [] u q h
ppLContext (Just c) u q h = ppContext (unLoc c) u q h

ppLContextNoArrow
  :: LHsContext DocNameI
  -> Unicode
  -> Qualification
  -> HideEmptyContexts
  -> Html
ppLContextNoArrow c u q h = ppContextNoArrow (unLoc c) u q h

ppContextNoArrow :: HsContext DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppContextNoArrow cxt unicode qual emptyCtxts =
  Maybe.fromMaybe noHtml $
    ppContextNoLocsMaybe (map unLoc cxt) unicode qual emptyCtxts

ppContextNoLocs :: [HsType DocNameI] -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppContextNoLocs cxt unicode qual emptyCtxts =
  maybe noHtml (<+> darrow unicode) $
    ppContextNoLocsMaybe cxt unicode qual emptyCtxts

ppContextNoLocsMaybe :: [HsType DocNameI] -> Unicode -> Qualification -> HideEmptyContexts -> Maybe Html
ppContextNoLocsMaybe [] _ _ emptyCtxts =
  case emptyCtxts of
    HideEmptyContexts -> Nothing
    ShowEmptyToplevelContexts -> Just (toHtml "()")
ppContextNoLocsMaybe cxt unicode qual _ = Just $ ppHsContext cxt unicode qual

ppContext :: HsContext DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppContext cxt unicode qual emptyCtxts = ppContextNoLocs (map unLoc cxt) unicode qual emptyCtxts

ppHsContext :: [HsType DocNameI] -> Unicode -> Qualification -> Html
ppHsContext [] _ _ = noHtml
ppHsContext [p] unicode qual = ppCtxType unicode qual p
ppHsContext cxt unicode qual = parenBreakableList (map (ppType unicode qual HideEmptyContexts) cxt)

-------------------------------------------------------------------------------

-- * Class declarations

-------------------------------------------------------------------------------

ppClassHdr
  :: Bool
  -> Maybe (LocatedC [LHsType DocNameI])
  -> DocName
  -> LHsQTyVars DocNameI
  -> [LHsFunDep DocNameI]
  -> Unicode
  -> Qualification
  -> Html
ppClassHdr summ lctxt n tvs fds unicode qual =
  keyword "class"
    <+> (if not (null $ fromMaybeContext lctxt) then ppLContext lctxt unicode qual HideEmptyContexts else noHtml)
    <+> ppAppDocNameTyVarBndrs summ unicode qual n (hsQTvExplicit tvs)
    <+> ppFds fds unicode qual

ppFds :: [LHsFunDep DocNameI] -> Unicode -> Qualification -> Html
ppFds fds unicode qual =
  if null fds
    then noHtml
    else char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
    fundep (FunDep _ vars1 vars2) = ppVars vars1 <+> arrow unicode <+> ppVars vars2
    fundep (XFunDep _) = error "ppFds"
    ppVars = hsep . map ((ppDocName qual Prefix True) . unLoc)

ppShortClassDecl
  :: Bool
  -> LinksInfo
  -> TyClDecl DocNameI
  -> SrcSpan
  -> [(DocName, DocForDecl DocName)]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppShortClassDecl
  summary
  links
  ( ClassDecl
      { tcdCtxt = lctxt
      , tcdLName = lname
      , tcdTyVars = tvs
      , tcdFDs = fds
      , tcdSigs = sigs
      , tcdATs = ats
      }
    )
  loc
  subdocs
  splice
  unicode
  pkg
  qual =
    if not (any isUserLSig sigs) && null ats
      then (if summary then id else topDeclElem links loc splice nm) hdr
      else
        (if summary then id else topDeclElem links loc splice nm) (hdr <+> keyword "where")
          +++ shortSubDecls
            False
            ( [ ppAssocType summary links doc at [] splice unicode pkg qual | at <- ats, let doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs
              ]
                ++
                -- ToDo: add associated type defaults

                [ ppFunSig
                  summary
                  links
                  loc
                  noHtml
                  doc
                  names
                  typ
                  []
                  splice
                  unicode
                  pkg
                  qual
                | L _ (ClassOpSig _ False lnames typ) <- sigs
                , let names = map unLoc lnames
                      subdocName =
                        case Maybe.listToMaybe names of
                          Nothing -> error "No names. An invariant was broken. Please report this to the Haddock project"
                          Just hd -> hd
                      doc = lookupAnySubdoc subdocName subdocs
                ]
                -- FIXME: is taking just the first name ok? Is it possible that
                -- there are different subdocs for different names in a single
                -- type signature?
            )
    where
      hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds unicode qual
      nm = unLoc lname
ppShortClassDecl _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"

ppClassDecl
  :: Bool
  -> LinksInfo
  -> [DocInstance DocNameI]
  -> [(DocName, Fixity)]
  -> SrcSpan
  -> Documentation DocName
  -> [(DocName, DocForDecl DocName)]
  -> TyClDecl DocNameI
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppClassDecl
  summary
  links
  instances
  fixities
  loc
  d
  subdocs
  decl@( ClassDecl
          { tcdCtxt = lctxt
          , tcdLName = lname
          , tcdTyVars = ltyvars
          , tcdFDs = lfds
          , tcdSigs = lsigs
          , tcdATs = ats
          , tcdATDefs = atsDefs
          }
        )
  splice
  unicode
  pkg
  qual
    | summary = ppShortClassDecl summary links decl loc subdocs splice unicode pkg qual
    | otherwise =
        classheader
          +++ docSection curname pkg qual d
          +++ minimalBit
          +++ atBit
          +++ methodBit
          +++ instancesBit
    where
      curname = Just $ getName nm

      sigs = map unLoc lsigs

      classheader
        | any isUserLSig lsigs = topDeclElem links loc splice nm (hdr unicode qual <+> keyword "where" <+> fixs)
        | otherwise = topDeclElem links loc splice nm (hdr unicode qual <+> fixs)

      -- Only the fixity relevant to the class header
      fixs = ppFixities [f | f@(n, _) <- fixities, n == unLoc lname] qual

      nm = tcdNameI decl

      hdr = ppClassHdr summary lctxt (unLoc lname) ltyvars lfds

      -- Associated types
      atBit =
        subAssociatedTypes
          [ ppAssocType summary links doc at subfixs splice unicode pkg qual
            <+> subDefaults (Maybe.maybeToList defTys)
          | at <- ats
          , let name = unLoc . fdLName $ unLoc at
                doc = lookupAnySubdoc name subdocs
                subfixs = filter ((== name) . fst) fixities
                defTys = (declElem . ppDefaultAssocTy name) <$> lookupDAT name
          ]

      -- Default associated types
      ppDefaultAssocTy n (vs, rhs) =
        hsep
          [ keyword "type"
          , ppAppNameTypeArgs n vs unicode qual
          , equals
          , ppType unicode qual HideEmptyContexts (unLoc rhs)
          ]

      lookupDAT name = Map.lookup (getName name) defaultAssocTys
      defaultAssocTys =
        Map.fromList
          [ (getName name, (vs, typ))
          | L
              _
              ( TyFamInstDecl
                  _
                  ( FamEqn
                      { feqn_rhs = typ
                      , feqn_tycon = L _ name
                      , feqn_pats = vs
                      }
                    )
                ) <-
              atsDefs
          ]

      -- Methods
      methodBit =
        subMethods
          [ ppFunSig
            summary
            links
            loc
            noHtml
            doc
            [name]
            typ
            subfixs
            splice
            unicode
            pkg
            qual
            <+> subDefaults (Maybe.maybeToList defSigs)
          | ClassOpSig _ False lnames typ <- sigs
          , name <- map unLoc lnames
          , let doc = lookupAnySubdoc name subdocs
                subfixs = filter ((== name) . fst) fixities
                defSigs = ppDefaultFunSig name <$> lookupDM name
          ]
      -- N.B. taking just the first name is ok. Signatures with multiple names
      -- are expanded so that each name gets its own signature.

      -- Default methods
      ppDefaultFunSig n (t, d') =
        ppFunSig
          summary
          links
          loc
          (keyword "default")
          d'
          [n]
          t
          []
          splice
          unicode
          pkg
          qual

      lookupDM name = Map.lookup (occNameString $ mkDefaultMethodOcc $ getOccName name) defaultMethods
      defaultMethods =
        Map.fromList
          [ (nameStr, (typ, doc))
          | ClassOpSig _ True lnames typ <- sigs
          , name <- map unLoc lnames
          , let doc = lookupAnySubdoc name subdocs
                nameStr = getOccString name
          ]

      -- Minimal complete definition
      minimalBit = case [s | MinimalSig _ (L _ s) <- sigs] of
        -- Miminal complete definition = every shown method
        And xs : _
          | sort [getName n | L _ (Var (L _ n)) <- xs]
              == sort [getName n | ClassOpSig _ _ ns _ <- sigs, L _ n <- ns] ->
              noHtml
        -- Minimal complete definition = the only shown method
        Var (L _ n) : _
          | [getName n]
              == [getName n' | ClassOpSig _ _ ns _ <- sigs, L _ n' <- ns] ->
              noHtml
        -- Minimal complete definition = nothing
        And [] : _ -> subMinimal $ toHtml "Nothing"
        m : _ -> subMinimal $ ppMinimal False m
        _ -> noHtml

      ppMinimal _ (Var (L _ n)) = ppDocName qual Prefix True n
      ppMinimal _ (And fs) = foldr1 (\a b -> a +++ ", " +++ b) $ map (ppMinimal True . unLoc) fs
      ppMinimal p (Or fs) = wrap $ foldr1 (\a b -> a +++ " | " +++ b) $ map (ppMinimal False . unLoc) fs
        where
          wrap | p = parens | otherwise = id
      ppMinimal p (Parens x) = ppMinimal p (unLoc x)

      -- Instances
      instancesBit =
        ppInstances
          links
          (OriginClass nm)
          instances
          splice
          unicode
          pkg
          qual
ppClassDecl _ _ _ _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"

ppInstances
  :: LinksInfo
  -> InstOrigin DocName
  -> [DocInstance DocNameI]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppInstances links origin instances splice unicode pkg qual =
  subInstances pkg qual instName links True (zipWith instDecl [1 ..] instances)
  where
    -- force Splice = True to use line URLs

    instName = getOccString origin
    instDecl :: Int -> DocInstance DocNameI -> (SubDecl, Maybe Module, Located DocName)
    instDecl no (inst, mdoc, loc, mdl) =
      ((ppInstHead links splice unicode qual mdoc origin False no inst mdl), mdl, loc)

ppOrphanInstances
  :: LinksInfo
  -> [DocInstance DocNameI]
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppOrphanInstances links instances splice unicode pkg qual =
  subOrphanInstances pkg qual links True (zipWith instDecl [1 ..] instances)
  where
    instOrigin :: InstHead name -> InstOrigin (IdP name)
    instOrigin inst = OriginClass (ihdClsName inst)

    instDecl :: Int -> DocInstance DocNameI -> (SubDecl, Maybe Module, Located DocName)
    instDecl no (inst, mdoc, loc, mdl) =
      ((ppInstHead links splice unicode qual mdoc (instOrigin inst) True no inst Nothing), mdl, loc)

ppInstHead
  :: LinksInfo
  -> Splice
  -> Unicode
  -> Qualification
  -> Maybe (MDoc DocName)
  -> InstOrigin DocName
  -> Bool
  -- ^ Is instance orphan
  -> Int
  -- ^ Normal
  -> InstHead DocNameI
  -> Maybe Module
  -> SubDecl
ppInstHead links splice unicode qual mdoc origin orphan no ihd@(InstHead{..}) mdl =
  case ihdInstType of
    ClassInst{..} ->
      ( subInstHead iid $ ppContextNoLocs clsiCtx unicode qual HideEmptyContexts <+> typ
      , mdoc
      , [subInstDetails iid ats sigs mname]
      )
      where
        sigs = ppInstanceSigs links splice unicode qual clsiSigs
        ats = ppInstanceAssocTys links splice unicode qual orphan clsiAssocTys
    TypeInst rhs ->
      ( subInstHead iid ptype
      , mdoc
      , [subFamInstDetails iid prhs mname]
      )
      where
        ptype = keyword "type" <+> typ
        prhs =
          ptype
            <+> maybe
              noHtml
              (\t -> equals <+> ppType unicode qual HideEmptyContexts t)
              rhs
    DataInst dd@(DataDecl {}) ->
      ( subInstHead iid pdata
      , mdoc
      , [subFamInstDetails iid pdecl mname]
      )
      where
        cons = dd_cons (tcdDataDefn dd)
        pref = case cons of NewTypeCon _ -> keyword "newtype"; DataTypeCons _ _ -> keyword "data"
        pdata = pref <+> typ
        pdecl = pdata <+> ppShortDataDecl False True dd [] unicode qual
    DataInst {} -> error "ppInstHead"
  where
    mname = maybe noHtml (\m -> toHtml "Defined in" <+> ppModule m) mdl
    iid = instanceId origin no orphan ihd
    typ = ppAppNameTypes ihdClsName ihdTypes unicode qual

ppInstanceAssocTys
  :: LinksInfo
  -> Splice
  -> Unicode
  -> Qualification
  -> Bool
  -> [DocInstance DocNameI]
  -> [Html]
ppInstanceAssocTys links splice unicode qual orphan insts =
  Maybe.maybeToList $
    subTableSrc Nothing qual links True $
      zipWith
        mkInstHead
        insts
        [1 ..]
  where
    mkInstHead (inst, doc, name, mdl) no =
      ( ppInstHead links splice unicode qual doc (OriginFamily (unLoc name)) orphan no inst mdl
      , mdl
      , name
      )

ppInstanceSigs
  :: LinksInfo
  -> Splice
  -> Unicode
  -> Qualification
  -> [Sig DocNameI]
  -> [Html]
ppInstanceSigs links splice unicode qual sigs = do
  TypeSig _ lnames typ <- sigs
  let names = map unLoc lnames
      L _ rtyp = dropWildCardsI typ
  -- Instance methods signatures are synified and thus don't have a useful
  -- SrcSpan value. Use the methods name location instead.
  let lname =
        case Maybe.listToMaybe lnames of
          Nothing -> error "No names. An invariant was broken. Please report this to the Haddock project"
          Just hd -> hd
  return $ ppSimpleSig links splice unicode qual HideEmptyContexts (getLocA lname) names rtyp

lookupAnySubdoc :: Eq id1 => id1 -> [(id1, DocForDecl id2)] -> DocForDecl id2
lookupAnySubdoc n = Maybe.fromMaybe noDocForDecl . lookup n

instanceId :: InstOrigin DocName -> Int -> Bool -> InstHead DocNameI -> String
instanceId origin no orphan ihd =
  concat $
    ["o:" | orphan]
      ++ [ qual origin
         , ":" ++ getOccString origin
         , ":" ++ getOccString (ihdClsName ihd)
         , ":" ++ show no
         ]
  where
    qual (OriginClass _) = "ic"
    qual (OriginData _) = "id"
    qual (OriginFamily _) = "if"

-------------------------------------------------------------------------------

-- * Data & newtype declarations

-------------------------------------------------------------------------------

-- TODO: print contexts
ppShortDataDecl
  :: Bool
  -> Bool
  -> TyClDecl DocNameI
  -> [(HsDecl DocNameI, DocForDecl DocName)]
  -> Unicode
  -> Qualification
  -> Html
ppShortDataDecl summary dataInst dataDecl@(DataDecl {}) pats unicode qual
  | [] <- toList cons
  , [] <- pats =
      dataHeader
  | [lcon] <- toList cons
  , [] <- pats
  , isH98
  , (cHead, cBody, cFoot) <- ppShortConstrParts summary dataInst (unLoc lcon) unicode qual =
      (dataHeader <+> equals <+> cHead) +++ cBody +++ cFoot
  | [] <- pats
  , isH98 =
      dataHeader
        +++ shortSubDecls dataInst (zipWith doConstr ('=' : repeat '|') (toList cons) ++ pats1)
  | otherwise =
      (dataHeader <+> keyword "where")
        +++ shortSubDecls dataInst (map doGADTConstr (toList cons) ++ pats1)
  where
    dataHeader
      | dataInst = noHtml
      | otherwise = ppDataHeader summary dataDecl unicode qual
    doConstr c con = toHtml [c] <+> ppShortConstr summary (unLoc con) unicode qual
    doGADTConstr con = ppShortConstr summary (unLoc con) unicode qual

    cons = dd_cons (tcdDataDefn dataDecl)
    isH98 = flip any (unLoc <$> cons) $ \case
      ConDeclH98{} -> True
      ConDeclGADT{} -> False

    pats1 =
      [ hsep
        [ keyword "pattern"
        , hsep $ punctuate comma $ map (ppBinder summary . getOccName) lnames
        , dcolon unicode
        , ppPatSigType unicode qual typ
        ]
      | (SigD _ (PatSynSig _ lnames typ), _) <- pats
      ]
ppShortDataDecl _ _ _ _ _ _ = error "ppShortDataDecl"

-- | Pretty-print a data declaration
ppDataDecl
  :: Bool
  -> LinksInfo
  -> [DocInstance DocNameI]
  -- ^ relevant instances
  -> [(DocName, Fixity)]
  -- ^ relevant fixities
  -> [(DocName, DocForDecl DocName)]
  -- ^ all decl documentation
  -> SrcSpan
  -> Documentation DocName
  -- ^ this decl's documentation
  -> TyClDecl DocNameI
  -- ^ this decl; always a DataDecl
  -> [(HsDecl DocNameI, DocForDecl DocName)]
  -- ^ relevant patterns
  -> Splice
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> Html
ppDataDecl
  summary
  links
  instances
  fixities
  subdocs
  loc
  doc
  dataDecl@(DataDecl {})
  pats
  splice
  unicode
  pkg
  qual
    | summary = ppShortDataDecl summary False dataDecl pats unicode qual
    | otherwise = header_ +++ docSection curname pkg qual doc +++ constrBit +++ patternBit +++ instancesBit
    where
      docname = tcdNameI dataDecl
      curname = Just $ getName docname
      cons = dd_cons (tcdDataDefn dataDecl)
      isH98 = flip any (unLoc <$> cons) $ \case
        ConDeclH98{} -> True
        ConDeclGADT{} -> False

      header_ =
        topDeclElem links loc splice docname $
          ppDataHeader summary dataDecl unicode qual <+> whereBit <+> fix

      fix = ppFixities (filter (\(n, _) -> n == docname) fixities) qual

      whereBit
        | null cons
        , null pats =
            noHtml
        | isH98 = noHtml
        | otherwise = keyword "where"

      constrBit =
        subConstructors
          pkg
          qual
          [ ppSideBySideConstr subdocs subfixs unicode pkg qual c
          | c <- toList cons
          , let subfixs =
                  filter
                    ( \(n, _) ->
                        any
                          (\cn -> cn == n)
                          (unL <$> getConNamesI (unLoc c))
                    )
                    fixities
          ]

      patternBit =
        subPatterns
          pkg
          qual
          [ ppSideBySidePat subfixs unicode qual lnames typ d
          | (SigD _ (PatSynSig _ lnames typ), d) <- pats
          , let subfixs =
                  filter
                    ( \(n, _) ->
                        any
                          (\cn -> cn == n)
                          (map unLoc lnames)
                    )
                    fixities
          ]

      instancesBit =
        ppInstances
          links
          (OriginData docname)
          instances
          splice
          unicode
          pkg
          qual
ppDataDecl _ _ _ _ _ _ _ _ _ _ _ _ _ = error "ppDataDecl"

ppShortConstr :: Bool -> ConDecl DocNameI -> Unicode -> Qualification -> Html
ppShortConstr summary con unicode qual = cHead <+> cBody <+> cFoot
  where
    (cHead, cBody, cFoot) = ppShortConstrParts summary False con unicode qual

-- returns three pieces: header, body, footer so that header & footer can be
-- incorporated into the declaration
ppShortConstrParts :: Bool -> Bool -> ConDecl DocNameI -> Unicode -> Qualification -> (Html, Html, Html)
ppShortConstrParts summary dataInst con unicode qual =
  case con of
    ConDeclH98
      { con_args = det
      , con_ex_tvs = tyVars
      , con_forall = forall_
      , con_mb_cxt = cxt
      } ->
        let context = fromMaybeContext cxt
            header_ = ppConstrHdr forall_ tyVars context unicode qual
         in case det of
              -- Prefix constructor, e.g. 'Just a'
              PrefixCon args ->
                ( header_ <+> hsep (ppOcc : map (ppLParendType unicode qual HideEmptyContexts . hsConDeclFieldToHsTypeNoMult) args)
                , noHtml
                , noHtml
                )
              -- Record constructor, e.g. 'Identity { runIdentity :: a }'
              RecCon (L _ fields) ->
                ( header_ +++ ppOcc <+> char '{'
                , shortSubDecls
                    dataInst
                    [ ppShortField summary unicode qual field
                    | L _ field <- fields
                    ]
                , char '}'
                )
              -- Infix constructor, e.g. 'a :| [a]'
              InfixCon arg1 arg2 ->
                ( header_
                    <+> hsep
                      [ ppLParendType unicode qual HideEmptyContexts (hsConDeclFieldToHsTypeNoMult arg1)
                      , ppOccInfix
                      , ppLParendType unicode qual HideEmptyContexts (hsConDeclFieldToHsTypeNoMult arg2)
                      ]
                , noHtml
                , noHtml
                )
    -- GADT constructor, e.g. 'Foo :: Int -> Foo'
    ConDeclGADT{} ->
      ( hsep [ppOcc, dcolon unicode, ppLSigType unicode qual HideEmptyContexts (getGADTConType con)]
      , noHtml
      , noHtml
      )
  where
    occ = toList $ nameOccName . getName . unL <$> getConNamesI con
    ppOcc = hsep (punctuate comma (map (ppBinder summary) occ))
    ppOccInfix = hsep (punctuate comma (map (ppBinderInfix summary) occ))

-- | Pretty print an expanded constructor
ppSideBySideConstr
  :: [(DocName, DocForDecl DocName)]
  -> [(DocName, Fixity)]
  -> Unicode
  -> Maybe Package
  -> Qualification
  -> LConDecl DocNameI
  -- ^ constructor declaration to print
  -> SubDecl
ppSideBySideConstr subdocs fixities unicode pkg qual (L _ con) =
  ( decl -- Constructor header (name, fixity)
  , mbDoc -- Docs on the whole constructor
  , fieldPart -- Information on the fields (or arguments, if they have docs)
  )
  where
    -- Find the name of a constructors in the decl (`getConName` always returns a non-empty list)
    L _ aConName :| _ = getConNamesI con

    fixity = ppFixities fixities qual
    occ = toList $ nameOccName . getName . unL <$> getConNamesI con

    ppOcc = hsep (punctuate comma (map (ppBinder False) occ))
    ppOccInfix = hsep (punctuate comma (map (ppBinderInfix False) occ))

    -- Extract out the map of of docs corresponding to the constructors arguments
    argDocs = maybe Map.empty snd (lookup aConName subdocs)
    hasArgDocs = not $ Map.null argDocs

    decl = case con of
      ConDeclH98
        { con_args = det
        , con_ex_tvs = tyVars
        , con_forall = forall_
        , con_mb_cxt = cxt
        } ->
          let context = fromMaybeContext cxt
              header_ = ppConstrHdr forall_ tyVars context unicode qual
           in case det of
                -- Prefix constructor, e.g. 'Just a'
                PrefixCon args
                  | hasArgDocs -> header_ <+> ppOcc <+> fixity
                  | otherwise ->
                      hsep
                        [ header_ <+> ppOcc
                        , hsep (map (ppLParendType unicode qual HideEmptyContexts . hsConDeclFieldToHsTypeNoMult) args)
                        , fixity
                        ]
                -- Record constructor, e.g. 'Identity { runIdentity :: a }'
                RecCon _ -> header_ <+> ppOcc <+> fixity
                -- Infix constructor, e.g. 'a :| [a]'
                InfixCon arg1 arg2
                  | hasArgDocs -> header_ <+> ppOcc <+> fixity
                  | otherwise ->
                      hsep
                        [ header_ <+> ppLParendType unicode qual HideEmptyContexts (hsConDeclFieldToHsTypeNoMult arg1)
                        , ppOccInfix
                        , ppLParendType unicode qual HideEmptyContexts (hsConDeclFieldToHsTypeNoMult arg2)
                        , fixity
                        ]
      -- GADT constructor, e.g. 'Foo :: Int -> Foo'
      ConDeclGADT{}
        | hasArgDocs || not (null fieldPart) -> ppOcc <+> fixity
        | otherwise ->
            hsep
              [ ppOcc
              , dcolon unicode
              , -- ++AZ++ make this prepend "{..}" when it is a record style GADT
                ppLSigType unicode qual HideEmptyContexts (getGADTConType con)
              , fixity
              ]

    fieldPart = case con of
      ConDeclGADT{con_g_args = con_args'} -> case con_args' of
        -- GADT record declarations
        RecConGADT _ _ -> [doConstrArgsWithDocs []]
        -- GADT prefix data constructors
        PrefixConGADT _ args | hasArgDocs -> [doConstrArgsWithDocs args]
        _ -> []
      ConDeclH98{con_args = con_args'} -> case con_args' of
        -- H98 record declarations
        RecCon (L _ fields) -> [doRecordFields fields]
        -- H98 prefix data constructors
        PrefixCon args | hasArgDocs -> [doConstrArgsWithDocs args]
        -- H98 infix data constructor
        InfixCon arg1 arg2 | hasArgDocs -> [doConstrArgsWithDocs [arg1, arg2]]
        _ -> []

    doRecordFields fields =
      subFields
        pkg
        qual
        (map (ppSideBySideField subdocs unicode qual) (map unLoc fields))

    doConstrArgsWithDocs :: [HsConDeclField DocNameI] -> Html
    doConstrArgsWithDocs args = subFields pkg qual $ case con of
      ConDeclH98{} ->
        [ (ppLParendType unicode qual HideEmptyContexts arg, mdoc, [])
        | (i, arg) <- zip [0 ..] (map hsConDeclFieldToHsTypeNoMult args)
        , let mdoc = Map.lookup i argDocs
        ]
      ConDeclGADT{} ->
        ppSubSigLike
          unicode
          qual
          (unLoc (getGADTConType con))
          argDocs
          subdocs
          (dcolon unicode)
          HideEmptyContexts

    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc =
      lookup aConName subdocs
        >>= combineDocumentation . fst

-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr
  :: Bool
  -- ^ print explicit foralls
  -> [LHsTyVarBndr Specificity DocNameI]
  -- ^ type variables
  -> HsContext DocNameI
  -- ^ context
  -> Unicode
  -> Qualification
  -> Html
ppConstrHdr forall_ tvs ctxt unicode qual = ppForall +++ ppCtxt
  where
    ppForall
      | null tvs || not forall_ = noHtml
      | otherwise = ppForAllPart unicode qual (HsForAllInvis noExtField tvs)

    ppCtxt
      | null ctxt = noHtml
      | otherwise =
          ppContextNoArrow ctxt unicode qual HideEmptyContexts
            <+> darrow unicode
            +++ toHtml " "

-- | Pretty-print a record field
ppSideBySideField
  :: [(DocName, DocForDecl DocName)]
  -> Unicode
  -> Qualification
  -> HsConDeclRecField DocNameI
  -> SubDecl
ppSideBySideField subdocs unicode qual (HsConDeclRecField _ names ltype) =
  ( hsep
      ( punctuate
          comma
          [ ppBinder False (rdrNameOcc field)
          | L _ name <- names
          , let field = (foExt) name
          ]
      )
      <+> ppRecFieldMultAnn unicode qual ltype
      <+> dcolon unicode
      <+> ppLType unicode qual HideEmptyContexts (hsConDeclFieldToHsTypeNoMult ltype)
  , mbDoc
  , []
  )
  where
    mbDoc = lookup (unLoc . foLabel $ unLoc declName) subdocs >>= combineDocumentation . fst
    declName = case Maybe.listToMaybe names of
      Nothing -> error "No names. An invariant was broken. Please report this to the Haddock project"
      Just hd -> hd

-- don't use cdf_doc for same reason we don't use con_doc above
-- Where there is more than one name, they all have the same documentation
ppRecFieldMultAnn :: Unicode -> Qualification -> HsConDeclField DocNameI -> Html
ppRecFieldMultAnn unicode qual (CDF { cdf_multiplicity = ann }) = case ann of
  HsUnannotated _ -> noHtml
  HsLinearAnn _ -> toHtml "%1"
  HsExplicitMult _ mult -> multAnnotation <> ppr_mono_lty mult unicode qual HideEmptyContexts

ppShortField :: Bool -> Unicode -> Qualification -> HsConDeclRecField DocNameI -> Html
ppShortField summary unicode qual (HsConDeclRecField _ names ltype) =
  hsep (punctuate comma (map ((ppBinder summary) . rdrNameOcc . foExt . unLoc) names))
    <+> ppRecFieldMultAnn unicode qual ltype
    <+> dcolon unicode
    <+> ppLType unicode qual HideEmptyContexts (hsConDeclFieldToHsTypeNoMult ltype)

-- | Pretty print an expanded pattern (for bundled patterns)
ppSideBySidePat
  :: [(DocName, Fixity)]
  -> Unicode
  -> Qualification
  -> [LocatedN DocName]
  -- ^ pattern name(s)
  -> LHsSigType DocNameI
  -- ^ type of pattern(s)
  -> DocForDecl DocName
  -- ^ doc map
  -> SubDecl
ppSideBySidePat fixities unicode qual lnames typ (doc, argDocs) =
  ( decl
  , combineDocumentation doc
  , fieldPart
  )
  where
    hasArgDocs = not $ Map.null argDocs
    fixity = ppFixities fixities qual
    ppOcc = hsep (punctuate comma (map (ppBinder False . getOccName) lnames))

    decl
      | hasArgDocs = keyword "pattern" <+> ppOcc <+> fixity
      | otherwise =
          hsep
            [ keyword "pattern"
            , ppOcc
            , dcolon unicode
            , ppPatSigType unicode qual typ
            , fixity
            ]

    fieldPart
      | not hasArgDocs = []
      | otherwise =
          [ subFields
              Nothing
              qual
              ( ppSubSigLike
                  unicode
                  qual
                  (unLoc typ)
                  argDocs
                  []
                  (dcolon unicode)
                  emptyCtxt
              )
          ]

    emptyCtxt = patSigContext typ

-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocNameI -> Unicode -> Qualification -> Html
ppDataHeader
  summary
  ( DataDecl
      { tcdDataDefn =
        HsDataDefn
          { dd_cons = cons
          , dd_ctxt = ctxt
          , dd_kindSig = ks
          }
      , tcdLName = L _ name
      , tcdTyVars = tvs
      }
    )
  unicode
  qual =
    -- newtype or data
    ( case cons of
        NewTypeCon _ -> keyword "newtype"
        DataTypeCons False _ -> keyword "data"
        DataTypeCons True _ -> keyword "type" <+> keyword "data"
    )
      <+>
      -- context
      ppLContext ctxt unicode qual HideEmptyContexts
      <+>
      -- T a b c ..., or a :+: b
      ppAppDocNameTyVarBndrs summary unicode qual name (hsQTvExplicit tvs)
      <+> case ks of
        Nothing -> mempty
        Just (L _ x) -> dcolon unicode <+> ppKind unicode qual x
ppDataHeader _ _ _ _ = error "ppDataHeader: illegal argument"

--------------------------------------------------------------------------------

-- * Types and contexts

--------------------------------------------------------------------------------

ppBang :: HsSrcBang -> Html
ppBang (HsSrcBang _ _ SrcStrict) = toHtml "!"
ppBang (HsSrcBang _ _ SrcLazy) = toHtml "~"
ppBang _ = noHtml

tupleParens :: HsTupleSort -> [Html] -> Html
tupleParens HsUnboxedTuple = ubxParenList
tupleParens _ = parenList

sumParens :: [Html] -> Html
sumParens = ubxSumList

--------------------------------------------------------------------------------

-- * Rendering of HsType

--------------------------------------------------------------------------------

ppLType, ppLParendType, ppLFunLhType :: Unicode -> Qualification -> HideEmptyContexts -> LHsType DocNameI -> Html
ppLType unicode qual emptyCtxts y = ppType unicode qual emptyCtxts (unLoc y)
ppLParendType unicode qual emptyCtxts y = ppParendType unicode qual emptyCtxts (unLoc y)
ppLFunLhType unicode qual emptyCtxts y = ppFunLhType unicode qual emptyCtxts (unLoc y)

ppLSigType :: Unicode -> Qualification -> HideEmptyContexts -> LHsSigType DocNameI -> Html
ppLSigType unicode qual emptyCtxts y = ppSigType unicode qual emptyCtxts (unLoc y)

ppCtxType :: Unicode -> Qualification -> HsType DocNameI -> Html
ppCtxType unicode qual ty = ppr_mono_ty (reparenTypePrec PREC_CTX ty) unicode qual HideEmptyContexts

ppType, ppParendType, ppFunLhType :: Unicode -> Qualification -> HideEmptyContexts -> HsType DocNameI -> Html
ppType unicode qual emptyCtxts ty = ppr_mono_ty (reparenTypePrec PREC_TOP ty) unicode qual emptyCtxts
ppParendType unicode qual emptyCtxts ty = ppr_mono_ty (reparenTypePrec PREC_CON ty) unicode qual emptyCtxts
ppFunLhType unicode qual emptyCtxts ty = ppr_mono_ty (reparenTypePrec PREC_FUN ty) unicode qual emptyCtxts

ppSigType :: Unicode -> Qualification -> HideEmptyContexts -> HsSigType DocNameI -> Html
ppSigType unicode qual emptyCtxts sig_ty = ppr_sig_ty (reparenSigType sig_ty) unicode qual emptyCtxts

ppLHsTypeArg :: Unicode -> Qualification -> HideEmptyContexts -> LHsTypeArg DocNameI -> Html
ppLHsTypeArg unicode qual emptyCtxts (HsValArg _ ty) = ppLParendType unicode qual emptyCtxts ty
ppLHsTypeArg unicode qual emptyCtxts (HsTypeArg _ ki) = atSign <> ppLParendType unicode qual emptyCtxts ki
ppLHsTypeArg _ _ _ (HsArgPar _) = toHtml ""

class RenderableBndrFlag flag where
  ppHsTyVarBndr :: Unicode -> Qualification -> HsTyVarBndr flag DocNameI -> Html

instance RenderableBndrFlag () where
  ppHsTyVarBndr unicode qual (HsTvb _ _ bvar bkind) =
    decorate (pp_hs_tvb unicode qual bvar bkind)
    where decorate :: Html -> Html
          decorate d = parens_if_kind bkind d

instance RenderableBndrFlag Specificity where
  ppHsTyVarBndr unicode qual (HsTvb _ spec bvar bkind) =
    decorate (pp_hs_tvb unicode qual bvar bkind)
    where decorate :: Html -> Html
          decorate d = case spec of
            InferredSpec  -> braces d
            SpecifiedSpec -> parens_if_kind bkind d

instance RenderableBndrFlag (HsBndrVis DocNameI) where
  ppHsTyVarBndr unicode qual (HsTvb _ bvis bvar bkind) =
    decorate (pp_hs_tvb unicode qual bvar bkind)
    where decorate :: Html -> Html
          decorate d = case bvis of
            HsBndrRequired  _ -> parens_if_kind bkind d
            HsBndrInvisible _ -> atSign <> parens_if_kind bkind d

ppHsBndrVar :: Qualification -> HsBndrVar DocNameI -> Html
ppHsBndrVar qual (HsBndrVar _ name) = ppDocName qual Raw False (unLoc name)
ppHsBndrVar _    (HsBndrWildCard _) = char '_'

pp_hs_tvb :: Unicode -> Qualification -> HsBndrVar DocNameI -> HsBndrKind DocNameI -> Html
pp_hs_tvb _       qual bvar (HsBndrNoKind _) = ppHsBndrVar qual bvar
pp_hs_tvb unicode qual bvar (HsBndrKind _ k) =
  ppHsBndrVar qual bvar <+> dcolon unicode
                        <+> ppLKind unicode qual k

parens_if_kind :: HsBndrKind DocNameI -> Html -> Html
parens_if_kind (HsBndrNoKind _) d = d
parens_if_kind (HsBndrKind _ _) d = parens d

ppLKind :: Unicode -> Qualification -> LHsKind DocNameI -> Html
ppLKind unicode qual y = ppKind unicode qual (unLoc y)

ppKind :: Unicode -> Qualification -> HsKind DocNameI -> Html
ppKind unicode qual ki = ppr_mono_ty (reparenTypePrec PREC_TOP ki) unicode qual HideEmptyContexts

patSigContext :: LHsSigType DocNameI -> HideEmptyContexts
patSigContext sig_typ
  | hasNonEmptyContext typ && isFirstContextEmpty typ = ShowEmptyToplevelContexts
  | otherwise = HideEmptyContexts
  where
    typ = sig_body (unLoc sig_typ)

    hasNonEmptyContext t =
      case unLoc t of
        HsForAllTy _ _ s -> hasNonEmptyContext s
        HsQualTy _ cxt s -> if null (unLoc cxt) then hasNonEmptyContext s else True
        HsFunTy _ _ _ s -> hasNonEmptyContext s
        _ -> False
    isFirstContextEmpty t =
      case unLoc t of
        HsForAllTy _ _ s -> isFirstContextEmpty s
        HsQualTy _ cxt _ -> null (unLoc cxt)
        HsFunTy _ _ _ s -> isFirstContextEmpty s
        _ -> False

-- | Pretty-print a pattern signature (all this does over 'ppLType' is slot in
-- the right 'HideEmptyContext' value)
ppPatSigType :: Unicode -> Qualification -> LHsSigType DocNameI -> Html
ppPatSigType unicode qual typ =
  let emptyCtxts = patSigContext typ in ppLSigType unicode qual emptyCtxts typ

ppHsOuterTyVarBndrs
  :: RenderableBndrFlag flag
  => Unicode
  -> Qualification
  -> HsOuterTyVarBndrs flag DocNameI
  -> Html
ppHsOuterTyVarBndrs unicode qual outer_bndrs = case outer_bndrs of
  HsOuterImplicit{} -> noHtml
  HsOuterExplicit{hso_bndrs = bndrs} ->
    hsep (forallSymbol unicode : ppTyVars unicode qual bndrs) +++ dot

ppForAllPart :: Unicode -> Qualification -> HsForAllTelescope DocNameI -> Html
ppForAllPart unicode qual tele = case tele of
  HsForAllVis{hsf_vis_bndrs = bndrs} ->
    hsep (forallSymbol unicode : ppTyVars unicode qual bndrs)
      +++ spaceHtml
      +++ arrow unicode
  HsForAllInvis{hsf_invis_bndrs = bndrs} ->
    hsep (forallSymbol unicode : ppTyVars unicode qual bndrs) +++ dot

ppr_sig_ty :: HsSigType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_sig_ty (HsSig{sig_bndrs = outer_bndrs, sig_body = ltype}) unicode qual emptyCtxts =
  ppHsOuterTyVarBndrs unicode qual outer_bndrs <+> ppr_mono_lty ltype unicode qual emptyCtxts

ppr_mono_lty :: LHsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_mono_lty ty = ppr_mono_ty (unLoc ty)

ppr_mono_ty :: HsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_mono_ty (HsForAllTy _ tele ty) unicode qual emptyCtxts =
  ppForAllPart unicode qual tele <+> ppr_mono_lty ty unicode qual emptyCtxts
ppr_mono_ty (HsQualTy _ ctxt ty) unicode qual emptyCtxts =
  ppLContext (Just ctxt) unicode qual emptyCtxts <+> ppr_mono_lty ty unicode qual emptyCtxts
-- UnicodeSyntax alternatives
ppr_mono_ty (HsTyVar _ _ (L _ name)) True _ _
  | getOccString (getName name) == "(->)" = toHtml "(→)"
ppr_mono_ty (HsTyVar _ prom (L _ name)) _ q _
  | isPromoted prom = promoQuote (ppDocName q Prefix True name)
  | otherwise = ppDocName q Prefix True name
ppr_mono_ty (HsStarTy _ isUni) u _ _ =
  toHtml (if u || isUni then "★" else "*")
ppr_mono_ty (HsFunTy _ mult ty1 ty2) u q e =
  hsep
    [ ppr_mono_lty ty1 u q HideEmptyContexts
    , arr <+> ppr_mono_lty ty2 u q e
    ]
  where
    arr = case mult of
      HsLinearAnn _ -> lollipop u
      HsUnannotated _ -> arrow u
      HsExplicitMult _ m -> multAnnotation <> ppr_mono_lty m u q e <+> arrow u
ppr_mono_ty (HsTupleTy _ con tys) u q _ =
  tupleParens con (map (ppLType u q HideEmptyContexts) tys)
ppr_mono_ty (HsSumTy _ tys) u q _ =
  sumParens (map (ppLType u q HideEmptyContexts) tys)
ppr_mono_ty (HsKindSig _ ty kind) u q e =
  ppr_mono_lty ty u q e <+> dcolon u <+> ppLKind u q kind
ppr_mono_ty (HsListTy _ ty) u q _ = brackets (ppr_mono_lty ty u q HideEmptyContexts)
ppr_mono_ty (HsIParamTy _ (L _ n) ty) u q _ =
  ppIPName n <+> dcolon u <+> ppr_mono_lty ty u q HideEmptyContexts
ppr_mono_ty (HsSpliceTy v _) _ _ _ = dataConCantHappen v
ppr_mono_ty (XHsType (HsBangTy b ty)) u q _ =
  ppBang b +++ ppLParendType u q HideEmptyContexts ty
ppr_mono_ty (XHsType (HsRecTy{})) _ _ _ = toHtml "{..}"
-- Can now legally occur in ConDeclGADT, the output here is to provide a
-- placeholder in the signature, which is followed by the field
-- declarations.
ppr_mono_ty (XHsType HsCoreTy{}) _ _ _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty (HsExplicitListTy _ IsPromoted tys) u q _ = promoQuote $ brackets $ hsep $ punctuate comma $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty (HsExplicitListTy _ NotPromoted tys) u q _ = brackets $ hsep $ punctuate comma $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty (HsExplicitTupleTy _ IsPromoted tys) u q _ = promoQuote $ parenList $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty (HsExplicitTupleTy _ NotPromoted tys) u q _ = parenList $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty (HsAppTy _ fun_ty arg_ty) unicode qual _ =
  hsep
    [ ppr_mono_lty fun_ty unicode qual HideEmptyContexts
    , ppr_mono_lty arg_ty unicode qual HideEmptyContexts
    ]
ppr_mono_ty (HsAppKindTy _ fun_ty arg_ki) unicode qual _ =
  hsep
    [ ppr_mono_lty fun_ty unicode qual HideEmptyContexts
    , atSign <> ppr_mono_lty arg_ki unicode qual HideEmptyContexts
    ]
ppr_mono_ty (HsOpTy _ prom ty1 op ty2) unicode qual _ =
  ppr_mono_lty ty1 unicode qual HideEmptyContexts <+> ppr_op_prom <+> ppr_mono_lty ty2 unicode qual HideEmptyContexts
  where
    ppr_op_prom
      | isPromoted prom =
          promoQuote ppr_op
      | otherwise =
          ppr_op
    ppr_op = ppLDocName qual Infix op
ppr_mono_ty (HsParTy _ ty) unicode qual emptyCtxts =
  parens (ppr_mono_lty ty unicode qual emptyCtxts)
--  = parens (ppr_mono_lty ctxt_prec ty unicode qual emptyCtxts)

ppr_mono_ty (HsDocTy _ ty _) unicode qual emptyCtxts =
  ppr_mono_lty ty unicode qual emptyCtxts
ppr_mono_ty (HsWildCardTy _) _ _ _ = char '_'
ppr_mono_ty (HsTyLit _ n) _ _ _ = ppr_tylit n
ppr_mono_ty (XHsType HsRedacted{}) _ _ _ = error "ppr_mono_ty: HsRedacted can't be used here"

ppr_tylit :: HsTyLit DocNameI -> Html
ppr_tylit (HsNumTy _ n) = toHtml (show n)
ppr_tylit (HsStrTy _ s) = toHtml (show s)
ppr_tylit (HsCharTy _ c) = toHtml (show c)
