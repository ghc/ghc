{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.Html.Decl
-- Copyright   :  (c) Simon Marlow   2003-2006,
--                    David Waern    2006-2009,
--                    Mark Lentczner 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.Xhtml.Decl (
  ppDecl,

  ppTyName, ppTyFamHeader, ppTypeApp, ppOrphanInstances,
  tyvarNames
) where

import Haddock.Backends.Xhtml.DocMarkup
import Haddock.Backends.Xhtml.Layout
import Haddock.Backends.Xhtml.Names
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Doc (combineDocumentation)

import           Data.List             ( intersperse, sort )
import qualified Data.Map as Map
import           Data.Maybe
import           Text.XHtml hiding     ( name, title, p, quote )

import GHC hiding (LexicalFixity(..))
import GHC.Exts
import Name
import BooleanFormula
import RdrName ( rdrNameOcc )
import Outputable ( panic )

-- | Pretty print a declaration
ppDecl :: Bool                                     -- ^ print summary info only
       -> LinksInfo                                -- ^ link information
       -> LHsDecl DocNameI                         -- ^ declaration to print
       -> [(HsDecl DocNameI, DocForDecl DocName)]  -- ^ relevant pattern synonyms
       -> DocForDecl DocName                       -- ^ documentation for this decl
       -> [DocInstance DocNameI]                   -- ^ relevant instances
       -> [(DocName, Fixity)]                      -- ^ relevant fixities
       -> [(DocName, DocForDecl DocName)]          -- ^ documentation for all decls
       -> Splice
       -> Unicode                                  -- ^ unicode output
       -> Qualification
       -> Html
ppDecl summ links (L loc decl) pats (mbDoc, fnArgsDoc) instances fixities subdocs splice unicode qual = case decl of
  TyClD _ (FamDecl _ d)          -> ppTyFam summ False links instances fixities loc mbDoc d splice unicode qual
  TyClD _ d@(DataDecl {})        -> ppDataDecl summ links instances fixities subdocs loc mbDoc d pats splice unicode qual
  TyClD _ d@(SynDecl {})         -> ppTySyn summ links fixities loc (mbDoc, fnArgsDoc) d splice unicode qual
  TyClD _ d@(ClassDecl {})       -> ppClassDecl summ links instances fixities loc mbDoc subdocs d splice unicode qual
  SigD _ (TypeSig _ lnames lty)  -> ppLFunSig summ links loc (mbDoc, fnArgsDoc) lnames
                                         (hsSigWcType lty) fixities splice unicode qual
  SigD _ (PatSynSig _ lnames lty) -> ppLPatSig summ links loc (mbDoc, fnArgsDoc) lnames
                                         (hsSigType lty) fixities splice unicode qual
  ForD _ d                       -> ppFor summ links loc (mbDoc, fnArgsDoc) d fixities splice unicode qual
  InstD _ _                      -> noHtml
  DerivD _ _                     -> noHtml
  _                              -> error "declaration not supported by ppDecl"


ppLFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             [Located DocName] -> LHsType DocNameI -> [(DocName, Fixity)] ->
             Splice -> Unicode -> Qualification -> Html
ppLFunSig summary links loc doc lnames lty fixities splice unicode qual =
  ppFunSig summary links loc doc (map unLoc lnames) lty fixities
           splice unicode qual

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
            [DocName] -> LHsType DocNameI -> [(DocName, Fixity)] ->
            Splice -> Unicode -> Qualification -> Html
ppFunSig summary links loc doc docnames typ fixities splice unicode qual =
  ppSigLike summary links loc mempty doc docnames fixities (unLoc typ, pp_typ)
            splice unicode qual HideEmptyContexts
  where
    pp_typ = ppLType unicode qual HideEmptyContexts typ

-- | Pretty print a pattern synonym
ppLPatSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName
          -> [Located DocName]     -- ^ names of patterns in declaration
          -> LHsType DocNameI      -- ^ type of patterns in declaration
          -> [(DocName, Fixity)]
          -> Splice -> Unicode -> Qualification -> Html
ppLPatSig summary links loc doc lnames typ fixities splice unicode qual =
  ppSigLike summary links loc (keyword "pattern") doc (map unLoc lnames) fixities
            (unLoc typ, pp_typ) splice unicode qual (patSigContext typ)
  where
    pp_typ = ppPatSigType unicode qual typ


ppSigLike :: Bool -> LinksInfo -> SrcSpan -> Html -> DocForDecl DocName ->
             [DocName] -> [(DocName, Fixity)] -> (HsType DocNameI, Html) ->
             Splice -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppSigLike summary links loc leader doc docnames fixities (typ, pp_typ)
          splice unicode qual emptyCtxts =
  ppTypeOrFunSig summary links loc docnames typ doc
    ( addFixities $ leader <+> ppTypeSig summary occnames pp_typ unicode
    , (leader <+>) . addFixities . concatHtml . punctuate comma $ map (ppBinder False) occnames
    , dcolon unicode
    )
    splice unicode qual emptyCtxts
  where
    occnames = map (nameOccName . getName) docnames
    addFixities html
      | summary   = html
      | otherwise = html <+> ppFixities fixities qual


ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> [DocName] -> HsType DocNameI
               -> DocForDecl DocName -> (Html, Html, Html)
               -> Splice -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppTypeOrFunSig summary links loc docnames typ (doc, argDocs) (pref1, pref2, sep) splice unicode qual emptyCtxts
  | summary = pref1
  | Map.null argDocs = topDeclElem links loc splice docnames pref1 +++ docSection curName qual doc
  | otherwise = topDeclElem links loc splice docnames pref2
                  +++ subArguments qual (ppSubSigLike unicode qual typ argDocs [] sep emptyCtxts)
                  +++ docSection curName qual doc
  where
    curName = getName <$> listToMaybe docnames


-- This splits up a type signature along `->` and adds docs (when they exist) to
-- the arguments.
--
-- If one passes in a list of the available subdocs, any top-level `HsRecTy`
-- found will be expanded out into their fields.
ppSubSigLike :: Unicode -> Qualification
             -> HsType DocNameI                  -- ^ type signature
             -> FnArgsDoc DocName                -- ^ docs to add
             -> [(DocName, DocForDecl DocName)]  -- ^ all subdocs (useful when
                                                 -- we expand an `HsRecTy`)
             -> Html -> HideEmptyContexts -> [SubDecl]
ppSubSigLike unicode qual typ argDocs subdocs sep emptyCtxts = do_args 0 sep typ
  where
    argDoc n = Map.lookup n argDocs

    do_largs n leader (L _ t) = do_args n leader t

    do_args :: Int -> Html -> HsType DocNameI -> [SubDecl]
    do_args n leader (HsForAllTy _ tvs ltype)
      = do_largs n leader' ltype
      where
        leader' = leader <+> ppForAll tvs unicode qual

    do_args n leader (HsQualTy _ lctxt ltype)
      | null (unLoc lctxt)
      = do_largs n leader ltype
      | otherwise
      = (leader <+> ppLContextNoArrow lctxt unicode qual emptyCtxts, Nothing, [])
        : do_largs n (darrow unicode) ltype

    do_args n leader (HsFunTy _ (L _ (HsRecTy _ fields)) r)
      = [ (ldr <+> html, mdoc, subs)
        | (L _ field, ldr) <- zip fields (leader <+> gadtOpen : repeat gadtComma)
        , let (html, mdoc, subs) = ppSideBySideField subdocs unicode qual field
        ]
        ++ do_largs (n+1) (gadtEnd <+> arrow unicode) r

    do_args n leader (HsFunTy _ lt r)
      = (leader <+> ppLFunLhType unicode qual emptyCtxts lt, argDoc n, [])
        : do_largs (n+1) (arrow unicode) r

    do_args n leader t
      = [(leader <+> ppType unicode qual emptyCtxts t, argDoc n, [])]


    -- FIXME: this should be done more elegantly
    --
    -- We need 'gadtComma' and 'gadtEnd' to line up with the `{` from
    -- 'gadtOpen', so we add 3 spaces to cover for `-> `/`:: ` (3 in unicode
    -- mode since `->` and `::` are rendered as single characters.
    gadtComma = concatHtml (replicate (if unicode then 2 else 3) spaceHtml) <> toHtml ","
    gadtEnd = concatHtml (replicate (if unicode then 2 else 3) spaceHtml) <> toHtml "}"
    gadtOpen = toHtml "{"



ppForAll :: [LHsTyVarBndr DocNameI] -> Unicode -> Qualification -> Html
ppForAll tvs unicode qual =
  case [ppKTv n k | L _ (KindedTyVar _ (L _ n) k) <- tvs] of
    [] -> noHtml
    ts -> forallSymbol unicode <+> hsep ts +++ dot
  where ppKTv n k = parens $
          ppTyName (getName n) <+> dcolon unicode <+> ppLKind unicode qual k

ppFixities :: [(DocName, Fixity)] -> Qualification -> Html
ppFixities [] _ = noHtml
ppFixities fs qual = foldr1 (+++) (map ppFix uniq_fs) +++ rightEdge
  where
    ppFix (ns, p, d) = thespan ! [theclass "fixity"] <<
                         (toHtml d <+> toHtml (show p) <+> ppNames ns)

    ppDir InfixR = "infixr"
    ppDir InfixL = "infixl"
    ppDir InfixN = "infix"

    ppNames = case fs of
      _:[] -> const noHtml -- Don't display names for fixities on single names
      _    -> concatHtml . intersperse (stringToHtml ", ") . map (ppDocName qual Infix False)

    uniq_fs = [ (n, the p, the d') | (n, Fixity _ p d) <- fs
                                   , let d' = ppDir d
                                   , then group by Down (p,d') using groupWith ]

    rightEdge = thespan ! [theclass "rightedge"] << noHtml


-- | Pretty-print type variables.
ppTyVars :: Unicode -> Qualification -> [LHsTyVarBndr DocNameI] -> [Html]
ppTyVars unicode qual tvs = map (ppHsTyVarBndr unicode qual . unLoc) tvs

tyvarNames :: LHsQTyVars DocNameI -> [Name]
tyvarNames = map (getName . hsLTyVarName) . hsQTvExplicit


ppFor :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName
      -> ForeignDecl DocNameI -> [(DocName, Fixity)]
      -> Splice -> Unicode -> Qualification -> Html
ppFor summary links loc doc (ForeignImport _ (L _ name) typ _) fixities
      splice unicode qual
  = ppFunSig summary links loc doc [name] (hsSigType typ) fixities splice unicode qual
ppFor _ _ _ _ _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> [(DocName, Fixity)] -> SrcSpan
        -> DocForDecl DocName -> TyClDecl DocNameI
        -> Splice -> Unicode -> Qualification -> Html
ppTySyn summary links fixities loc doc (SynDecl { tcdLName = L _ name, tcdTyVars = ltyvars
                                                , tcdRhs = ltype })
        splice unicode qual
  = ppTypeOrFunSig summary links loc [name] (unLoc ltype) doc
                   (full <+> fixs, hdr <+> fixs, spaceHtml +++ equals)
                   splice unicode qual ShowEmptyToplevelContexts
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ]
                 ++ ppTyVars unicode qual (hsQTvExplicit ltyvars))
    full = hdr <+> equals <+> ppPatSigType unicode qual ltype
    occ  = nameOccName . getName $ name
    fixs
      | summary   = noHtml
      | otherwise = ppFixities fixities qual
ppTySyn _ _ _ _ _ _ _ _ _ = error "declaration not supported by ppTySyn"


ppTypeSig :: Bool -> [OccName] -> Html -> Unicode -> Html
ppTypeSig summary nms pp_ty unicode =
  concatHtml htmlNames <+> dcolon unicode <+> pp_ty
  where
    htmlNames = intersperse (stringToHtml ", ") $ map (ppBinder summary) nms


ppTyName :: Name -> Html
ppTyName = ppName Prefix


ppSimpleSig :: LinksInfo -> Splice -> Unicode -> Qualification -> HideEmptyContexts -> SrcSpan
            -> [DocName] -> HsType DocNameI
            -> Html
ppSimpleSig links splice unicode qual emptyCtxts loc names typ =
    topDeclElem' names $ ppTypeSig True occNames ppTyp unicode
  where
    topDeclElem' = topDeclElem links loc splice
    ppTyp = ppType unicode qual emptyCtxts typ
    occNames = map getOccName names


--------------------------------------------------------------------------------
-- * Type families
--------------------------------------------------------------------------------


ppFamilyInfo :: Bool -> FamilyInfo DocNameI -> Html
ppFamilyInfo assoc OpenTypeFamily
    | assoc = keyword "type"
    | otherwise = keyword "type family"
ppFamilyInfo assoc DataFamily
    | assoc = keyword "data"
    | otherwise = keyword "data family"
ppFamilyInfo _ (ClosedTypeFamily _) = keyword "type family"


ppTyFamHeader :: Bool -> Bool -> FamilyDecl DocNameI
              -> Unicode -> Qualification -> Html
ppTyFamHeader summary associated d@(FamilyDecl { fdInfo = info
                                               , fdResultSig = L _ result
                                               , fdInjectivityAnn = injectivity })
              unicode qual =
  (case info of
     OpenTypeFamily
       | associated -> keyword "type"
       | otherwise  -> keyword "type family"
     DataFamily
       | associated -> keyword "data"
       | otherwise  -> keyword "data family"
     ClosedTypeFamily _
                    -> keyword "type family"
  ) <+>

  ppFamDeclBinderWithVars summary unicode qual d <+>
  ppResultSig result unicode qual <+>

  (case injectivity of
     Nothing                   -> noHtml
     Just (L _ injectivityAnn) -> ppInjectivityAnn unicode qual injectivityAnn
  ) <+>

  (case info of
     ClosedTypeFamily _ -> keyword "where ..."
     _                  -> mempty
  )
ppTyFamHeader _ _ (XFamilyDecl _) _ _ = panic "haddock;ppTyFamHeader"

ppResultSig :: FamilyResultSig DocNameI -> Unicode -> Qualification -> Html
ppResultSig result unicode qual = case result of
    NoSig _               -> noHtml
    KindSig _ kind        -> dcolon unicode  <+> ppLKind unicode qual kind
    TyVarSig _ (L _ bndr) -> equals <+> ppHsTyVarBndr unicode qual bndr
    XFamilyResultSig _    -> panic "haddock:ppResultSig"

ppPseudoFamilyHeader :: Unicode -> Qualification -> PseudoFamilyDecl DocNameI
                     -> Html
ppPseudoFamilyHeader unicode qual (PseudoFamilyDecl { .. }) =
    ppFamilyInfo True pfdInfo <+>
    ppAppNameTypes (unLoc pfdLName) (map unLoc pfdTyVars) unicode qual <+>
    ppResultSig (unLoc pfdKindSig) unicode qual

ppInjectivityAnn :: Bool -> Qualification -> InjectivityAnn DocNameI -> Html
ppInjectivityAnn unicode qual (InjectivityAnn lhs rhs) =
    char '|' <+> ppLDocName qual Raw lhs <+> arrow unicode <+>
    hsep (map (ppLDocName qual Raw) rhs)


ppTyFam :: Bool -> Bool -> LinksInfo -> [DocInstance DocNameI] ->
           [(DocName, Fixity)] -> SrcSpan -> Documentation DocName ->
           FamilyDecl DocNameI -> Splice -> Unicode -> Qualification -> Html
ppTyFam summary associated links instances fixities loc doc decl splice unicode qual

  | summary   = ppTyFamHeader True associated decl unicode qual
  | otherwise = header_ +++ docSection Nothing qual doc +++ instancesBit

  where
    docname = unLoc $ fdLName decl

    header_ = topDeclElem links loc splice [docname] $
       ppTyFamHeader summary associated decl unicode qual <+> ppFixities fixities qual

    instancesBit
      | FamilyDecl { fdInfo = ClosedTypeFamily mb_eqns } <- decl
      , not summary
      = subEquations qual $ map (ppTyFamEqn . unLoc) $ fromMaybe [] mb_eqns

      | otherwise
      = ppInstances links (OriginFamily docname) instances splice unicode qual

    -- Individual equation of a closed type family
    ppTyFamEqn :: TyFamInstEqn DocNameI -> SubDecl
    ppTyFamEqn (HsIB { hsib_body = FamEqn { feqn_tycon = n, feqn_rhs = rhs
                                          , feqn_pats = ts } })
      = ( ppAppNameTypes (unLoc n) (map unLoc ts) unicode qual
          <+> equals <+> ppType unicode qual HideEmptyContexts (unLoc rhs)
        , Nothing, [] )
    ppTyFamEqn (XHsImplicitBndrs _) = panic "haddock:ppTyFam"
    ppTyFamEqn (HsIB { hsib_body = XFamEqn _}) = panic "haddock:ppTyFam"



ppPseudoFamilyDecl :: LinksInfo -> Splice -> Unicode -> Qualification
                   -> PseudoFamilyDecl DocNameI
                   -> Html
ppPseudoFamilyDecl links splice unicode qual
                   decl@(PseudoFamilyDecl { pfdLName = L loc name, .. }) =
    wrapper $ ppPseudoFamilyHeader unicode qual decl
  where
    wrapper = topDeclElem links loc splice [name]


--------------------------------------------------------------------------------
-- * Associated Types
--------------------------------------------------------------------------------


ppAssocType :: Bool -> LinksInfo -> DocForDecl DocName -> LFamilyDecl DocNameI
            -> [(DocName, Fixity)] -> Splice -> Unicode -> Qualification -> Html
ppAssocType summ links doc (L loc decl) fixities splice unicode qual =
   ppTyFam summ True links [] fixities loc (fst doc) decl splice unicode qual


--------------------------------------------------------------------------------
-- * TyClDecl helpers
--------------------------------------------------------------------------------

-- | Print a type family and its variables
ppFamDeclBinderWithVars :: Bool -> Unicode -> Qualification -> FamilyDecl DocNameI -> Html
ppFamDeclBinderWithVars summ unicode qual (FamilyDecl { fdLName = lname, fdTyVars = tvs }) =
  ppAppDocNameTyVarBndrs summ unicode qual (unLoc lname) (hsq_explicit tvs)
ppFamDeclBinderWithVars _ _ _ (XFamilyDecl _) = panic "haddock:ppFamDeclBinderWithVars"

-- | Print a newtype / data binder and its variables
ppDataBinderWithVars :: Bool -> Unicode -> Qualification -> TyClDecl DocNameI -> Html
ppDataBinderWithVars summ unicode qual decl =
  ppAppDocNameTyVarBndrs summ unicode qual (tcdName decl) (hsQTvExplicit $ tcdTyVars decl)

--------------------------------------------------------------------------------
-- * Type applications
--------------------------------------------------------------------------------

ppAppDocNameTyVarBndrs :: Bool -> Unicode -> Qualification -> DocName -> [LHsTyVarBndr DocNameI] -> Html
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


-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (Notation -> DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n (t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN Infix n <+> ppT t2

ppTypeApp n ts ppDN ppT = ppDN Prefix n <+> hsep (map ppT ts)


-------------------------------------------------------------------------------
-- * Contexts
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocNameI) -> Unicode
                              -> Qualification -> HideEmptyContexts -> Html
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc

ppContextNoArrow :: HsContext DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppContextNoArrow cxt unicode qual emptyCtxts = fromMaybe noHtml $
                                               ppContextNoLocsMaybe (map unLoc cxt) unicode qual emptyCtxts


ppContextNoLocs :: [HsType DocNameI] -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppContextNoLocs cxt unicode qual emptyCtxts = maybe noHtml (<+> darrow unicode) $
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
ppHsContext []  _       _    = noHtml
ppHsContext [p] unicode qual = ppCtxType unicode qual p
ppHsContext cxt unicode qual = parenList (map (ppType unicode qual HideEmptyContexts) cxt)


-------------------------------------------------------------------------------
-- * Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsType DocNameI] -> DocName
           -> LHsQTyVars DocNameI -> [Located ([Located DocName], [Located DocName])]
           -> Unicode -> Qualification -> Html
ppClassHdr summ lctxt n tvs fds unicode qual =
  keyword "class"
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode qual HideEmptyContexts else noHtml)
  <+> ppAppDocNameTyVarBndrs summ unicode qual n (hsQTvExplicit tvs)
  <+> ppFds fds unicode qual


ppFds :: [Located ([Located DocName], [Located DocName])] -> Unicode -> Qualification -> Html
ppFds fds unicode qual =
  if null fds then noHtml else
        char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
        fundep (vars1,vars2) = ppVars vars1 <+> arrow unicode <+> ppVars vars2
        ppVars = hsep . map ((ppDocName qual Prefix True) . unLoc)

ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocNameI -> SrcSpan
                 -> [(DocName, DocForDecl DocName)]
                 -> Splice -> Unicode -> Qualification -> Html
ppShortClassDecl summary links (ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = tvs
                                          , tcdFDs = fds, tcdSigs = sigs, tcdATs = ats }) loc
    subdocs splice unicode qual =
  if not (any isUserLSig sigs) && null ats
    then (if summary then id else topDeclElem links loc splice [nm]) hdr
    else (if summary then id else topDeclElem links loc splice [nm]) (hdr <+> keyword "where")
      +++ shortSubDecls False
          (
            [ ppAssocType summary links doc at [] splice unicode qual | at <- ats
              , let doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs ]  ++

                -- ToDo: add associated type defaults

            [ ppFunSig summary links loc doc names (hsSigWcType typ)
                       [] splice unicode qual
              | L _ (TypeSig _ lnames typ) <- sigs
              , let doc = lookupAnySubdoc (head names) subdocs
                    names = map unLoc lnames ]
              -- FIXME: is taking just the first name ok? Is it possible that
              -- there are different subdocs for different names in a single
              -- type signature?
          )
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds unicode qual
    nm  = unLoc lname
ppShortClassDecl _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"



ppClassDecl :: Bool -> LinksInfo -> [DocInstance DocNameI] -> [(DocName, Fixity)]
            -> SrcSpan -> Documentation DocName
            -> [(DocName, DocForDecl DocName)] -> TyClDecl DocNameI
            -> Splice -> Unicode -> Qualification -> Html
ppClassDecl summary links instances fixities loc d subdocs
        decl@(ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = ltyvars
                        , tcdFDs = lfds, tcdSigs = lsigs, tcdATs = ats })
            splice unicode qual
  | summary = ppShortClassDecl summary links decl loc subdocs splice unicode qual
  | otherwise = classheader +++ docSection Nothing qual d
                  +++ minimalBit +++ atBit +++ methodBit +++ instancesBit
  where
    sigs = map unLoc lsigs

    classheader
      | any isUserLSig lsigs = topDeclElem links loc splice [nm] (hdr unicode qual <+> keyword "where" <+> fixs)
      | otherwise = topDeclElem links loc splice [nm] (hdr unicode qual <+> fixs)

    -- Only the fixity relevant to the class header
    fixs = ppFixities [ f | f@(n,_) <- fixities, n == unLoc lname ] qual

    nm   = tcdName decl

    hdr = ppClassHdr summary lctxt (unLoc lname) ltyvars lfds

    -- ToDo: add assocatied typ defaults
    atBit = subAssociatedTypes [ ppAssocType summary links doc at subfixs splice unicode qual
                      | at <- ats
                      , let n = unL . fdLName $ unL at
                            doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs
                            subfixs = [ f | f@(n',_) <- fixities, n == n' ] ]

    methodBit = subMethods [ ppFunSig summary links loc doc names (hsSigType typ)
                                      subfixs splice unicode qual
                           | L _ (ClassOpSig _ _ lnames typ) <- lsigs
                           , let doc = lookupAnySubdoc (head names) subdocs
                                 subfixs = [ f | n <- names
                                               , f@(n',_) <- fixities
                                               , n == n' ]
                                 names = map unLoc lnames ]
                           -- N.B. taking just the first name is ok. Signatures with multiple names
                           -- are expanded so that each name gets its own signature.

    minimalBit = case [ s | MinimalSig _ _ (L _ s) <- sigs ] of
      -- Miminal complete definition = every shown method
      And xs : _ | sort [getName n | L _ (Var (L _ n)) <- xs] ==
                   sort [getName n | TypeSig _ ns _ <- sigs, L _ n <- ns]
        -> noHtml

      -- Minimal complete definition = the only shown method
      Var (L _ n) : _ | [getName n] ==
                        [getName n' | L _ (TypeSig _ ns _) <- lsigs, L _ n' <- ns]
        -> noHtml

      -- Minimal complete definition = nothing
      And [] : _ -> subMinimal $ toHtml "Nothing"

      m : _  -> subMinimal $ ppMinimal False m
      _ -> noHtml

    ppMinimal _ (Var (L _ n)) = ppDocName qual Prefix True n
    ppMinimal _ (And fs) = foldr1 (\a b -> a+++", "+++b) $ map (ppMinimal True . unLoc) fs
    ppMinimal p (Or fs) = wrap $ foldr1 (\a b -> a+++" | "+++b) $ map (ppMinimal False . unLoc) fs
      where wrap | p = parens | otherwise = id
    ppMinimal p (Parens x) = ppMinimal p (unLoc x)

    instancesBit = ppInstances links (OriginClass nm) instances
        splice unicode qual

ppClassDecl _ _ _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstances :: LinksInfo
            -> InstOrigin DocName -> [DocInstance DocNameI]
            -> Splice -> Unicode -> Qualification
            -> Html
ppInstances links origin instances splice unicode qual
  = subInstances qual instName links True (zipWith instDecl [1..] instances)
  -- force Splice = True to use line URLs
  where
    instName = getOccString origin
    instDecl :: Int -> DocInstance DocNameI -> (SubDecl,Located DocName)
    instDecl no (inst, mdoc, loc) =
        ((ppInstHead links splice unicode qual mdoc origin False no inst), loc)


ppOrphanInstances :: LinksInfo
                  -> [DocInstance DocNameI]
                  -> Splice -> Unicode -> Qualification
                  -> Html
ppOrphanInstances links instances splice unicode qual
  = subOrphanInstances qual links True (zipWith instDecl [1..] instances)
  where
    instOrigin :: InstHead name -> InstOrigin (IdP name)
    instOrigin inst = OriginClass (ihdClsName inst)

    instDecl :: Int -> DocInstance DocNameI -> (SubDecl,Located DocName)
    instDecl no (inst, mdoc, loc) =
        ((ppInstHead links splice unicode qual mdoc (instOrigin inst) True no inst), loc)


ppInstHead :: LinksInfo -> Splice -> Unicode -> Qualification
           -> Maybe (MDoc DocName)
           -> InstOrigin DocName
           -> Bool -- ^ Is instance orphan
           -> Int  -- ^ Normal
           -> InstHead DocNameI
           -> SubDecl
ppInstHead links splice unicode qual mdoc origin orphan no ihd@(InstHead {..}) =
    case ihdInstType of
        ClassInst { .. } ->
            ( subInstHead iid $ ppContextNoLocs clsiCtx unicode qual HideEmptyContexts <+> typ
            , mdoc
            , [subInstDetails iid ats sigs]
            )
          where
            sigs = ppInstanceSigs links splice unicode qual clsiSigs
            ats = ppInstanceAssocTys links splice unicode qual clsiAssocTys
        TypeInst rhs ->
            ( subInstHead iid ptype
            , mdoc
            , [subFamInstDetails iid prhs]
            )
          where
            ptype = keyword "type" <+> typ
            prhs = ptype <+> maybe noHtml
                                   (\t -> equals <+> ppType unicode qual HideEmptyContexts t) rhs
        DataInst dd ->
            ( subInstHead iid pdata
            , mdoc
            , [subFamInstDetails iid pdecl])
          where
            pdata = keyword "data" <+> typ
            pdecl = pdata <+> ppShortDataDecl False True dd [] unicode qual
  where
    iid = instanceId origin no orphan ihd
    typ = ppAppNameTypes ihdClsName ihdTypes unicode qual


ppInstanceAssocTys :: LinksInfo -> Splice -> Unicode -> Qualification
                   -> [PseudoFamilyDecl DocNameI]
                   -> [Html]
ppInstanceAssocTys links splice unicode qual =
    map ppFamilyDecl'
  where
    ppFamilyDecl' = ppPseudoFamilyDecl links splice unicode qual


ppInstanceSigs :: LinksInfo -> Splice -> Unicode -> Qualification
              -> [Sig DocNameI]
              -> [Html]
ppInstanceSigs links splice unicode qual sigs = do
    TypeSig _ lnames typ <- sigs
    let names = map unLoc lnames
        L _ rtyp = hsSigWcType typ
    -- Instance methods signatures are synified and thus don't have a useful
    -- SrcSpan value. Use the methods name location instead.
    return $ ppSimpleSig links splice unicode qual HideEmptyContexts (getLoc $ head $ lnames) names rtyp


lookupAnySubdoc :: Eq id1 => id1 -> [(id1, DocForDecl id2)] -> DocForDecl id2
lookupAnySubdoc n = fromMaybe noDocForDecl . lookup n


instanceId :: InstOrigin DocName -> Int -> Bool -> InstHead DocNameI -> String
instanceId origin no orphan ihd = concat $
    [ "o:" | orphan ] ++
    [ qual origin
    , ":" ++ getOccString origin
    , ":" ++ (occNameString . getOccName . ihdClsName) ihd
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
ppShortDataDecl :: Bool -> Bool -> TyClDecl DocNameI
                -> [(HsDecl DocNameI, DocForDecl DocName)]
                -> Unicode -> Qualification -> Html
ppShortDataDecl summary dataInst dataDecl pats unicode qual

  | [] <- cons
  , [] <- pats = dataHeader

  | [lcon] <- cons, [] <- pats, isH98,
    (cHead,cBody,cFoot) <- ppShortConstrParts summary dataInst (unLoc lcon) unicode qual
       = (dataHeader <+> equals <+> cHead) +++ cBody +++ cFoot

  | [] <- pats, isH98 = dataHeader
      +++ shortSubDecls dataInst (zipWith doConstr ('=':repeat '|') cons ++ pats1)

  | otherwise = (dataHeader <+> keyword "where")
      +++ shortSubDecls dataInst (map doGADTConstr cons ++ pats1)

  where
    dataHeader
      | dataInst  = noHtml
      | otherwise = ppDataHeader summary dataDecl unicode qual
    doConstr c con = toHtml [c] <+> ppShortConstr summary (unLoc con) unicode qual
    doGADTConstr con = ppShortConstr summary (unLoc con) unicode qual

    cons      = dd_cons (tcdDataDefn dataDecl)
    isH98     = case unLoc (head cons) of
                  ConDeclH98 {} -> True
                  ConDeclGADT{} -> False
                  XConDecl{}    -> False

    pats1 = [ hsep [ keyword "pattern"
                   , hsep $ punctuate comma $ map (ppBinder summary . getOccName) lnames
                   , dcolon unicode
                   , ppPatSigType unicode qual (hsSigType typ)
                   ]
            | (SigD _ (PatSynSig _ lnames typ),_) <- pats
            ]


-- | Pretty-print a data declaration
ppDataDecl :: Bool -> LinksInfo
           -> [DocInstance DocNameI]                  -- ^ relevant instances
           -> [(DocName, Fixity)]                     -- ^ relevant fixities
           -> [(DocName, DocForDecl DocName)]         -- ^ all decl documentation
           -> SrcSpan
           -> Documentation DocName                   -- ^ this decl's documentation
           -> TyClDecl DocNameI                       -- ^ this decl
           -> [(HsDecl DocNameI, DocForDecl DocName)] -- ^ relevant patterns
           -> Splice -> Unicode -> Qualification -> Html
ppDataDecl summary links instances fixities subdocs loc doc dataDecl pats
           splice unicode qual

  | summary   = ppShortDataDecl summary False dataDecl pats unicode qual
  | otherwise = header_ +++ docSection Nothing qual doc +++ constrBit +++ patternBit +++ instancesBit

  where
    docname   = tcdName dataDecl
    cons      = dd_cons (tcdDataDefn dataDecl)
    isH98     = case unLoc (head cons) of
                  ConDeclH98 {} -> True
                  ConDeclGADT{} -> False
                  XConDecl{}    -> False

    header_ = topDeclElem links loc splice [docname] $
             ppDataHeader summary dataDecl unicode qual <+> whereBit <+> fix

    fix = ppFixities (filter (\(n,_) -> n == docname) fixities) qual

    whereBit
      | null cons
      , null pats = noHtml
      | null cons = keyword "where"
      | otherwise = if isH98 then noHtml else keyword "where"

    constrBit = subConstructors qual
      [ ppSideBySideConstr subdocs subfixs unicode qual c
      | c <- cons
      , let subfixs = filter (\(n,_) -> any (\cn -> cn == n)
                                            (map unLoc (getConNames (unLoc c)))) fixities
      ]

    patternBit = subPatterns qual
      [ ppSideBySidePat subfixs unicode qual lnames typ d
      | (SigD _ (PatSynSig _ lnames typ), d) <- pats
      , let subfixs = filter (\(n,_) -> any (\cn -> cn == n)
                                            (map unLoc lnames)) fixities
      ]

    instancesBit = ppInstances links (OriginData docname) instances
        splice unicode qual


ppShortConstr :: Bool -> ConDecl DocNameI -> Unicode -> Qualification -> Html
ppShortConstr summary con unicode qual = cHead <+> cBody <+> cFoot
  where
    (cHead,cBody,cFoot) = ppShortConstrParts summary False con unicode qual


-- returns three pieces: header, body, footer so that header & footer can be
-- incorporated into the declaration
ppShortConstrParts :: Bool -> Bool -> ConDecl DocNameI -> Unicode -> Qualification -> (Html, Html, Html)
ppShortConstrParts summary dataInst con unicode qual
  = case con of
      ConDeclH98{ con_args = det
                , con_ex_tvs = vars
                , con_mb_cxt = cxt
                } -> let tyVars = map (getName . hsLTyVarName) vars
                         context = unLoc (fromMaybe (noLoc []) cxt)
                         forall_ = False
                         header_ = ppConstrHdr forall_ tyVars context unicode qual
                     in case det of

        -- Prefix constructor, e.g. 'Just a'
        PrefixCon args ->
          ( header_ +++ hsep (ppOcc : map (ppLParendType unicode qual HideEmptyContexts) args)
          , noHtml
          , noHtml
          )

        -- Record constructor, e.g. 'Identity { runIdentity :: a }'
        RecCon (L _ fields) ->
          ( header_ +++ ppOcc <+> char '{'
          , shortSubDecls dataInst [ ppShortField summary unicode qual field
                                   | L _ field <- fields
                                   ]
          , char '}'
          )

        -- Infix constructor, e.g. 'a :| [a]'
        InfixCon arg1 arg2 ->
          ( header_ +++ hsep [ ppLParendType unicode qual HideEmptyContexts arg1
                             , ppOccInfix
                             , ppLParendType unicode qual HideEmptyContexts arg2
                             ]
          , noHtml
          , noHtml
          )

      -- GADT constructor, e.g. 'Foo :: Int -> Foo'
      ConDeclGADT {} ->
          ( hsep [ ppOcc, dcolon unicode, ppLType unicode qual HideEmptyContexts (getGADTConType con) ]
          , noHtml
          , noHtml
          )
      XConDecl {} -> panic "haddock:ppShortConstrParts"

  where
    occ        = map (nameOccName . getName . unLoc) $ getConNames con
    ppOcc      = hsep (punctuate comma (map (ppBinder summary) occ))
    ppOccInfix = hsep (punctuate comma (map (ppBinderInfix summary) occ))


-- | Pretty print an expanded constructor
ppSideBySideConstr :: [(DocName, DocForDecl DocName)] -> [(DocName, Fixity)]
                   -> Unicode -> Qualification
                   -> LConDecl DocNameI -- ^ constructor declaration to print
                   -> SubDecl
ppSideBySideConstr subdocs fixities unicode qual (L _ con)
 = ( decl       -- Constructor header (name, fixity)
   , mbDoc      -- Docs on the whole constructor
   , fieldPart  -- Information on the fields (or arguments, if they have docs)
   )
 where
    -- Find the name of a constructors in the decl (`getConName` always returns a non-empty list)
    aConName = unLoc (head (getConNames con))

    fixity   = ppFixities fixities qual
    occ      = map (nameOccName . getName . unLoc) $ getConNames con

    ppOcc      = hsep (punctuate comma (map (ppBinder False) occ))
    ppOccInfix = hsep (punctuate comma (map (ppBinderInfix False) occ))

    -- Extract out the map of of docs corresponding to the constructors arguments
    argDocs = maybe Map.empty snd (lookup aConName subdocs)
    hasArgDocs = not $ Map.null argDocs

    decl = case con of
      ConDeclH98{ con_args = det
                , con_ex_tvs = vars
                , con_mb_cxt = cxt
                } -> let tyVars = map (getName . hsLTyVarName) vars
                         context = unLoc (fromMaybe (noLoc []) cxt)
                         forall_ = False
                         header_ = ppConstrHdr forall_ tyVars context unicode qual
                     in case det of
        -- Prefix constructor, e.g. 'Just a'
        PrefixCon args
          | hasArgDocs -> header_ +++ ppOcc <+> fixity
          | otherwise -> hsep [ header_ +++ ppOcc
                              , hsep (map (ppLParendType unicode qual HideEmptyContexts) args)
                              , fixity
                              ]

        -- Record constructor, e.g. 'Identity { runIdentity :: a }'
        RecCon _ -> header_ +++ ppOcc <+> fixity

        -- Infix constructor, e.g. 'a :| [a]'
        InfixCon arg1 arg2
          | hasArgDocs -> header_ +++ ppOcc <+> fixity
          | otherwise -> hsep [ header_ +++ ppLParendType unicode qual HideEmptyContexts arg1
                              , ppOccInfix
                              , ppLParendType unicode qual HideEmptyContexts arg2
                              , fixity
                              ]

      -- GADT constructor, e.g. 'Foo :: Int -> Foo'
      ConDeclGADT{}
          | hasArgDocs || not (null fieldPart) -> ppOcc <+> fixity
          | otherwise -> hsep [ ppOcc
                              , dcolon unicode
                              -- ++AZ++ make this prepend "{..}" when it is a record style GADT
                              , ppLType unicode qual HideEmptyContexts (getGADTConType con)
                              , fixity
                              ]
      XConDecl{} -> panic "haddock:ppSideBySideConstr"

    fieldPart = case (con, getConArgs con) of
        -- Record style GADTs
        (ConDeclGADT{}, RecCon _)            -> [ doConstrArgsWithDocs [] ]

        -- Regular record declarations
        (_, RecCon (L _ fields))             -> [ doRecordFields fields ]

        -- Any GADT or a regular H98 prefix data constructor
        (_, PrefixCon args)     | hasArgDocs -> [ doConstrArgsWithDocs args ]

        -- An infix H98 data constructor
        (_, InfixCon arg1 arg2) | hasArgDocs -> [ doConstrArgsWithDocs [arg1,arg2] ]

        _ -> []

    doRecordFields fields = subFields qual
      (map (ppSideBySideField subdocs unicode qual) (map unLoc fields))

    doConstrArgsWithDocs args = subFields qual $ case con of
      ConDeclH98{} ->
        [ (ppLParendType unicode qual HideEmptyContexts arg, mdoc, [])
        | (i, arg) <- zip [0..] args
        , let mdoc = Map.lookup i argDocs
        ]
      ConDeclGADT{} ->
        ppSubSigLike unicode qual (unLoc (getGADTConType con))
                     argDocs subdocs (dcolon unicode) HideEmptyContexts
      XConDecl{} -> panic "haddock:doConstrArgsWithDocs"

    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc = lookup (unLoc $ head $ getConNames con) subdocs >>=
            combineDocumentation . fst


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr :: Bool               -- ^ print explicit foralls
            -> [Name]             -- ^ type variables
            -> HsContext DocNameI -- ^ context
            -> Unicode -> Qualification -> Html
ppConstrHdr forall_ tvs ctxt unicode qual = ppForall +++ ppCtxt
  where
    ppForall
      | null tvs || not forall_ = noHtml
      | otherwise = forallSymbol unicode
                      <+> hsep (map (ppName Prefix) tvs)
                      <+> toHtml ". "

    ppCtxt
      | null ctxt = noHtml
      | otherwise = ppContextNoArrow ctxt unicode qual HideEmptyContexts
                      <+> darrow unicode +++ toHtml " "


-- | Pretty-print a record field
ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Unicode -> Qualification
                  -> ConDeclField DocNameI -> SubDecl
ppSideBySideField subdocs unicode qual (ConDeclField _ names ltype _) =
  ( hsep (punctuate comma [ ppBinder False (rdrNameOcc field)
                          | L _ name <- names
                          , let field = (unLoc . rdrNameFieldOcc) name
                          ])
      <+> dcolon unicode
      <+> ppLType unicode qual HideEmptyContexts ltype
  , mbDoc
  , []
  )
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    -- Where there is more than one name, they all have the same documentation
    mbDoc = lookup (extFieldOcc $ unLoc $ head names) subdocs >>= combineDocumentation . fst
ppSideBySideField _ _ _ (XConDeclField _) = panic "haddock:ppSideBySideField"


ppShortField :: Bool -> Unicode -> Qualification -> ConDeclField DocNameI -> Html
ppShortField summary unicode qual (ConDeclField _ names ltype _)
  = hsep (punctuate comma (map ((ppBinder summary) . rdrNameOcc . unLoc . rdrNameFieldOcc . unLoc) names))
    <+> dcolon unicode <+> ppLType unicode qual HideEmptyContexts ltype
ppShortField _ _ _ (XConDeclField _) = panic "haddock:ppShortField"


-- | Pretty print an expanded pattern (for bundled patterns)
ppSideBySidePat :: [(DocName, Fixity)] -> Unicode -> Qualification
                   -> [Located DocName]    -- ^ pattern name(s)
                   -> LHsSigType DocNameI  -- ^ type of pattern(s)
                   -> DocForDecl DocName   -- ^ doc map
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

    decl | hasArgDocs = keyword "pattern" <+> ppOcc <+> fixity
         | otherwise = hsep [ keyword "pattern"
                            , ppOcc
                            , dcolon unicode
                            , ppPatSigType unicode qual (hsSigType typ)
                            , fixity
                            ]

    fieldPart
      | not hasArgDocs = []
      | otherwise = [ subFields qual (ppSubSigLike unicode qual (unLoc patTy)
                                                        argDocs [] (dcolon unicode)
                                                        emptyCtxt) ]

    patTy = hsSigType typ
    emptyCtxt = patSigContext patTy


-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocNameI -> Unicode -> Qualification -> Html
ppDataHeader summary decl@(DataDecl { tcdDataDefn =
                                         HsDataDefn { dd_ND = nd
                                                    , dd_ctxt = ctxt
                                                    , dd_kindSig = ks } })
             unicode qual
  = -- newtype or data
    (case nd of { NewType -> keyword "newtype"; DataType -> keyword "data" })
    <+>
    -- context
    ppLContext ctxt unicode qual HideEmptyContexts <+>
    -- T a b c ..., or a :+: b
    ppDataBinderWithVars summary unicode qual decl
    <+> case ks of
      Nothing -> mempty
      Just (L _ x) -> dcolon unicode <+> ppKind unicode qual x

ppDataHeader _ _ _ _ = error "ppDataHeader: illegal argument"

--------------------------------------------------------------------------------
-- * Types and contexts
--------------------------------------------------------------------------------


ppBang :: HsSrcBang -> Html
ppBang (HsSrcBang _ _ SrcStrict) = toHtml "!"
ppBang (HsSrcBang _ _ SrcLazy)   = toHtml "~"
ppBang _                         = noHtml


tupleParens :: HsTupleSort -> [Html] -> Html
tupleParens HsUnboxedTuple = ubxParenList
tupleParens _              = parenList


sumParens :: [Html] -> Html
sumParens = ubxSumList

--------------------------------------------------------------------------------
-- * Rendering of HsType
--------------------------------------------------------------------------------


pREC_TOP, pREC_CTX, pREC_FUN, pREC_OP, pREC_CON :: Int

pREC_TOP = 0 :: Int   -- type in ParseIface.y in GHC
pREC_CTX = 1 :: Int   -- Used for single contexts, eg. ctx => type
                      -- (as opposed to (ctx1, ctx2) => type)
pREC_FUN = 2 :: Int   -- btype in ParseIface.y in GHC
                      -- Used for LH arg of (->)
pREC_OP  = 3 :: Int   -- Used for arg of any infix operator
                      -- (we don't keep their fixities around)
pREC_CON = 4 :: Int   -- Used for arg of type applicn:
                      -- always parenthesise unless atomic

maybeParen :: Int           -- Precedence of context
           -> Int           -- Precedence of top-level operator
           -> Html -> Html  -- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
                               | otherwise            = p


ppLType, ppLParendType, ppLFunLhType :: Unicode -> Qualification -> HideEmptyContexts -> Located (HsType DocNameI) -> Html
ppLType       unicode qual emptyCtxts y = ppType unicode qual emptyCtxts (unLoc y)
ppLParendType unicode qual emptyCtxts y = ppParendType unicode qual emptyCtxts (unLoc y)
ppLFunLhType  unicode qual emptyCtxts y = ppFunLhType unicode qual emptyCtxts (unLoc y)

ppCtxType :: Unicode -> Qualification -> HsType DocNameI -> Html
ppCtxType unicode qual ty = ppr_mono_ty pREC_CTX ty unicode qual HideEmptyContexts

ppType, ppParendType, ppFunLhType :: Unicode -> Qualification -> HideEmptyContexts -> HsType DocNameI -> Html
ppType       unicode qual emptyCtxts ty = ppr_mono_ty pREC_TOP ty unicode qual emptyCtxts
ppParendType unicode qual emptyCtxts ty = ppr_mono_ty pREC_CON ty unicode qual emptyCtxts
ppFunLhType  unicode qual emptyCtxts ty = ppr_mono_ty pREC_FUN ty unicode qual emptyCtxts

ppHsTyVarBndr :: Unicode -> Qualification -> HsTyVarBndr DocNameI -> Html
ppHsTyVarBndr _       qual (UserTyVar _ (L _ name)) =
    ppDocName qual Raw False name
ppHsTyVarBndr unicode qual (KindedTyVar _ name kind) =
    parens (ppDocName qual Raw False (unLoc name) <+> dcolon unicode <+>
            ppLKind unicode qual kind)
ppHsTyVarBndr _ _ (XTyVarBndr _) = error "haddock:ppHsTyVarBndr"

ppLKind :: Unicode -> Qualification -> LHsKind DocNameI -> Html
ppLKind unicode qual y = ppKind unicode qual (unLoc y)

ppKind :: Unicode -> Qualification -> HsKind DocNameI -> Html
ppKind unicode qual ki = ppr_mono_ty pREC_TOP ki unicode qual HideEmptyContexts

patSigContext :: LHsType name -> HideEmptyContexts
patSigContext typ | hasNonEmptyContext typ && isFirstContextEmpty typ =  ShowEmptyToplevelContexts
                  | otherwise = HideEmptyContexts
  where
    hasNonEmptyContext :: LHsType name -> Bool
    hasNonEmptyContext t =
      case unLoc t of
        HsForAllTy _ _ s -> hasNonEmptyContext s
        HsQualTy _ cxt s -> if null (unLoc cxt) then hasNonEmptyContext s else True
        HsFunTy _ _ s    -> hasNonEmptyContext s
        _ -> False
    isFirstContextEmpty :: LHsType name -> Bool
    isFirstContextEmpty t =
      case unLoc t of
        HsForAllTy _ _ s -> isFirstContextEmpty s
        HsQualTy _ cxt _ -> null (unLoc cxt)
        HsFunTy _ _ s    -> isFirstContextEmpty s
        _ -> False


-- | Pretty-print a pattern signature (all this does over 'ppLType' is slot in
-- the right 'HideEmptyContext' value)
ppPatSigType :: Unicode -> Qualification -> LHsType DocNameI -> Html
ppPatSigType unicode qual typ =
  let emptyCtxts = patSigContext typ in ppLType unicode qual emptyCtxts typ

ppForAllPart :: Unicode -> Qualification -> [LHsTyVarBndr DocNameI] -> Html
ppForAllPart unicode qual tvs = hsep (forallSymbol unicode : ppTyVars unicode qual tvs) +++ dot

ppr_mono_lty :: Int -> LHsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)


ppr_mono_ty :: Int -> HsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_mono_ty ctxt_prec (HsForAllTy _ tvs ty) unicode qual emptyCtxts
  = maybeParen ctxt_prec pREC_FUN $
    ppForAllPart unicode qual tvs <+> ppr_mono_lty pREC_TOP ty unicode qual emptyCtxts

ppr_mono_ty ctxt_prec (HsQualTy _ ctxt ty) unicode qual emptyCtxts
  = maybeParen ctxt_prec pREC_FUN $
    ppLContext ctxt unicode qual emptyCtxts <+> ppr_mono_lty pREC_TOP ty unicode qual emptyCtxts

-- UnicodeSyntax alternatives
ppr_mono_ty _ (HsTyVar _ _ (L _ name)) True _ _
  | getOccString (getName name) == "*"    = toHtml "★"
  | getOccString (getName name) == "(->)" = toHtml "(→)"

ppr_mono_ty _         (HsBangTy _ b ty)     u q _ = ppBang b +++ ppLParendType u q HideEmptyContexts ty
ppr_mono_ty _         (HsTyVar _ _ (L _ name)) _ q _ = ppDocName q Prefix True name
ppr_mono_ty ctxt_prec (HsFunTy _ ty1 ty2)   u q e = ppr_fun_ty ctxt_prec ty1 ty2 u q e
ppr_mono_ty _         (HsTupleTy _ con tys) u q _ = tupleParens con (map (ppLType u q HideEmptyContexts) tys)
ppr_mono_ty _         (HsSumTy _ tys) u q _ = sumParens (map (ppLType u q HideEmptyContexts) tys)
ppr_mono_ty _         (HsKindSig _ ty kind) u q e =
    parens (ppr_mono_lty pREC_TOP ty u q e <+> dcolon u <+> ppLKind u q kind)
ppr_mono_ty _         (HsListTy _ ty)       u q _ = brackets (ppr_mono_lty pREC_TOP ty u q HideEmptyContexts)
ppr_mono_ty _         (HsPArrTy _ ty)       u q _ = pabrackets (ppr_mono_lty pREC_TOP ty u q HideEmptyContexts)
ppr_mono_ty ctxt_prec (HsIParamTy _ (L _ n) ty) u q _ =
    maybeParen ctxt_prec pREC_CTX $ ppIPName n <+> dcolon u <+> ppr_mono_lty pREC_TOP ty u q HideEmptyContexts
ppr_mono_ty _         (HsSpliceTy {})     _ _ _ = error "ppr_mono_ty HsSpliceTy"
ppr_mono_ty _         (HsRecTy {})        _ _ _ = toHtml "{..}"
       -- Can now legally occur in ConDeclGADT, the output here is to provide a
       -- placeholder in the signature, which is followed by the field
       -- declarations.
ppr_mono_ty _         (XHsType (NHsCoreTy {})) _ _ _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty _         (HsExplicitListTy _ Promoted tys) u q _ = promoQuote $ brackets $ hsep $ punctuate comma $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty _         (HsExplicitListTy _ NotPromoted tys) u q _ = brackets $ hsep $ punctuate comma $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty _         (HsExplicitTupleTy _ tys) u q _ = promoQuote $ parenList $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty _         (HsAppsTy {})       _ _ _ = error "ppr_mono_ty HsAppsTy"

ppr_mono_ty ctxt_prec (HsEqTy _ ty1 ty2) unicode qual _
  = maybeParen ctxt_prec pREC_CTX $
    ppr_mono_lty pREC_OP ty1 unicode qual HideEmptyContexts <+> char '~' <+> ppr_mono_lty pREC_OP ty2 unicode qual HideEmptyContexts

ppr_mono_ty ctxt_prec (HsAppTy _ fun_ty arg_ty) unicode qual _
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode qual HideEmptyContexts, ppr_mono_lty pREC_CON arg_ty unicode qual HideEmptyContexts]

ppr_mono_ty ctxt_prec (HsOpTy _ ty1 op ty2) unicode qual _
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode qual HideEmptyContexts <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode qual HideEmptyContexts
  where
    -- `(:)` is valid in type signature only as constructor to promoted list
    -- and needs to be quoted in code so we explicitly quote it here too.
    ppr_op
        | (getOccString . getName . unLoc) op == ":" = promoQuote ppr_op'
        | otherwise = ppr_op'
    ppr_op' = ppLDocName qual Infix op

ppr_mono_ty ctxt_prec (HsParTy _ ty) unicode qual emptyCtxts
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode qual emptyCtxts

ppr_mono_ty ctxt_prec (HsDocTy _ ty _) unicode qual emptyCtxts
  = ppr_mono_lty ctxt_prec ty unicode qual emptyCtxts

ppr_mono_ty _ (HsWildCardTy (AnonWildCard _)) _ _ _ = char '_'
ppr_mono_ty _ (HsTyLit _ n) _ _ _ = ppr_tylit n

ppr_tylit :: HsTyLit -> Html
ppr_tylit (HsNumTy _ n) = toHtml (show n)
ppr_tylit (HsStrTy _ s) = toHtml (show s)

ppr_fun_ty :: Int -> LHsType DocNameI -> LHsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_fun_ty ctxt_prec ty1 ty2 unicode qual emptyCtxts
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode qual HideEmptyContexts
        p2 = ppr_mono_lty pREC_TOP ty2 unicode qual emptyCtxts
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow unicode <+> p2]
