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

ppDecl :: Bool -> LinksInfo -> LHsDecl DocNameI
       -> [(HsDecl DocNameI, DocForDecl DocName)]
       -> DocForDecl DocName ->  [DocInstance DocNameI] -> [(DocName, Fixity)]
       -> [(DocName, DocForDecl DocName)] -> Splice -> Unicode
       -> Maybe Package -> Qualification -> Html
ppDecl summ links (L loc decl) pats (mbDoc, fnArgsDoc) instances fixities subdocs splice unicode pkg qual = case decl of
  TyClD (FamDecl d)            -> ppTyFam summ False links instances fixities loc mbDoc d splice unicode pkg qual
  TyClD d@(DataDecl {})        -> ppDataDecl summ links instances fixities subdocs loc mbDoc d pats splice unicode pkg qual
  TyClD d@(SynDecl {})         -> ppTySyn summ links fixities loc (mbDoc, fnArgsDoc) d splice unicode pkg qual
  TyClD d@(ClassDecl {})       -> ppClassDecl summ links instances fixities loc mbDoc subdocs d splice unicode pkg qual
  SigD (TypeSig lnames lty)    -> ppLFunSig summ links loc (mbDoc, fnArgsDoc) lnames
                                         (hsSigWcType lty) fixities splice unicode pkg qual
  SigD (PatSynSig lnames ty)   -> ppLPatSig summ links loc (mbDoc, fnArgsDoc) lnames
                                         ty fixities splice unicode pkg qual
  ForD d                       -> ppFor summ links loc (mbDoc, fnArgsDoc) d fixities splice unicode pkg qual
  InstD _                      -> noHtml
  DerivD _                     -> noHtml
  _                            -> error "declaration not supported by ppDecl"


ppLFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             [Located DocName] -> LHsType DocNameI -> [(DocName, Fixity)] ->
             Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppLFunSig summary links loc doc lnames lty fixities splice unicode pkg qual =
  ppFunSig summary links loc doc (map unLoc lnames) lty fixities
           splice unicode pkg qual

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
            [DocName] -> LHsType DocNameI -> [(DocName, Fixity)] ->
            Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppFunSig summary links loc doc docnames typ fixities splice unicode pkg qual =
  ppSigLike summary links loc mempty doc docnames fixities (unLoc typ, pp_typ)
            splice unicode pkg qual HideEmptyContexts
  where
    pp_typ = ppLType unicode qual HideEmptyContexts typ

ppLPatSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             [Located DocName] -> LHsSigType DocNameI ->
             [(DocName, Fixity)] ->
             Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppLPatSig summary links loc (doc, _argDocs) docnames typ fixities splice
          unicode pkg qual
  | summary = pref1
  | otherwise = topDeclElem links loc splice (map unLoc docnames) (pref1 <+> ppFixities fixities qual)
                +++ docSection Nothing pkg qual doc
  where
    pref1 = hsep [ keyword "pattern"
                 , hsep $ punctuate comma $ map (ppBinder summary . getOccName) docnames
                 , dcolon unicode
                 , ppPatSigType unicode qual (hsSigType typ)
                 ]

ppSigLike :: Bool -> LinksInfo -> SrcSpan -> Html -> DocForDecl DocName ->
             [DocName] -> [(DocName, Fixity)] -> (HsType DocNameI, Html) ->
             Splice -> Unicode -> Maybe Package -> Qualification -> HideEmptyContexts -> Html
ppSigLike summary links loc leader doc docnames fixities (typ, pp_typ)
          splice unicode pkg qual emptyCtxts =
  ppTypeOrFunSig summary links loc docnames typ doc
    ( addFixities $ leader <+> ppTypeSig summary occnames pp_typ unicode
    , addFixities . concatHtml . punctuate comma $ map (ppBinder False) occnames
    , dcolon unicode
    )
    splice unicode pkg qual emptyCtxts
  where
    occnames = map (nameOccName . getName) docnames
    addFixities html
      | summary   = html
      | otherwise = html <+> ppFixities fixities qual


ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> [DocName] -> HsType DocNameI
               -> DocForDecl DocName -> (Html, Html, Html)
               -> Splice -> Unicode -> Maybe Package -> Qualification
               -> HideEmptyContexts -> Html
ppTypeOrFunSig summary links loc docnames typ (doc, argDocs) (pref1, pref2, sep)
               splice unicode pkg qual emptyCtxts
  | summary = pref1
  | Map.null argDocs = topDeclElem links loc splice docnames pref1 +++ docSection curName pkg qual doc
  | otherwise = topDeclElem links loc splice docnames pref2 +++
      subArguments pkg qual (do_args 0 sep typ) +++ docSection curName pkg qual doc
  where
    curName = getName <$> listToMaybe docnames
    argDoc n = Map.lookup n argDocs

    do_largs n leader (L _ t) = do_args n leader t

    do_args :: Int -> Html -> HsType DocNameI -> [SubDecl]
    do_args n leader (HsForAllTy tvs ltype)
      = do_largs n leader' ltype
      where
        leader' = leader <+> ppForAll tvs unicode qual

    do_args n leader (HsQualTy lctxt ltype)
      | null (unLoc lctxt)
      = do_largs n leader ltype
      | otherwise
      = (leader <+> ppLContextNoArrow lctxt unicode qual emptyCtxts, Nothing, [])
        : do_largs n (darrow unicode) ltype

    do_args n leader (HsFunTy lt r)
      = (leader <+> ppLFunLhType unicode qual emptyCtxts lt, argDoc n, [])
        : do_largs (n+1) (arrow unicode) r
    do_args n leader t
      = [(leader <+> ppType unicode qual emptyCtxts t, argDoc n, [])]

ppForAll :: [LHsTyVarBndr DocNameI] -> Unicode -> Qualification -> Html
ppForAll tvs unicode qual =
  case [ppKTv n k | L _ (KindedTyVar (L _ n) k) <- tvs] of
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
      -> Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppFor summary links loc doc (ForeignImport (L _ name) typ _ _) fixities
      splice unicode pkg qual
  = ppFunSig summary links loc doc [name] (hsSigType typ) fixities splice unicode pkg qual
ppFor _ _ _ _ _ _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> [(DocName, Fixity)] -> SrcSpan
        -> DocForDecl DocName -> TyClDecl DocNameI
        -> Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppTySyn summary links fixities loc doc (SynDecl { tcdLName = L _ name, tcdTyVars = ltyvars
                                                , tcdRhs = ltype })
        splice unicode pkg qual
  = ppTypeOrFunSig summary links loc [name] (unLoc ltype) doc
                   (full <+> fixs, hdr <+> fixs, spaceHtml +++ equals)
                   splice unicode pkg qual ShowEmptyToplevelContexts
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ]
                 ++ ppTyVars unicode qual (hsQTvExplicit ltyvars))
    full = hdr <+> equals <+> ppPatSigType unicode qual ltype
    occ  = nameOccName . getName $ name
    fixs
      | summary   = noHtml
      | otherwise = ppFixities fixities qual
ppTySyn _ _ _ _ _ _ _ _ _ _ = error "declaration not supported by ppTySyn"


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

ppResultSig :: FamilyResultSig DocNameI -> Unicode -> Qualification -> Html
ppResultSig result unicode qual = case result of
    NoSig               -> noHtml
    KindSig kind        -> dcolon unicode  <+> ppLKind unicode qual kind
    TyVarSig (L _ bndr) -> equals <+> ppHsTyVarBndr unicode qual bndr

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
           FamilyDecl DocNameI -> Splice -> Unicode -> Maybe Package ->
           Qualification -> Html
ppTyFam summary associated links instances fixities loc doc decl splice unicode
        pkg qual

  | summary   = ppTyFamHeader True associated decl unicode qual
  | otherwise = header_ +++ docSection Nothing pkg qual doc +++ instancesBit

  where
    docname = unLoc $ fdLName decl

    header_ = topDeclElem links loc splice [docname] $
       ppTyFamHeader summary associated decl unicode qual <+> ppFixities fixities qual

    instancesBit
      | FamilyDecl { fdInfo = ClosedTypeFamily mb_eqns } <- decl
      , not summary
      = subEquations pkg qual $ map (ppTyFamEqn . unLoc) $ fromMaybe [] mb_eqns

      | otherwise
      = ppInstances links (OriginFamily docname) instances splice unicode pkg qual

    -- Individual equation of a closed type family
    ppTyFamEqn :: TyFamInstEqn DocNameI -> SubDecl
    ppTyFamEqn (HsIB { hsib_body = FamEqn { feqn_tycon = n, feqn_rhs = rhs
                                          , feqn_pats = ts } })
      = ( ppAppNameTypes (unLoc n) (map unLoc ts) unicode qual
          <+> equals <+> ppType unicode qual HideEmptyContexts (unLoc rhs)
        , Nothing, [] )



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
            -> [(DocName, Fixity)] -> Splice -> Unicode -> Maybe Package
            -> Qualification -> Html
ppAssocType summ links doc (L loc decl) fixities splice unicode pkg qual =
   ppTyFam summ True links [] fixities loc (fst doc) decl splice unicode pkg qual


--------------------------------------------------------------------------------
-- * TyClDecl helpers
--------------------------------------------------------------------------------

-- | Print a type family and its variables
ppFamDeclBinderWithVars :: Bool -> Unicode -> Qualification -> FamilyDecl DocNameI -> Html
ppFamDeclBinderWithVars summ unicode qual (FamilyDecl { fdLName = lname, fdTyVars = tvs }) =
  ppAppDocNameTyVarBndrs summ unicode qual (unLoc lname) (hsq_explicit tvs)

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
                 -> Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppShortClassDecl summary links (ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = tvs
                                          , tcdFDs = fds, tcdSigs = sigs, tcdATs = ats }) loc
    subdocs splice unicode pkg qual =
  if not (any isUserLSig sigs) && null ats
    then (if summary then id else topDeclElem links loc splice [nm]) hdr
    else (if summary then id else topDeclElem links loc splice [nm]) (hdr <+> keyword "where")
      +++ shortSubDecls False
          (
            [ ppAssocType summary links doc at [] splice unicode pkg qual | at <- ats
              , let doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs ]  ++

                -- ToDo: add associated type defaults

            [ ppFunSig summary links loc doc names (hsSigWcType typ)
                       [] splice unicode pkg qual
              | L _ (TypeSig lnames typ) <- sigs
              , let doc = lookupAnySubdoc (head names) subdocs
                    names = map unLoc lnames ]
              -- FIXME: is taking just the first name ok? Is it possible that
              -- there are different subdocs for different names in a single
              -- type signature?
          )
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds unicode qual
    nm  = unLoc lname
ppShortClassDecl _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"



ppClassDecl :: Bool -> LinksInfo -> [DocInstance DocNameI] -> [(DocName, Fixity)]
            -> SrcSpan -> Documentation DocName
            -> [(DocName, DocForDecl DocName)] -> TyClDecl DocNameI
            -> Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppClassDecl summary links instances fixities loc d subdocs
        decl@(ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = ltyvars
                        , tcdFDs = lfds, tcdSigs = lsigs, tcdATs = ats })
            splice unicode pkg qual
  | summary = ppShortClassDecl summary links decl loc subdocs splice unicode pkg qual
  | otherwise = classheader +++ docSection Nothing pkg qual d
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
    atBit = subAssociatedTypes [ ppAssocType summary links doc at subfixs splice unicode pkg qual
                      | at <- ats
                      , let n = unL . fdLName $ unL at
                            doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs
                            subfixs = [ f | f@(n',_) <- fixities, n == n' ] ]

    methodBit = subMethods [ ppFunSig summary links loc doc [name] (hsSigType typ)
                                      subfixs splice unicode pkg qual
                           | L _ (ClassOpSig _ lnames typ) <- lsigs
                           , name <- map unLoc lnames
                           , let doc = lookupAnySubdoc name subdocs
                                 subfixs = [ f | f@(n',_) <- fixities
                                               , name == n' ]
                           ]
                           -- N.B. taking just the first name is ok. Signatures with multiple names
                           -- are expanded so that each name gets its own signature.

    minimalBit = case [ s | MinimalSig _ (L _ s) <- sigs ] of
      -- Miminal complete definition = every shown method
      And xs : _ | sort [getName n | L _ (Var (L _ n)) <- xs] ==
                   sort [getName n | TypeSig ns _ <- sigs, L _ n <- ns]
        -> noHtml

      -- Minimal complete definition = the only shown method
      Var (L _ n) : _ | [getName n] ==
                        [getName n' | L _ (TypeSig ns _) <- lsigs, L _ n' <- ns]
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
        splice unicode pkg qual

ppClassDecl _ _ _ _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstances :: LinksInfo
            -> InstOrigin DocName -> [DocInstance DocNameI]
            -> Splice -> Unicode -> Maybe Package -> Qualification
            -> Html
ppInstances links origin instances splice unicode pkg qual
  = subInstances pkg qual instName links True (zipWith instDecl [1..] instances)
  -- force Splice = True to use line URLs
  where
    instName = getOccString origin
    instDecl :: Int -> DocInstance DocNameI -> (SubDecl,Located DocName)
    instDecl no (inst, mdoc, loc, mdl) =
        ((ppInstHead links splice unicode qual mdoc origin False no inst mdl), loc)


ppOrphanInstances :: LinksInfo
                  -> [DocInstance DocNameI]
                  -> Splice -> Unicode -> Maybe Package -> Qualification
                  -> Html
ppOrphanInstances links instances splice unicode pkg qual
  = subOrphanInstances pkg qual links True (zipWith instDecl [1..] instances)
  where
    instOrigin :: InstHead name -> InstOrigin (IdP name)
    instOrigin inst = OriginClass (ihdClsName inst)

    instDecl :: Int -> DocInstance DocNameI -> (SubDecl,Located DocName)
    instDecl no (inst, mdoc, loc, mdl) =
        ((ppInstHead links splice unicode qual mdoc (instOrigin inst) True no inst mdl), loc)


ppInstHead :: LinksInfo -> Splice -> Unicode -> Qualification
           -> Maybe (MDoc DocName)
           -> InstOrigin DocName
           -> Bool -- ^ Is instance orphan
           -> Int  -- ^ Normal
           -> InstHead DocNameI
           -> Maybe Module
           -> SubDecl
ppInstHead links splice unicode qual mdoc origin orphan no ihd@(InstHead {..}) mdl =
    case ihdInstType of
        ClassInst { .. } ->
            ( subInstHead iid $ ppContextNoLocs clsiCtx unicode qual HideEmptyContexts <+> typ
            , mdoc
            , [subInstDetails iid ats sigs mname]
            )
          where
            sigs = ppInstanceSigs links splice unicode qual clsiSigs
            ats = ppInstanceAssocTys links splice unicode qual clsiAssocTys
        TypeInst rhs ->
            ( subInstHead iid ptype
            , mdoc
            , [subFamInstDetails iid prhs mname]
            )
          where
            ptype = keyword "type" <+> typ
            prhs = ptype <+> maybe noHtml
                                   (\t -> equals <+> ppType unicode qual HideEmptyContexts t) rhs
        DataInst dd ->
            ( subInstHead iid pdata
            , mdoc
            , [subFamInstDetails iid pdecl mname])
          where
            pdata = keyword "data" <+> typ
            pdecl = pdata <+> ppShortDataDecl False True dd [] unicode qual
  where
    mname = maybe noHtml (\m -> toHtml "Defined in" <+> ppModule m) mdl
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
    TypeSig lnames typ <- sigs
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

    pats1 = [ hsep [ keyword "pattern"
                   , hsep $ punctuate comma $ map (ppBinder summary . getOccName) lnames
                   , dcolon unicode
                   , ppPatSigType unicode qual (hsSigType typ)
                   ]
            | (SigD (PatSynSig lnames typ),_) <- pats
            ]


ppDataDecl :: Bool -> LinksInfo -> [DocInstance DocNameI] -> [(DocName, Fixity)] ->
              [(DocName, DocForDecl DocName)] ->
              SrcSpan -> Documentation DocName -> TyClDecl DocNameI ->
              [(HsDecl DocNameI, DocForDecl DocName)] ->
              Splice -> Unicode -> Maybe Package -> Qualification -> Html
ppDataDecl summary links instances fixities subdocs loc doc dataDecl pats
           splice unicode pkg qual

  | summary   = ppShortDataDecl summary False dataDecl pats unicode qual
  | otherwise = header_ +++ docSection Nothing pkg qual doc +++ constrBit +++ patternBit +++ instancesBit

  where
    docname   = tcdName dataDecl
    cons      = dd_cons (tcdDataDefn dataDecl)
    isH98     = case unLoc (head cons) of
                  ConDeclH98 {} -> True
                  ConDeclGADT{} -> False

    header_ = topDeclElem links loc splice [docname] $
             ppDataHeader summary dataDecl unicode qual <+> whereBit <+> fix

    fix = ppFixities (filter (\(n,_) -> n == docname) fixities) qual

    whereBit
      | null cons
      , null pats = noHtml
      | null cons = keyword "where"
      | otherwise = if isH98 then noHtml else keyword "where"

    constrBit = subConstructors pkg qual
      [ ppSideBySideConstr subdocs subfixs unicode pkg qual c
      | c <- cons
      , let subfixs = filter (\(n,_) -> any (\cn -> cn == n)
                                     (map unLoc (getConNames (unLoc c)))) fixities
      ]

    patternBit = subPatterns pkg qual
      [ (hsep [ keyword "pattern"
              , hsep $ punctuate comma $ map (ppBinder summary . getOccName) lnames
              , dcolon unicode
              , ppPatSigType unicode qual (hsSigType typ)
              ] <+> ppFixities subfixs qual
        ,combineDocumentation (fst d), [])
      | (SigD (PatSynSig lnames typ),d) <- pats
      , let subfixs = filter (\(n,_) -> any (\cn -> cn == n) (map unLoc lnames)) fixities
      ]

    instancesBit = ppInstances links (OriginData docname) instances
        splice unicode pkg qual



ppShortConstr :: Bool -> ConDecl DocNameI -> Unicode -> Qualification -> Html
ppShortConstr summary con unicode qual = cHead <+> cBody <+> cFoot
  where
    (cHead,cBody,cFoot) = ppShortConstrParts summary False con unicode qual


-- returns three pieces: header, body, footer so that header & footer can be
-- incorporated into the declaration
ppShortConstrParts :: Bool -> Bool -> ConDecl DocNameI -> Unicode -> Qualification -> (Html, Html, Html)
ppShortConstrParts summary dataInst con unicode qual = case con of
  ConDeclH98{} -> case con_details con of
    PrefixCon args ->
      (header_ unicode qual +++ hsep (ppOcc
            : map (ppLParendType unicode qual HideEmptyContexts) args), noHtml, noHtml)
    RecCon (L _ fields) ->
      (header_ unicode qual +++ ppOcc <+> char '{',
       doRecordFields fields,
       char '}')
    InfixCon arg1 arg2 ->
      (header_ unicode qual +++ hsep [ppLParendType unicode qual HideEmptyContexts arg1,
            ppOccInfix, ppLParendType unicode qual HideEmptyContexts arg2],
       noHtml, noHtml)

  ConDeclGADT {} -> (ppOcc <+> dcolon unicode <+> ppLType unicode qual HideEmptyContexts resTy,noHtml,noHtml)

  where
    resTy = hsib_body (con_type con)

    doRecordFields fields = shortSubDecls dataInst (map (ppShortField summary unicode qual) (map unLoc fields))

    header_  = ppConstrHdr forall_ tyVars context
    occ        = map (nameOccName . getName . unLoc) $ getConNames con

    ppOcc      = case occ of
      [one] -> ppBinder summary one
      _     -> hsep (punctuate comma (map (ppBinder summary) occ))

    ppOccInfix = case occ of
      [one] -> ppBinderInfix summary one
      _     -> hsep (punctuate comma (map (ppBinderInfix summary) occ))

    ltvs     = fromMaybe (HsQTvs PlaceHolder [] PlaceHolder) (con_qvars con)
    tyVars   = tyvarNames ltvs
    lcontext = fromMaybe (noLoc []) (con_cxt con)
    context  = unLoc lcontext
    forall_  = False


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr :: Bool -> [Name] -> HsContext DocNameI -> Unicode
            -> Qualification -> Html
ppConstrHdr forall_ tvs ctxt unicode qual
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml
    else ppContextNoArrow ctxt unicode qual HideEmptyContexts
         <+> darrow unicode +++ toHtml " ")
  where
    ppForall | forall_   = forallSymbol unicode <+> hsep (map (ppName Prefix) tvs)
                           <+> toHtml ". "
             | otherwise = noHtml

ppSideBySideConstr :: [(DocName, DocForDecl DocName)] -> [(DocName, Fixity)]
                   -> Unicode -> Maybe Package -> Qualification -> LConDecl DocNameI -> SubDecl
ppSideBySideConstr subdocs fixities unicode pkg qual (L _ con)
 = (decl, mbDoc, fieldPart)
 where
    decl = case con of
      ConDeclH98{} -> case con_details con of
        PrefixCon args ->
          hsep ((header_ +++ ppOcc)
            : map (ppLParendType unicode qual HideEmptyContexts) args)
          <+> fixity

        RecCon _ -> header_ +++ ppOcc <+> fixity

        InfixCon arg1 arg2 ->
          hsep [header_ +++ ppLParendType unicode qual HideEmptyContexts arg1,
            ppOccInfix,
            ppLParendType unicode qual HideEmptyContexts arg2]
          <+> fixity

      ConDeclGADT{} -> doGADTCon resTy

    resTy = hsib_body (con_type con)

    fieldPart = case getConDetails con of
        RecCon (L _ fields) -> [doRecordFields fields]
        _ -> []

    doRecordFields fields = subFields pkg qual
      (map (ppSideBySideField subdocs unicode qual) (map unLoc fields))

    doGADTCon :: Located (HsType DocNameI) -> Html
    doGADTCon ty = ppOcc <+> dcolon unicode
        -- ++AZ++ make this prepend "{..}" when it is a record style GADT
        <+> ppLType unicode qual HideEmptyContexts ty
        <+> fixity

    fixity  = ppFixities fixities qual
    header_ = ppConstrHdr forall_ tyVars context unicode qual
    occ       = map (nameOccName . getName . unLoc) $ getConNames con

    ppOcc     = case occ of
      [one] -> ppBinder False one
      _     -> hsep (punctuate comma (map (ppBinder False) occ))

    ppOccInfix = case occ of
      [one] -> ppBinderInfix False one
      _     -> hsep (punctuate comma (map (ppBinderInfix False) occ))

    tyVars  = tyvarNames (fromMaybe (HsQTvs PlaceHolder [] PlaceHolder) (con_qvars con))
    context = unLoc (fromMaybe (noLoc []) (con_cxt con))
    forall_ = False
    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc = lookup (unLoc $ head $ getConNames con) subdocs >>=
            combineDocumentation . fst


ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Unicode -> Qualification
                  -> ConDeclField DocNameI -> SubDecl
ppSideBySideField subdocs unicode qual (ConDeclField names ltype _) =
  ( hsep (punctuate comma (map ((ppBinder False) . rdrNameOcc . unLoc . rdrNameFieldOcc . unLoc) names))
      <+> dcolon unicode
      <+> ppLType unicode qual HideEmptyContexts ltype
  , mbDoc
  , []
  )
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    -- Where there is more than one name, they all have the same documentation
    mbDoc = lookup (selectorFieldOcc $ unLoc $ head names) subdocs >>= combineDocumentation . fst


ppShortField :: Bool -> Unicode -> Qualification -> ConDeclField DocNameI -> Html
ppShortField summary unicode qual (ConDeclField names ltype _)
  = hsep (punctuate comma (map ((ppBinder summary) . rdrNameOcc . unLoc . rdrNameFieldOcc . unLoc) names))
    <+> dcolon unicode <+> ppLType unicode qual HideEmptyContexts ltype


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
ppHsTyVarBndr _       qual (UserTyVar (L _ name)) =
    ppDocName qual Raw False name
ppHsTyVarBndr unicode qual (KindedTyVar name kind) =
    parens (ppDocName qual Raw False (unLoc name) <+> dcolon unicode <+>
            ppLKind unicode qual kind)

ppLKind :: Unicode -> Qualification -> LHsKind DocNameI -> Html
ppLKind unicode qual y = ppKind unicode qual (unLoc y)

ppKind :: Unicode -> Qualification -> HsKind DocNameI -> Html
ppKind unicode qual ki = ppr_mono_ty pREC_TOP ki unicode qual HideEmptyContexts

ppPatSigType :: Unicode -> Qualification -> LHsType DocNameI -> Html
ppPatSigType unicode qual typ =
  let emptyCtxts =
        if hasNonEmptyContext typ && isFirstContextEmpty typ
          then ShowEmptyToplevelContexts
          else HideEmptyContexts
  in ppLType unicode qual emptyCtxts typ
  where
    hasNonEmptyContext :: LHsType name -> Bool
    hasNonEmptyContext t =
      case unLoc t of
        HsForAllTy _ s -> hasNonEmptyContext s
        HsQualTy cxt s -> if null (unLoc cxt) then hasNonEmptyContext s else True
        HsFunTy _ s -> hasNonEmptyContext s
        _ -> False
    isFirstContextEmpty :: LHsType name -> Bool
    isFirstContextEmpty t =
      case unLoc t of
        HsForAllTy _ s -> isFirstContextEmpty s
        HsQualTy cxt _ -> null (unLoc cxt)
        HsFunTy _ s -> isFirstContextEmpty s
        _ -> False

ppForAllPart :: Unicode -> Qualification -> [LHsTyVarBndr DocNameI] -> Html
ppForAllPart unicode qual tvs = hsep (forallSymbol unicode : ppTyVars unicode qual tvs) +++ dot

ppr_mono_lty :: Int -> LHsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)


ppr_mono_ty :: Int -> HsType DocNameI -> Unicode -> Qualification -> HideEmptyContexts -> Html
ppr_mono_ty ctxt_prec (HsForAllTy tvs ty) unicode qual emptyCtxts
  = maybeParen ctxt_prec pREC_FUN $
    ppForAllPart unicode qual tvs <+> ppr_mono_lty pREC_TOP ty unicode qual emptyCtxts

ppr_mono_ty ctxt_prec (HsQualTy ctxt ty) unicode qual emptyCtxts
  = maybeParen ctxt_prec pREC_FUN $
    ppLContext ctxt unicode qual emptyCtxts <+> ppr_mono_lty pREC_TOP ty unicode qual emptyCtxts

-- UnicodeSyntax alternatives
ppr_mono_ty _ (HsTyVar _ (L _ name)) True _ _
  | getOccString (getName name) == "*"    = toHtml "★"
  | getOccString (getName name) == "(->)" = toHtml "(→)"

ppr_mono_ty _         (HsBangTy b ty)     u q _ = ppBang b +++ ppLParendType u q HideEmptyContexts ty
ppr_mono_ty _         (HsTyVar _ (L _ name)) _ q _ = ppDocName q Prefix True name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   u q e = ppr_fun_ty ctxt_prec ty1 ty2 u q e
ppr_mono_ty _         (HsTupleTy con tys) u q _ = tupleParens con (map (ppLType u q HideEmptyContexts) tys)
ppr_mono_ty _         (HsSumTy tys) u q _ = sumParens (map (ppLType u q HideEmptyContexts) tys)
ppr_mono_ty _         (HsKindSig ty kind) u q e =
    parens (ppr_mono_lty pREC_TOP ty u q e <+> dcolon u <+> ppLKind u q kind)
ppr_mono_ty _         (HsListTy ty)       u q _ = brackets (ppr_mono_lty pREC_TOP ty u q HideEmptyContexts)
ppr_mono_ty _         (HsPArrTy ty)       u q _ = pabrackets (ppr_mono_lty pREC_TOP ty u q HideEmptyContexts)
ppr_mono_ty ctxt_prec (HsIParamTy (L _ n) ty) u q _ =
    maybeParen ctxt_prec pREC_CTX $ ppIPName n <+> dcolon u <+> ppr_mono_lty pREC_TOP ty u q HideEmptyContexts
ppr_mono_ty _         (HsSpliceTy {})     _ _ _ = error "ppr_mono_ty HsSpliceTy"
ppr_mono_ty _         (HsRecTy {})        _ _ _ = toHtml "{..}"
       -- Can now legally occur in ConDeclGADT, the output here is to provide a
       -- placeholder in the signature, which is followed by the field
       -- declarations.
ppr_mono_ty _         (HsCoreTy {})       _ _ _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty _         (HsExplicitListTy Promoted _ tys) u q _ = promoQuote $ brackets $ hsep $ punctuate comma $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty _         (HsExplicitListTy NotPromoted _ tys) u q _ = brackets $ hsep $ punctuate comma $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty _         (HsExplicitTupleTy _ tys) u q _ = promoQuote $ parenList $ map (ppLType u q HideEmptyContexts) tys
ppr_mono_ty _         (HsAppsTy {})       _ _ _ = error "ppr_mono_ty HsAppsTy"

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2) unicode qual _
  = maybeParen ctxt_prec pREC_CTX $
    ppr_mono_lty pREC_OP ty1 unicode qual HideEmptyContexts <+> char '~' <+> ppr_mono_lty pREC_OP ty2 unicode qual HideEmptyContexts

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode qual _
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode qual HideEmptyContexts, ppr_mono_lty pREC_CON arg_ty unicode qual HideEmptyContexts]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2) unicode qual _
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode qual HideEmptyContexts <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode qual HideEmptyContexts
  where
    -- `(:)` is valid in type signature only as constructor to promoted list
    -- and needs to be quoted in code so we explicitly quote it here too.
    ppr_op
        | (getOccString . getName . unLoc) op == ":" = promoQuote ppr_op'
        | otherwise = ppr_op'
    ppr_op' = ppLDocName qual Infix op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode qual emptyCtxts
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode qual emptyCtxts

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode qual emptyCtxts
  = ppr_mono_lty ctxt_prec ty unicode qual emptyCtxts

ppr_mono_ty _ (HsWildCardTy (AnonWildCard _)) _ _ _ = char '_'
ppr_mono_ty _ (HsTyLit n) _ _ _ = ppr_tylit n

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
