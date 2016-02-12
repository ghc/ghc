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

import GHC
import GHC.Exts
import Name
import BooleanFormula
import RdrName ( rdrNameOcc )

ppDecl :: Bool -> LinksInfo -> LHsDecl DocName
       -> DocForDecl DocName -> [DocInstance DocName] -> [(DocName, Fixity)]
       -> [(DocName, DocForDecl DocName)] -> Splice -> Unicode -> Qualification -> Html
ppDecl summ links (L loc decl) (mbDoc, fnArgsDoc) instances fixities subdocs splice unicode qual = case decl of
  TyClD (FamDecl d)         -> ppTyFam summ False links instances fixities loc mbDoc d splice unicode qual
  TyClD d@(DataDecl {})     -> ppDataDecl summ links instances fixities subdocs loc mbDoc d splice unicode qual
  TyClD d@(SynDecl {})      -> ppTySyn summ links fixities loc (mbDoc, fnArgsDoc) d splice unicode qual
  TyClD d@(ClassDecl {})    -> ppClassDecl summ links instances fixities loc mbDoc subdocs d splice unicode qual
  SigD (TypeSig lnames lty) -> ppLFunSig summ links loc (mbDoc, fnArgsDoc) lnames
                                         (hsSigWcType lty) fixities splice unicode qual
  SigD (PatSynSig lname ty) -> ppLPatSig summ links loc (mbDoc, fnArgsDoc) lname
                                         ty fixities splice unicode qual
  ForD d                         -> ppFor summ links loc (mbDoc, fnArgsDoc) d fixities splice unicode qual
  InstD _                        -> noHtml
  _                              -> error "declaration not supported by ppDecl"


ppLFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             [Located DocName] -> LHsType DocName -> [(DocName, Fixity)] ->
             Splice -> Unicode -> Qualification -> Html
ppLFunSig summary links loc doc lnames lty fixities splice unicode qual =
  ppFunSig summary links loc doc (map unLoc lnames) lty fixities
           splice unicode qual

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
            [DocName] -> LHsType DocName -> [(DocName, Fixity)] ->
            Splice -> Unicode -> Qualification -> Html
ppFunSig summary links loc doc docnames typ fixities splice unicode qual =
  ppSigLike summary links loc mempty doc docnames fixities (unLoc typ, pp_typ)
            splice unicode qual
  where
    pp_typ = ppLType unicode qual typ

ppLPatSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             Located DocName -> LHsSigType DocName ->
             [(DocName, Fixity)] ->
             Splice -> Unicode -> Qualification -> Html
ppLPatSig summary links loc (doc, _argDocs) (L _ name) typ fixities splice unicode qual
  | summary = pref1
  | otherwise = topDeclElem links loc splice [name] (pref1 <+> ppFixities fixities qual)
                +++ docSection Nothing qual doc
  where
    pref1 = hsep [ keyword "pattern"
                 , ppBinder summary occname
                 , dcolon unicode
                 , ppLType unicode qual (hsSigType typ)
                 ]

    occname = nameOccName . getName $ name

ppSigLike :: Bool -> LinksInfo -> SrcSpan -> Html -> DocForDecl DocName ->
             [DocName] -> [(DocName, Fixity)] -> (HsType DocName, Html) ->
             Splice -> Unicode -> Qualification -> Html
ppSigLike summary links loc leader doc docnames fixities (typ, pp_typ)
          splice unicode qual =
  ppTypeOrFunSig summary links loc docnames typ doc
    ( addFixities $ leader <+> ppTypeSig summary occnames pp_typ unicode
    , addFixities . concatHtml . punctuate comma $ map (ppBinder False) occnames
    , dcolon unicode
    )
    splice unicode qual
  where
    occnames = map (nameOccName . getName) docnames
    addFixities html
      | summary   = html
      | otherwise = html <+> ppFixities fixities qual


ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> [DocName] -> HsType DocName
               -> DocForDecl DocName -> (Html, Html, Html)
               -> Splice -> Unicode -> Qualification -> Html
ppTypeOrFunSig summary links loc docnames typ (doc, argDocs) (pref1, pref2, sep) splice unicode qual
  | summary = pref1
  | Map.null argDocs = topDeclElem links loc splice docnames pref1 +++ docSection curName qual doc
  | otherwise = topDeclElem links loc splice docnames pref2 +++
      subArguments qual (do_args 0 sep typ) +++ docSection curName qual doc
  where
    curName = getName <$> listToMaybe docnames
    argDoc n = Map.lookup n argDocs

    do_largs n leader (L _ t) = do_args n leader t

    do_args :: Int -> Html -> HsType DocName -> [SubDecl]
    do_args n leader (HsForAllTy tvs ltype)
      = do_largs n leader' ltype
      where
        leader' = leader <+> ppForAll tvs unicode qual

    do_args n leader (HsQualTy lctxt ltype)
      | null (unLoc lctxt)
      = do_largs n leader ltype
      | otherwise
      = (leader <+> ppLContextNoArrow lctxt unicode qual, Nothing, [])
        : do_largs n (darrow unicode) ltype

    do_args n leader (HsFunTy lt r)
      = (leader <+> ppLFunLhType unicode qual lt, argDoc n, [])
        : do_largs (n+1) (arrow unicode) r
    do_args n leader t
      = [(leader <+> ppType unicode qual t, argDoc n, [])]

ppForAll :: [LHsTyVarBndr DocName] -> Unicode -> Qualification -> Html
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
ppTyVars :: [LHsTyVarBndr DocName] -> [Html]
ppTyVars tvs = map (ppTyName . getName . hsLTyVarName) tvs

tyvarNames :: LHsQTyVars DocName -> [Name]
tyvarNames = map (getName . hsLTyVarName) . hsQTvExplicit


ppFor :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName
      -> ForeignDecl DocName -> [(DocName, Fixity)]
      -> Splice -> Unicode -> Qualification -> Html
ppFor summary links loc doc (ForeignImport (L _ name) typ _ _) fixities
      splice unicode qual
  = ppFunSig summary links loc doc [name] (hsSigType typ) fixities splice unicode qual
ppFor _ _ _ _ _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> [(DocName, Fixity)] -> SrcSpan
        -> DocForDecl DocName -> TyClDecl DocName
        -> Splice -> Unicode -> Qualification -> Html
ppTySyn summary links fixities loc doc (SynDecl { tcdLName = L _ name, tcdTyVars = ltyvars
                                                , tcdRhs = ltype })
        splice unicode qual
  = ppTypeOrFunSig summary links loc [name] (unLoc ltype) doc
                   (full <+> fixs, hdr <+> fixs, spaceHtml +++ equals)
                   splice unicode qual
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ]
                 ++ ppTyVars (hsQTvExplicit ltyvars))
    full = hdr <+> equals <+> ppLType unicode qual ltype
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


ppSimpleSig :: LinksInfo -> Splice -> Unicode -> Qualification -> SrcSpan
            -> [DocName] -> HsType DocName
            -> Html
ppSimpleSig links splice unicode qual loc names typ =
    topDeclElem' names $ ppTypeSig True occNames ppTyp unicode
  where
    topDeclElem' = topDeclElem links loc splice
    ppTyp = ppType unicode qual typ
    occNames = map getOccName names


--------------------------------------------------------------------------------
-- * Type families
--------------------------------------------------------------------------------


ppFamilyInfo :: Bool -> FamilyInfo DocName -> Html
ppFamilyInfo assoc OpenTypeFamily
    | assoc = keyword "type"
    | otherwise = keyword "type family"
ppFamilyInfo assoc DataFamily
    | assoc = keyword "data"
    | otherwise = keyword "data family"
ppFamilyInfo _ (ClosedTypeFamily _) = keyword "type family"


ppTyFamHeader :: Bool -> Bool -> FamilyDecl DocName
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

ppResultSig :: FamilyResultSig DocName -> Unicode -> Qualification -> Html
ppResultSig result unicode qual = case result of
    NoSig               -> noHtml
    KindSig kind        -> dcolon unicode  <+> ppLKind unicode qual kind
    TyVarSig (L _ bndr) -> equals <+> ppHsTyVarBndr unicode qual bndr

ppPseudoFamilyHeader :: Unicode -> Qualification -> PseudoFamilyDecl DocName
                     -> Html
ppPseudoFamilyHeader unicode qual (PseudoFamilyDecl { .. }) =
    ppFamilyInfo True pfdInfo <+>
    ppAppNameTypes (unLoc pfdLName) [] (map unLoc pfdTyVars) unicode qual <+>
    ppResultSig (unLoc pfdKindSig) unicode qual

ppInjectivityAnn :: Bool -> Qualification -> InjectivityAnn DocName -> Html
ppInjectivityAnn unicode qual (InjectivityAnn lhs rhs) =
    char '|' <+> ppLDocName qual Raw lhs <+> arrow unicode <+>
    hsep (map (ppLDocName qual Raw) rhs)


ppTyFam :: Bool -> Bool -> LinksInfo -> [DocInstance DocName] ->
           [(DocName, Fixity)] -> SrcSpan -> Documentation DocName ->
           FamilyDecl DocName -> Splice -> Unicode -> Qualification -> Html
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
    ppTyFamEqn TyFamEqn { tfe_tycon = n, tfe_rhs = rhs
                        , tfe_pats = HsIB { hsib_body = ts }}
      = ( ppAppNameTypes (unLoc n) [] (map unLoc ts) unicode qual
          <+> equals <+> ppType unicode qual (unLoc rhs)
        , Nothing, [] )



ppPseudoFamilyDecl :: LinksInfo -> Splice -> Unicode -> Qualification
                   -> PseudoFamilyDecl DocName
                   -> Html
ppPseudoFamilyDecl links splice unicode qual
                   decl@(PseudoFamilyDecl { pfdLName = L loc name, .. }) =
    wrapper $ ppPseudoFamilyHeader unicode qual decl
  where
    wrapper = topDeclElem links loc splice [name]


--------------------------------------------------------------------------------
-- * Associated Types
--------------------------------------------------------------------------------


ppAssocType :: Bool -> LinksInfo -> DocForDecl DocName -> LFamilyDecl DocName
            -> [(DocName, Fixity)] -> Splice -> Unicode -> Qualification -> Html
ppAssocType summ links doc (L loc decl) fixities splice unicode qual =
   ppTyFam summ True links [] fixities loc (fst doc) decl splice unicode qual


--------------------------------------------------------------------------------
-- * TyClDecl helpers
--------------------------------------------------------------------------------

-- | Print a type family and its variables
ppFamDeclBinderWithVars :: Bool -> Unicode -> Qualification -> FamilyDecl DocName -> Html
ppFamDeclBinderWithVars summ unicode qual (FamilyDecl { fdLName = lname, fdTyVars = tvs }) =
  ppAppDocNameTyVarBndrs summ unicode qual (unLoc lname) (map unLoc $ hsq_explicit tvs)

-- | Print a newtype / data binder and its variables
ppDataBinderWithVars :: Bool -> TyClDecl DocName -> Html
ppDataBinderWithVars summ decl =
  ppAppDocNameNames summ (tcdName decl) (tyvarNames $ tcdTyVars decl)

--------------------------------------------------------------------------------
-- * Type applications
--------------------------------------------------------------------------------

ppAppDocNameTyVarBndrs :: Bool -> Unicode -> Qualification -> DocName -> [HsTyVarBndr DocName] -> Html
ppAppDocNameTyVarBndrs summ unicode qual n vs =
    ppTypeApp n [] vs ppDN (ppHsTyVarBndr unicode qual)
  where
    ppDN notation = ppBinderFixity notation summ . nameOccName . getName
    ppBinderFixity Infix = ppBinderInfix
    ppBinderFixity _ = ppBinder

-- | Print an application of a 'DocName' and two lists of 'HsTypes' (kinds, types)
ppAppNameTypes :: DocName -> [HsType DocName] -> [HsType DocName]
               -> Unicode -> Qualification -> Html
ppAppNameTypes n ks ts unicode qual =
    ppTypeApp n ks ts (\p -> ppDocName qual p True) (ppParendType unicode qual)


-- | Print an application of a 'DocName' and a list of 'Names'
ppAppDocNameNames :: Bool -> DocName -> [Name] -> Html
ppAppDocNameNames summ n ns =
    ppTypeApp n [] ns ppDN ppTyName
  where
    ppDN notation = ppBinderFixity notation summ . nameOccName . getName
    ppBinderFixity Infix = ppBinderInfix
    ppBinderFixity _ = ppBinder

-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> [a] -> (Notation -> DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n [] (t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN Infix n <+> ppT t2

ppTypeApp n ks ts ppDN ppT = ppDN Prefix n <+> hsep (map ppT $ ks ++ ts)


-------------------------------------------------------------------------------
-- * Contexts
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Unicode
                              -> Qualification -> Html
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc

ppContextNoArrow :: HsContext DocName -> Unicode -> Qualification -> Html
ppContextNoArrow cxt unicode qual = fromMaybe noHtml $
                                    ppContextNoLocsMaybe (map unLoc cxt) unicode qual


ppContextNoLocs :: [HsType DocName] -> Unicode -> Qualification -> Html
ppContextNoLocs cxt unicode qual = maybe noHtml (<+> darrow unicode) $
                                   ppContextNoLocsMaybe cxt unicode qual


ppContextNoLocsMaybe :: [HsType DocName] -> Unicode -> Qualification -> Maybe Html
ppContextNoLocsMaybe []  _       _    = Nothing
ppContextNoLocsMaybe cxt unicode qual = Just $ ppHsContext cxt unicode qual

ppContext :: HsContext DocName -> Unicode -> Qualification -> Html
ppContext cxt unicode qual = ppContextNoLocs (map unLoc cxt) unicode qual


ppHsContext :: [HsType DocName] -> Unicode -> Qualification-> Html
ppHsContext []  _       _     = noHtml
ppHsContext [p] unicode qual = ppCtxType unicode qual p
ppHsContext cxt unicode qual = parenList (map (ppType unicode qual) cxt)


-------------------------------------------------------------------------------
-- * Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsType DocName] -> DocName
           -> LHsQTyVars DocName -> [Located ([Located DocName], [Located DocName])]
           -> Unicode -> Qualification -> Html
ppClassHdr summ lctxt n tvs fds unicode qual =
  keyword "class"
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode qual else noHtml)
  <+> ppAppDocNameNames summ n (tyvarNames tvs)
  <+> ppFds fds unicode qual


ppFds :: [Located ([Located DocName], [Located DocName])] -> Unicode -> Qualification -> Html
ppFds fds unicode qual =
  if null fds then noHtml else
        char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
        fundep (vars1,vars2) = ppVars vars1 <+> arrow unicode <+> ppVars vars2
        ppVars = hsep . map ((ppDocName qual Prefix True) . unLoc)

ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan
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
ppShortClassDecl _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"



ppClassDecl :: Bool -> LinksInfo -> [DocInstance DocName] -> [(DocName, Fixity)]
            -> SrcSpan -> Documentation DocName
            -> [(DocName, DocForDecl DocName)] -> TyClDecl DocName
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
                           | L _ (ClassOpSig _ lnames typ) <- lsigs
                           , let doc = lookupAnySubdoc (head names) subdocs
                                 subfixs = [ f | n <- names
                                               , f@(n',_) <- fixities
                                               , n == n' ]
                                 names = map unLoc lnames ]
                           -- FIXME: is taking just the first name ok? Is it possible that
                           -- there are different subdocs for different names in a single
                           -- type signature?

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
        splice unicode qual

ppClassDecl _ _ _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstances :: LinksInfo
            -> InstOrigin DocName -> [DocInstance DocName]
            -> Splice -> Unicode -> Qualification
            -> Html
ppInstances links origin instances splice unicode qual
  = subInstances qual instName links True (zipWith instDecl [1..] instances)
  -- force Splice = True to use line URLs
  where
    instName = getOccString origin
    instDecl :: Int -> DocInstance DocName -> (SubDecl,Located DocName)
    instDecl no (inst, mdoc, loc) =
        ((ppInstHead links splice unicode qual mdoc origin False no inst), loc)


ppOrphanInstances :: LinksInfo
                  -> [DocInstance DocName]
                  -> Splice -> Unicode -> Qualification
                  -> Html
ppOrphanInstances links instances splice unicode qual
  = subOrphanInstances qual links True (zipWith instDecl [1..] instances)
  where
    instOrigin :: InstHead name -> InstOrigin name
    instOrigin inst = OriginClass (ihdClsName inst)

    instDecl :: Int -> DocInstance DocName -> (SubDecl,Located DocName)
    instDecl no (inst, mdoc, loc) =
        ((ppInstHead links splice unicode qual mdoc (instOrigin inst) True no inst), loc)


ppInstHead :: LinksInfo -> Splice -> Unicode -> Qualification
           -> Maybe (MDoc DocName)
           -> InstOrigin DocName
           -> Bool -- ^ Is instance orphan
           -> Int  -- ^ Normal
           -> InstHead DocName
           -> SubDecl
ppInstHead links splice unicode qual mdoc origin orphan no ihd@(InstHead {..}) =
    case ihdInstType of
        ClassInst { .. } ->
            ( subInstHead iid $ ppContextNoLocs clsiCtx unicode qual <+> typ
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
                                   (\t -> equals <+> ppType unicode qual t) rhs
        DataInst dd ->
            ( subInstHead iid pdata
            , mdoc
            , [subFamInstDetails iid pdecl])
          where
            pdata = keyword "data" <+> typ
            pdecl = pdata <+> ppShortDataDecl False True dd unicode qual
  where
    iid = instanceId origin no orphan ihd
    typ = ppAppNameTypes ihdClsName ihdKinds ihdTypes unicode qual


ppInstanceAssocTys :: LinksInfo -> Splice -> Unicode -> Qualification
                   -> [PseudoFamilyDecl DocName]
                   -> [Html]
ppInstanceAssocTys links splice unicode qual =
    map ppFamilyDecl'
  where
    ppFamilyDecl' = ppPseudoFamilyDecl links splice unicode qual


ppInstanceSigs :: LinksInfo -> Splice -> Unicode -> Qualification
              -> [Sig DocName]
              -> [Html]
ppInstanceSigs links splice unicode qual sigs = do
    TypeSig lnames typ <- sigs
    let names = map unLoc lnames
        L loc rtyp = get_type typ
    return $ ppSimpleSig links splice unicode qual loc names rtyp
    where
      get_type = hswc_body . hsib_body


lookupAnySubdoc :: Eq id1 => id1 -> [(id1, DocForDecl id2)] -> DocForDecl id2
lookupAnySubdoc n = fromMaybe noDocForDecl . lookup n


instanceId :: InstOrigin DocName -> Int -> Bool -> InstHead DocName -> String
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
ppShortDataDecl :: Bool -> Bool -> TyClDecl DocName -> Unicode -> Qualification -> Html
ppShortDataDecl summary dataInst dataDecl unicode qual

  | [] <- cons = dataHeader

  | [lcon] <- cons, isH98,
    (cHead,cBody,cFoot) <- ppShortConstrParts summary dataInst (unLoc lcon) unicode qual
       = (dataHeader <+> equals <+> cHead) +++ cBody +++ cFoot

  | isH98 = dataHeader
      +++ shortSubDecls dataInst (zipWith doConstr ('=':repeat '|') cons)

  | otherwise = (dataHeader <+> keyword "where")
      +++ shortSubDecls dataInst (map doGADTConstr cons)

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


ppDataDecl :: Bool -> LinksInfo -> [DocInstance DocName] -> [(DocName, Fixity)] ->
              [(DocName, DocForDecl DocName)] ->
              SrcSpan -> Documentation DocName -> TyClDecl DocName ->
              Splice -> Unicode -> Qualification -> Html
ppDataDecl summary links instances fixities subdocs loc doc dataDecl
           splice unicode qual

  | summary   = ppShortDataDecl summary False dataDecl unicode qual
  | otherwise = header_ +++ docSection Nothing qual doc +++ constrBit +++ instancesBit

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
      | null cons = noHtml
      | otherwise = if isH98 then noHtml else keyword "where"

    constrBit = subConstructors qual
      [ ppSideBySideConstr subdocs subfixs unicode qual c
      | c <- cons
      , let subfixs = filter (\(n,_) -> any (\cn -> cn == n)
                                     (map unLoc (getConNames (unLoc c)))) fixities
      ]

    instancesBit = ppInstances links (OriginData docname) instances
        splice unicode qual



ppShortConstr :: Bool -> ConDecl DocName -> Unicode -> Qualification -> Html
ppShortConstr summary con unicode qual = cHead <+> cBody <+> cFoot
  where
    (cHead,cBody,cFoot) = ppShortConstrParts summary False con unicode qual


-- returns three pieces: header, body, footer so that header & footer can be
-- incorporated into the declaration
ppShortConstrParts :: Bool -> Bool -> ConDecl DocName -> Unicode -> Qualification -> (Html, Html, Html)
ppShortConstrParts summary dataInst con unicode qual = case con of
  ConDeclH98{} -> case con_details con of
    PrefixCon args ->
      (header_ unicode qual +++ hsep (ppOcc
            : map (ppLParendType unicode qual) args), noHtml, noHtml)
    RecCon (L _ fields) ->
      (header_ unicode qual +++ ppOcc <+> char '{',
       doRecordFields fields,
       char '}')
    InfixCon arg1 arg2 ->
      (header_ unicode qual +++ hsep [ppLParendType unicode qual arg1,
            ppOccInfix, ppLParendType unicode qual arg2],
       noHtml, noHtml)

  ConDeclGADT {} -> (ppOcc <+> dcolon unicode <+> ppLType unicode qual resTy,noHtml,noHtml)

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
ppConstrHdr :: Bool -> [Name] -> HsContext DocName -> Unicode
            -> Qualification -> Html
ppConstrHdr forall_ tvs ctxt unicode qual
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml
    else ppContextNoArrow ctxt unicode qual
         <+> darrow unicode +++ toHtml " ")
  where
    ppForall | forall_   = forallSymbol unicode <+> hsep (map (ppName Prefix) tvs)
                           <+> toHtml ". "
             | otherwise = noHtml

ppSideBySideConstr :: [(DocName, DocForDecl DocName)] -> [(DocName, Fixity)]
                   -> Unicode -> Qualification -> LConDecl DocName -> SubDecl
ppSideBySideConstr subdocs fixities unicode qual (L _ con)
 = (decl, mbDoc, fieldPart)
 where
    decl = case con of
      ConDeclH98{} -> case con_details con of
        PrefixCon args ->
          hsep ((header_ +++ ppOcc)
            : map (ppLParendType unicode qual) args)
          <+> fixity

        RecCon _ -> header_ +++ ppOcc <+> fixity

        InfixCon arg1 arg2 ->
          hsep [header_ +++ ppLParendType unicode qual arg1,
            ppOccInfix,
            ppLParendType unicode qual arg2]
          <+> fixity

      ConDeclGADT{} -> doGADTCon resTy

    resTy = hsib_body (con_type con)

    fieldPart = case getConDetails con of
        RecCon (L _ fields) -> [doRecordFields fields]
        _ -> []

    doRecordFields fields = subFields qual
      (map (ppSideBySideField subdocs unicode qual) (map unLoc fields))

    doGADTCon :: Located (HsType DocName) -> Html
    doGADTCon ty = ppOcc <+> dcolon unicode
        -- ++AZ++ make this prepend "{..}" when it is a record style GADT
        <+> ppLType unicode qual ty
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
                  -> ConDeclField DocName -> SubDecl
ppSideBySideField subdocs unicode qual (ConDeclField names ltype _) =
  (hsep (punctuate comma (map ((ppBinder False) . rdrNameOcc . unLoc . rdrNameFieldOcc . unLoc) names)) <+> dcolon unicode <+> ppLType unicode qual ltype,
    mbDoc,
    [])
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    -- Where there is more than one name, they all have the same documentation
    mbDoc = lookup (selectorFieldOcc $ unLoc $ head names) subdocs >>= combineDocumentation . fst


ppShortField :: Bool -> Unicode -> Qualification -> ConDeclField DocName -> Html
ppShortField summary unicode qual (ConDeclField names ltype _)
  = hsep (punctuate comma (map ((ppBinder summary) . rdrNameOcc . unLoc . rdrNameFieldOcc . unLoc) names))
    <+> dcolon unicode <+> ppLType unicode qual ltype


-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocName -> Unicode -> Qualification -> Html
ppDataHeader summary decl@(DataDecl { tcdDataDefn =
                                         HsDataDefn { dd_ND = nd
                                                    , dd_ctxt = ctxt
                                                    , dd_kindSig = ks } })
             unicode qual
  = -- newtype or data
    (case nd of { NewType -> keyword "newtype"; DataType -> keyword "data" })
    <+>
    -- context
    ppLContext ctxt unicode qual <+>
    -- T a b c ..., or a :+: b
    ppDataBinderWithVars summary decl
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


ppLType, ppLParendType, ppLFunLhType :: Unicode -> Qualification
                                     -> Located (HsType DocName) -> Html
ppLType       unicode qual y = ppType unicode qual (unLoc y)
ppLParendType unicode qual y = ppParendType unicode qual (unLoc y)
ppLFunLhType  unicode qual y = ppFunLhType unicode qual (unLoc y)


ppType, ppCtxType, ppParendType, ppFunLhType :: Unicode -> Qualification
                                             -> HsType DocName -> Html
ppType       unicode qual ty = ppr_mono_ty pREC_TOP ty unicode qual
ppCtxType    unicode qual ty = ppr_mono_ty pREC_CTX ty unicode qual
ppParendType unicode qual ty = ppr_mono_ty pREC_CON ty unicode qual
ppFunLhType  unicode qual ty = ppr_mono_ty pREC_FUN ty unicode qual

ppHsTyVarBndr :: Unicode -> Qualification -> HsTyVarBndr DocName -> Html
ppHsTyVarBndr _       qual (UserTyVar (L _ name)) =
    ppDocName qual Raw False name
ppHsTyVarBndr unicode qual (KindedTyVar name kind) =
    parens (ppDocName qual Raw False (unLoc name) <+> dcolon unicode <+>
            ppLKind unicode qual kind)

ppLKind :: Unicode -> Qualification -> LHsKind DocName -> Html
ppLKind unicode qual y = ppKind unicode qual (unLoc y)

ppKind :: Unicode -> Qualification -> HsKind DocName -> Html
ppKind unicode qual ki = ppr_mono_ty pREC_TOP ki unicode qual

ppForAllPart :: [LHsTyVarBndr DocName] -> Unicode -> Html
ppForAllPart tvs unicode = hsep (forallSymbol unicode : ppTyVars tvs) +++ dot

ppr_mono_lty :: Int -> LHsType DocName -> Unicode -> Qualification -> Html
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)


ppr_mono_ty :: Int -> HsType DocName -> Unicode -> Qualification -> Html
ppr_mono_ty ctxt_prec (HsForAllTy tvs ty) unicode qual
  = maybeParen ctxt_prec pREC_FUN $
    ppForAllPart tvs unicode <+> ppr_mono_lty pREC_TOP ty unicode qual

ppr_mono_ty ctxt_prec (HsQualTy ctxt ty) unicode qual
  = maybeParen ctxt_prec pREC_FUN $
    ppLContext ctxt unicode qual <+> ppr_mono_lty pREC_TOP ty unicode qual

-- UnicodeSyntax alternatives
ppr_mono_ty _ (HsTyVar (L _ name)) True _
  | getOccString (getName name) == "*"    = toHtml "★"
  | getOccString (getName name) == "(->)" = toHtml "(→)"

ppr_mono_ty _         (HsBangTy b ty)     u q = ppBang b +++ ppLParendType u q ty
ppr_mono_ty _         (HsTyVar (L _ name)) _ q = ppDocName q Prefix True name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   u q = ppr_fun_ty ctxt_prec ty1 ty2 u q
ppr_mono_ty _         (HsTupleTy con tys) u q = tupleParens con (map (ppLType u q) tys)
ppr_mono_ty _         (HsKindSig ty kind) u q =
    parens (ppr_mono_lty pREC_TOP ty u q <+> dcolon u <+> ppLKind u q kind)
ppr_mono_ty _         (HsListTy ty)       u q = brackets (ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty _         (HsPArrTy ty)       u q = pabrackets (ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty ctxt_prec (HsIParamTy n ty)   u q =
    maybeParen ctxt_prec pREC_CTX $ ppIPName n <+> dcolon u <+> ppr_mono_lty pREC_TOP ty u q
ppr_mono_ty _         (HsSpliceTy {})     _ _ = error "ppr_mono_ty HsSpliceTy"
ppr_mono_ty _         (HsRecTy {})        _ _ = toHtml "{..}"
       -- Can now legally occur in ConDeclGADT, the output here is to provide a
       -- placeholder in the signature, which is followed by the field
       -- declarations.
ppr_mono_ty _         (HsCoreTy {})       _ _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty _         (HsExplicitListTy _ tys) u q = promoQuote $ brackets $ hsep $ punctuate comma $ map (ppLType u q) tys
ppr_mono_ty _         (HsExplicitTupleTy _ tys) u q = promoQuote $ parenList $ map (ppLType u q) tys
ppr_mono_ty _         (HsAppsTy {})       _ _ = error "ppr_mono_ty HsAppsTy"

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2) unicode qual
  = maybeParen ctxt_prec pREC_CTX $
    ppr_mono_lty pREC_OP ty1 unicode qual <+> char '~' <+> ppr_mono_lty pREC_OP ty2 unicode qual

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode qual
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode qual, ppr_mono_lty pREC_CON arg_ty unicode qual]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2) unicode qual
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode qual <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode qual
  where
    -- `(:)` is valid in type signature only as constructor to promoted list
    -- and needs to be quoted in code so we explicitly quote it here too.
    ppr_op
        | (getOccString . getName . unLoc) op == ":" = promoQuote ppr_op'
        | otherwise = ppr_op'
    ppr_op' = ppLDocName qual Infix op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode qual
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode qual

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode qual
  = ppr_mono_lty ctxt_prec ty unicode qual

ppr_mono_ty _ (HsWildCardTy (AnonWildCard _)) _ _ = char '_'
ppr_mono_ty _ (HsTyLit n) _ _ = ppr_tylit n

ppr_tylit :: HsTyLit -> Html
ppr_tylit (HsNumTy _ n) = toHtml (show n)
ppr_tylit (HsStrTy _ s) = toHtml (show s)


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Unicode -> Qualification -> Html
ppr_fun_ty ctxt_prec ty1 ty2 unicode qual
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode qual
        p2 = ppr_mono_lty pREC_TOP ty2 unicode qual
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow unicode <+> p2]
