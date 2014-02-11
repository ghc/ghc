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

  ppTyName, ppTyFamHeader, ppTypeApp,
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

import           Data.List             ( intersperse )
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid           ( mempty )
import           Text.XHtml hiding     ( name, title, p, quote )

import GHC
import Name


ppDecl :: Bool -> LinksInfo -> LHsDecl DocName ->
          DocForDecl DocName -> [DocInstance DocName] -> [(DocName, DocForDecl DocName)] ->
          Bool -> Qualification -> Html
ppDecl summ links (L loc decl) (mbDoc, fnArgsDoc) instances subdocs unicode qual = case decl of
  TyClD (FamDecl d)         -> ppTyFam summ False links instances loc mbDoc d unicode qual
  TyClD d@(DataDecl {})     -> ppDataDecl summ links instances subdocs loc mbDoc d unicode qual
  TyClD d@(SynDecl {})      -> ppTySyn summ links loc (mbDoc, fnArgsDoc) d unicode qual
  TyClD d@(ClassDecl {})    -> ppClassDecl summ links instances loc mbDoc subdocs d unicode qual
  SigD (TypeSig lnames lty) -> ppLFunSig summ links loc (mbDoc, fnArgsDoc) lnames lty unicode qual
  SigD (PatSynSig lname args ty prov req) ->
      ppLPatSig summ links loc (mbDoc, fnArgsDoc) lname args ty prov req unicode qual
  ForD d                         -> ppFor summ links loc (mbDoc, fnArgsDoc) d unicode qual
  InstD _                        -> noHtml
  _                              -> error "declaration not supported by ppDecl"


ppLFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             [Located DocName] -> LHsType DocName -> Bool -> Qualification -> Html
ppLFunSig summary links loc doc lnames lty unicode qual =
  ppFunSig summary links loc doc (map unLoc lnames) (unLoc lty) unicode qual

ppFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
            [DocName] -> HsType DocName -> Bool -> Qualification -> Html
ppFunSig summary links loc doc docnames typ unicode qual =
  ppSigLike summary links loc mempty doc docnames (typ, pp_typ) unicode qual
  where
    pp_typ = ppType unicode qual typ

ppLPatSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
             Located DocName ->
             HsPatSynDetails (LHsType DocName) -> LHsType DocName ->
             LHsContext DocName -> LHsContext DocName ->
             Bool -> Qualification -> Html
ppLPatSig summary links loc doc lname args typ prov req unicode qual =
    ppPatSig summary links loc doc (unLoc lname) (fmap unLoc args) (unLoc typ) (unLoc prov) (unLoc req) unicode qual

ppPatSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
            DocName ->
            HsPatSynDetails (HsType DocName) -> HsType DocName ->
            HsContext DocName -> HsContext DocName ->
            Bool -> Qualification -> Html
ppPatSig summary links loc (doc, _argDocs) docname args typ prov req unicode qual
  | summary = pref1
  | otherwise = topDeclElem links loc [docname] pref1 +++ docSection qual doc
  where
    pref1 = hsep [ toHtml "pattern"
                 , pp_cxt prov
                 , pp_head
                 , dcolon unicode
                 , pp_cxt req
                 , ppType unicode qual typ
                 ]
    pp_head = case args of
        PrefixPatSyn typs -> hsep $ ppBinder summary occname : map pp_type typs
        InfixPatSyn left right -> hsep [pp_type left, ppBinderInfix summary occname, pp_type right]

    pp_cxt cxt = ppContext cxt unicode qual
    pp_type = ppParendType unicode qual

    occname = nameOccName . getName $ docname

ppSigLike :: Bool -> LinksInfo -> SrcSpan -> Html -> DocForDecl DocName ->
            [DocName] -> (HsType DocName, Html) -> Bool -> Qualification -> Html
ppSigLike summary links loc leader doc docnames (typ, pp_typ) unicode qual =
  ppTypeOrFunSig summary links loc docnames typ doc
    ( leader <+> ppTypeSig summary occnames pp_typ unicode
    , concatHtml . punctuate comma $ map (ppBinder False) occnames
    , dcolon unicode
    )
    unicode qual
  where
    occnames = map (nameOccName . getName) docnames


ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> [DocName] -> HsType DocName
               -> DocForDecl DocName -> (Html, Html, Html) -> Bool -> Qualification -> Html
ppTypeOrFunSig summary links loc docnames typ (doc, argDocs) (pref1, pref2, sep) unicode qual
  | summary = pref1
  | Map.null argDocs = topDeclElem links loc docnames pref1 +++ docSection qual doc
  | otherwise = topDeclElem links loc docnames pref2 +++
      subArguments qual (do_args 0 sep typ) +++ docSection qual doc
  where
    argDoc n = Map.lookup n argDocs

    do_largs n leader (L _ t) = do_args n leader t
    do_args :: Int -> Html -> HsType DocName -> [SubDecl]
    do_args n leader (HsForAllTy Explicit tvs lctxt ltype)
      = (leader <+>
          hsep (forallSymbol unicode : ppTyVars tvs ++ [dot]) <+>
          ppLContextNoArrow lctxt unicode qual,
          Nothing, [])
        : do_largs n (darrow unicode) ltype
    do_args n leader (HsForAllTy Implicit _ lctxt ltype)
      | not (null (unLoc lctxt))
      = (leader <+> ppLContextNoArrow lctxt unicode qual,
          Nothing, [])
        : do_largs n (darrow unicode) ltype
      -- if we're not showing any 'forall' or class constraints or
      -- anything, skip having an empty line for the context.
      | otherwise
      = do_largs n leader ltype
    do_args n leader (HsFunTy lt r)
      = (leader <+> ppLFunLhType unicode qual lt, argDoc n, [])
        : do_largs (n+1) (arrow unicode) r
    do_args n leader t
      = [(leader <+> ppType unicode qual t, argDoc n, [])]


ppTyVars :: LHsTyVarBndrs DocName -> [Html]
ppTyVars tvs = map ppTyName (tyvarNames tvs)


tyvarNames :: LHsTyVarBndrs DocName -> [Name]
tyvarNames = map getName . hsLTyVarNames


ppFor :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName
      -> ForeignDecl DocName -> Bool -> Qualification -> Html
ppFor summary links loc doc (ForeignImport (L _ name) (L _ typ) _ _) unicode qual
  = ppFunSig summary links loc doc [name] typ unicode qual
ppFor _ _ _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName -> TyClDecl DocName -> Bool
        -> Qualification -> Html
ppTySyn summary links loc doc (SynDecl { tcdLName = L _ name, tcdTyVars = ltyvars
                                       , tcdRhs = ltype })
        unicode qual
  = ppTypeOrFunSig summary links loc [name] (unLoc ltype) doc
                   (full, hdr, spaceHtml +++ equals) unicode qual
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ] ++ ppTyVars ltyvars)
    full = hdr <+> equals <+> ppLType unicode qual ltype
    occ  = nameOccName . getName $ name
ppTySyn _ _ _ _ _ _ _ = error "declaration not supported by ppTySyn"


ppTypeSig :: Bool -> [OccName] -> Html  -> Bool -> Html
ppTypeSig summary nms pp_ty unicode =
  concatHtml htmlNames <+> dcolon unicode <+> pp_ty
  where
    htmlNames = intersperse (stringToHtml ", ") $ map (ppBinder summary) nms


ppTyName :: Name -> Html
ppTyName = ppName (Just False)


--------------------------------------------------------------------------------
-- * Type families
--------------------------------------------------------------------------------


ppTyFamHeader :: Bool -> Bool -> FamilyDecl DocName -> Bool -> Qualification -> Html
ppTyFamHeader summary associated d@(FamilyDecl { fdInfo = info
                                               , fdKindSig = mkind }) unicode qual =
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

  ppFamDeclBinderWithVars summary d <+>

  (case mkind of
    Just kind -> dcolon unicode  <+> ppLKind unicode qual kind
    Nothing   -> noHtml
  )

ppTyFam :: Bool -> Bool -> LinksInfo -> [DocInstance DocName] -> SrcSpan -> Documentation DocName ->
              FamilyDecl DocName -> Bool -> Qualification -> Html
ppTyFam summary associated links instances loc doc decl unicode qual

  | summary   = ppTyFamHeader True associated decl unicode qual
  | otherwise = header_ +++ docSection qual doc +++ instancesBit

  where
    docname = unLoc $ fdLName decl

    header_ = topDeclElem links loc [docname] (ppTyFamHeader summary associated decl unicode qual)

    instancesBit
      | FamilyDecl { fdInfo = ClosedTypeFamily eqns } <- decl
      , not summary
      = subEquations qual $ map (ppTyFamEqn . unLoc) eqns

      | otherwise
      = ppInstances instances docname unicode qual

    -- Individual equation of a closed type family
    ppTyFamEqn TyFamInstEqn { tfie_tycon = n, tfie_rhs = rhs
                            , tfie_pats = HsWB { hswb_cts = ts }}
      = ( ppAppNameTypes (unLoc n) [] (map unLoc ts) unicode qual
          <+> equals <+> ppType unicode qual (unLoc rhs)
        , Nothing, [] )

--------------------------------------------------------------------------------
-- * Associated Types
--------------------------------------------------------------------------------


ppAssocType :: Bool -> LinksInfo -> DocForDecl DocName -> LFamilyDecl DocName -> Bool
            -> Qualification -> Html
ppAssocType summ links doc (L loc decl) unicode qual =
   ppTyFam summ True links [] loc (fst doc) decl unicode qual


--------------------------------------------------------------------------------
-- * TyClDecl helpers
--------------------------------------------------------------------------------

-- | Print a type family and its variables
ppFamDeclBinderWithVars :: Bool -> FamilyDecl DocName -> Html
ppFamDeclBinderWithVars summ (FamilyDecl { fdLName = lname, fdTyVars = tvs }) =
  ppAppDocNameNames summ (unLoc lname) (tyvarNames tvs)

-- | Print a newtype / data binder and its variables
ppDataBinderWithVars :: Bool -> TyClDecl DocName -> Html
ppDataBinderWithVars summ decl =
  ppAppDocNameNames summ (tcdName decl) (tyvarNames $ tcdTyVars decl)


--------------------------------------------------------------------------------
-- * Type applications
--------------------------------------------------------------------------------


-- | Print an application of a DocName and two lists of HsTypes (kinds, types)
ppAppNameTypes :: DocName -> [HsType DocName] -> [HsType DocName] -> Bool -> Qualification -> Html
ppAppNameTypes n ks ts unicode qual =
    ppTypeApp n ks ts (ppDocName qual . Just) (ppParendType unicode qual)


-- | Print an application of a DocName and a list of Names
ppAppDocNameNames :: Bool -> DocName -> [Name] -> Html
ppAppDocNameNames summ n ns =
    ppTypeApp n [] ns ppDN ppTyName
  where
    ppDN is_infix = ppBinderFixity is_infix summ . nameOccName . getName
    ppBinderFixity True = ppBinderInfix
    ppBinderFixity False = ppBinder

-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> [a] -> (Bool -> DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n [] (t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN True n <+> ppT t2

ppTypeApp n ks ts ppDN ppT = ppDN False n <+> hsep (map ppT $ ks ++ ts)


-------------------------------------------------------------------------------
-- * Contexts
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Bool
                              -> Qualification -> Html
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc


ppContextNoArrow :: HsContext DocName -> Bool -> Qualification -> Html
ppContextNoArrow []  _       _     = noHtml
ppContextNoArrow cxt unicode qual = ppHsContext (map unLoc cxt) unicode qual


ppContextNoLocs :: [HsType DocName] -> Bool -> Qualification -> Html
ppContextNoLocs []  _       _     = noHtml
ppContextNoLocs cxt unicode qual = ppHsContext cxt unicode qual
    <+> darrow unicode


ppContext :: HsContext DocName -> Bool -> Qualification -> Html
ppContext cxt unicode qual = ppContextNoLocs (map unLoc cxt) unicode qual


ppHsContext :: [HsType DocName] -> Bool -> Qualification-> Html
ppHsContext []  _       _     = noHtml
ppHsContext [p] unicode qual = ppType unicode qual p
ppHsContext cxt unicode qual = parenList (map (ppType unicode qual) cxt)


-------------------------------------------------------------------------------
-- * Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsType DocName] -> DocName
           -> LHsTyVarBndrs DocName -> [Located ([DocName], [DocName])]
           -> Bool -> Qualification -> Html
ppClassHdr summ lctxt n tvs fds unicode qual =
  keyword "class"
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode qual else noHtml)
  <+> ppAppDocNameNames summ n (tyvarNames tvs)
  <+> ppFds fds unicode qual


ppFds :: [Located ([DocName], [DocName])] -> Bool -> Qualification -> Html
ppFds fds unicode qual =
  if null fds then noHtml else
        char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
        fundep (vars1,vars2) = ppVars vars1 <+> arrow unicode <+> ppVars vars2
        ppVars = hsep . map (ppDocName qual (Just False))

ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan
                 -> [(DocName, DocForDecl DocName)] -> Bool -> Qualification
                 -> Html
ppShortClassDecl summary links (ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = tvs
                                          , tcdFDs = fds, tcdSigs = sigs, tcdATs = ats }) loc
    subdocs unicode qual =
  if null sigs && null ats
    then (if summary then id else topDeclElem links loc [nm]) hdr
    else (if summary then id else topDeclElem links loc [nm]) (hdr <+> keyword "where")
      +++ shortSubDecls
          (
            [ ppAssocType summary links doc at unicode qual | at <- ats
              , let doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs ]  ++

                -- ToDo: add associated type defaults

            [ ppFunSig summary links loc doc names typ unicode qual
              | L _ (TypeSig lnames (L _ typ)) <- sigs
              , let doc = lookupAnySubdoc (head names) subdocs
                    names = map unLoc lnames ]
              -- FIXME: is taking just the first name ok? Is it possible that
              -- there are different subdocs for different names in a single
              -- type signature?
          )
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds unicode qual
    nm  = unLoc lname
ppShortClassDecl _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"



ppClassDecl :: Bool -> LinksInfo -> [DocInstance DocName] -> SrcSpan
            -> Documentation DocName -> [(DocName, DocForDecl DocName)]
            -> TyClDecl DocName -> Bool -> Qualification -> Html
ppClassDecl summary links instances loc d subdocs
        decl@(ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = ltyvars
                        , tcdFDs = lfds, tcdSigs = lsigs, tcdATs = ats }) unicode qual
  | summary = ppShortClassDecl summary links decl loc subdocs unicode qual
  | otherwise = classheader +++ docSection qual d
                  +++ atBit +++ methodBit  +++ instancesBit
  where
    classheader
      | null lsigs = topDeclElem links loc [nm] (hdr unicode qual)
      | otherwise  = topDeclElem links loc [nm] (hdr unicode qual <+> keyword "where")

    nm   = tcdName decl

    hdr = ppClassHdr summary lctxt (unLoc lname) ltyvars lfds

    -- ToDo: add assocatied typ defaults
    atBit = subAssociatedTypes [ ppAssocType summary links doc at unicode qual
                      | at <- ats
                      , let doc = lookupAnySubdoc (unL $ fdLName $ unL at) subdocs ]

    methodBit = subMethods [ ppFunSig summary links loc doc names typ unicode qual
                           | L _ (TypeSig lnames (L _ typ)) <- lsigs
                           , let doc = lookupAnySubdoc (head names) subdocs
                                 names = map unLoc lnames ]
                           -- FIXME: is taking just the first name ok? Is it possible that
                           -- there are different subdocs for different names in a single
                           -- type signature?

    instancesBit = ppInstances instances nm unicode qual

ppClassDecl _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstances :: [DocInstance DocName] -> DocName -> Bool -> Qualification -> Html
ppInstances instances baseName unicode qual
  = subInstances qual instName (map instDecl instances)
  where
    instName = getOccString $ getName baseName
    instDecl :: DocInstance DocName -> SubDecl
    instDecl (inst, maybeDoc) = (instHead inst, maybeDoc, [])
    instHead (n, ks, ts, ClassInst cs) = ppContextNoLocs cs unicode qual
        <+> ppAppNameTypes n ks ts unicode qual
    instHead (n, ks, ts, TypeInst rhs) = keyword "type"
        <+> ppAppNameTypes n ks ts unicode qual
        <+> equals <+> ppType unicode qual rhs
    instHead (n, ks, ts, DataInst dd) = keyword "data"
        <+> ppAppNameTypes n ks ts unicode qual
        <+> ppShortDataDecl False True dd unicode qual

lookupAnySubdoc :: Eq id1 => id1 -> [(id1, DocForDecl id2)] -> DocForDecl id2
lookupAnySubdoc n = fromMaybe noDocForDecl . lookup n


-------------------------------------------------------------------------------
-- * Data & newtype declarations
-------------------------------------------------------------------------------


-- TODO: print contexts
ppShortDataDecl :: Bool -> Bool -> TyClDecl DocName -> Bool -> Qualification -> Html
ppShortDataDecl summary dataInst dataDecl unicode qual

  | [] <- cons = dataHeader

  | [lcon] <- cons, ResTyH98 <- resTy,
    (cHead,cBody,cFoot) <- ppShortConstrParts summary (unLoc lcon) unicode qual
       = (dataHeader <+> equals <+> cHead) +++ cBody +++ cFoot

  | ResTyH98 <- resTy = dataHeader
      +++ shortSubDecls (zipWith doConstr ('=':repeat '|') cons)

  | otherwise = (dataHeader <+> keyword "where")
      +++ shortSubDecls (map doGADTConstr cons)

  where
    dataHeader
      | dataInst  = noHtml
      | otherwise = ppDataHeader summary dataDecl unicode qual
    doConstr c con = toHtml [c] <+> ppShortConstr summary (unLoc con) unicode qual
    doGADTConstr con = ppShortConstr summary (unLoc con) unicode qual

    cons      = dd_cons (tcdDataDefn dataDecl)
    resTy     = (con_res . unLoc . head) cons


ppDataDecl :: Bool -> LinksInfo -> [DocInstance DocName] ->
              [(DocName, DocForDecl DocName)] ->
              SrcSpan -> Documentation DocName -> TyClDecl DocName -> Bool ->
              Qualification -> Html
ppDataDecl summary links instances subdocs loc doc dataDecl unicode qual

  | summary   = ppShortDataDecl summary False dataDecl unicode qual
  | otherwise = header_ +++ docSection qual doc +++ constrBit +++ instancesBit

  where
    docname   = tcdName dataDecl
    cons      = dd_cons (tcdDataDefn dataDecl)
    resTy     = (con_res . unLoc . head) cons

    header_ = topDeclElem links loc [docname] (ppDataHeader summary dataDecl unicode qual
             <+> whereBit)

    whereBit
      | null cons = noHtml
      | otherwise = case resTy of
        ResTyGADT _ -> keyword "where"
        _ -> noHtml

    constrBit = subConstructors qual
      (map (ppSideBySideConstr subdocs unicode qual) cons)

    instancesBit = ppInstances instances docname unicode qual



ppShortConstr :: Bool -> ConDecl DocName -> Bool -> Qualification -> Html
ppShortConstr summary con unicode qual = cHead <+> cBody <+> cFoot
  where
    (cHead,cBody,cFoot) = ppShortConstrParts summary con unicode qual


-- returns three pieces: header, body, footer so that header & footer can be
-- incorporated into the declaration
ppShortConstrParts :: Bool -> ConDecl DocName -> Bool -> Qualification -> (Html, Html, Html)
ppShortConstrParts summary con unicode qual = case con_res con of
  ResTyH98 -> case con_details con of
    PrefixCon args ->
      (header_ unicode qual +++ hsep (ppBinder summary occ
            : map (ppLParendType unicode qual) args), noHtml, noHtml)
    RecCon fields ->
      (header_ unicode qual +++ ppBinder summary occ <+> char '{',
       doRecordFields fields,
       char '}')
    InfixCon arg1 arg2 ->
      (header_ unicode qual +++ hsep [ppLParendType unicode qual arg1,
            ppBinderInfix summary occ, ppLParendType unicode qual arg2],
       noHtml, noHtml)

  ResTyGADT resTy -> case con_details con of
    -- prefix & infix could use hsConDeclArgTys if it seemed to
    -- simplify the code.
    PrefixCon args -> (doGADTCon args resTy, noHtml, noHtml)
    -- display GADT records with the new syntax,
    -- Constr :: (Context) => { field :: a, field2 :: b } -> Ty (a, b)
    -- (except each field gets its own line in docs, to match
    -- non-GADT records)
    RecCon fields -> (ppBinder summary occ <+> dcolon unicode <+>
                            ppForAll forall_ ltvs lcontext unicode qual <+> char '{',
                            doRecordFields fields,
                            char '}' <+> arrow unicode <+> ppLType unicode qual resTy)
    InfixCon arg1 arg2 -> (doGADTCon [arg1, arg2] resTy, noHtml, noHtml)

  where
    doRecordFields fields = shortSubDecls (map (ppShortField summary unicode qual) fields)
    doGADTCon args resTy = ppBinder summary occ <+> dcolon unicode <+> hsep [
                             ppForAll forall_ ltvs lcontext unicode qual,
                             ppLType unicode qual (foldr mkFunTy resTy args) ]

    header_  = ppConstrHdr forall_ tyVars context
    occ      = nameOccName . getName . unLoc . con_name $ con
    ltvs     = con_qvars con
    tyVars   = tyvarNames ltvs
    lcontext = con_cxt con
    context  = unLoc (con_cxt con)
    forall_  = con_explicit con
    mkFunTy a b = noLoc (HsFunTy a b)


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr :: HsExplicitFlag -> [Name] -> HsContext DocName -> Bool
            -> Qualification -> Html
ppConstrHdr forall_ tvs ctxt unicode qual
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml else ppContextNoArrow ctxt unicode qual
        <+> darrow unicode +++ toHtml " ")
  where
    ppForall = case forall_ of
      Explicit -> forallSymbol unicode <+> hsep (map (ppName (Just False)) tvs) <+> toHtml ". "
      Implicit -> noHtml


ppSideBySideConstr :: [(DocName, DocForDecl DocName)] -> Bool -> Qualification
                   -> LConDecl DocName -> SubDecl
ppSideBySideConstr subdocs unicode qual (L _ con) = (decl, mbDoc, fieldPart)
 where
    decl = case con_res con of
      ResTyH98 -> case con_details con of
        PrefixCon args ->
          hsep ((header_ unicode qual +++ ppBinder False occ)
            : map (ppLParendType unicode qual) args)

        RecCon _ -> header_ unicode qual +++ ppBinder False occ

        InfixCon arg1 arg2 ->
          hsep [header_ unicode qual +++ ppLParendType unicode qual arg1,
            ppBinderInfix False occ,
            ppLParendType unicode qual arg2]

      ResTyGADT resTy -> case con_details con of
        -- prefix & infix could also use hsConDeclArgTys if it seemed to
        -- simplify the code.
        PrefixCon args -> doGADTCon args resTy
        cd@(RecCon _) -> doGADTCon (hsConDeclArgTys cd) resTy
        InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy

    fieldPart = case con_details con of
        RecCon fields -> [doRecordFields fields]
        _ -> []

    doRecordFields fields = subFields qual
      (map (ppSideBySideField subdocs unicode qual) fields)
    doGADTCon :: [LHsType DocName] -> Located (HsType DocName) -> Html
    doGADTCon args resTy =
      ppBinder False occ <+> dcolon unicode
        <+> hsep [ppForAll forall_ ltvs (con_cxt con) unicode qual,
                  ppLType unicode qual (foldr mkFunTy resTy args) ]

    header_ = ppConstrHdr forall_ tyVars context
    occ     = nameOccName . getName . unLoc . con_name $ con
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall_ = con_explicit con
    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc = lookup (unLoc $ con_name con) subdocs >>= combineDocumentation . fst
    mkFunTy a b = noLoc (HsFunTy a b)


ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Bool -> Qualification
                  -> ConDeclField DocName ->  SubDecl
ppSideBySideField subdocs unicode qual (ConDeclField (L _ name) ltype _) =
  (ppBinder False (nameOccName . getName $ name) <+> dcolon unicode <+> ppLType unicode qual ltype,
    mbDoc,
    [])
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    mbDoc = lookup name subdocs >>= combineDocumentation . fst


ppShortField :: Bool -> Bool -> Qualification -> ConDeclField DocName -> Html
ppShortField summary unicode qual (ConDeclField (L _ name) ltype _)
  = ppBinder summary (nameOccName . getName $ name)
    <+> dcolon unicode <+> ppLType unicode qual ltype


-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocName -> Bool -> Qualification -> Html
ppDataHeader summary decl@(DataDecl { tcdDataDefn = HsDataDefn { dd_ND = nd
                                                               , dd_ctxt = ctxt } })
             unicode qual
  = -- newtype or data
    (case nd of { NewType -> keyword "newtype"; DataType -> keyword "data" }) <+>
    -- context
    ppLContext ctxt unicode qual <+>
    -- T a b c ..., or a :+: b
    ppDataBinderWithVars summary decl
ppDataHeader _ _ _ _ = error "ppDataHeader: illegal argument"


--------------------------------------------------------------------------------
-- * Types and contexts
--------------------------------------------------------------------------------


ppBang :: HsBang -> Html
ppBang HsNoBang = noHtml
ppBang _        = toHtml "!" -- Unpacked args is an implementation detail,
                             -- so we just show the strictness annotation


tupleParens :: HsTupleSort -> [Html] -> Html
tupleParens HsUnboxedTuple = ubxParenList
tupleParens _              = parenList


--------------------------------------------------------------------------------
-- * Rendering of HsType
--------------------------------------------------------------------------------


pREC_TOP, pREC_FUN, pREC_OP, pREC_CON :: Int

pREC_TOP = 0 :: Int   -- type in ParseIface.y in GHC
pREC_FUN = 1 :: Int   -- btype in ParseIface.y in GHC
                      -- Used for LH arg of (->)
pREC_OP  = 2 :: Int   -- Used for arg of any infix operator
                      -- (we don't keep their fixities around)
pREC_CON = 3 :: Int   -- Used for arg of type applicn:
                      -- always parenthesise unless atomic

maybeParen :: Int           -- Precedence of context
           -> Int           -- Precedence of top-level operator
           -> Html -> Html  -- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
                               | otherwise            = p


ppLType, ppLParendType, ppLFunLhType :: Bool -> Qualification
                                     -> Located (HsType DocName) -> Html
ppLType       unicode qual y = ppType unicode qual (unLoc y)
ppLParendType unicode qual y = ppParendType unicode qual (unLoc y)
ppLFunLhType  unicode qual y = ppFunLhType unicode qual (unLoc y)


ppType, ppParendType, ppFunLhType :: Bool -> Qualification-> HsType DocName -> Html
ppType       unicode qual ty = ppr_mono_ty pREC_TOP ty unicode qual
ppParendType unicode qual ty = ppr_mono_ty pREC_CON ty unicode qual
ppFunLhType  unicode qual ty = ppr_mono_ty pREC_FUN ty unicode qual

ppLKind :: Bool -> Qualification-> LHsKind DocName -> Html
ppLKind unicode qual y = ppKind unicode qual (unLoc y)

ppKind :: Bool -> Qualification-> HsKind DocName -> Html
ppKind unicode qual ki = ppr_mono_ty pREC_TOP ki unicode qual

-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppForAll :: HsExplicitFlag -> LHsTyVarBndrs DocName
         -> Located (HsContext DocName) -> Bool -> Qualification -> Html
ppForAll expl tvs cxt unicode qual
  | show_forall = forall_part <+> ppLContext cxt unicode qual
  | otherwise   = ppLContext cxt unicode qual
  where
    show_forall = not (null (hsQTvBndrs tvs)) && is_explicit
    is_explicit = case expl of {Explicit -> True; Implicit -> False}
    forall_part = hsep (forallSymbol unicode : ppTyVars tvs) +++ dot


ppr_mono_lty :: Int -> LHsType DocName -> Bool -> Qualification -> Html
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)


ppr_mono_ty :: Int -> HsType DocName -> Bool -> Qualification -> Html
ppr_mono_ty ctxt_prec (HsForAllTy expl tvs ctxt ty) unicode qual
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll expl tvs ctxt unicode qual, ppr_mono_lty pREC_TOP ty unicode qual]

ppr_mono_ty _         (HsBangTy b ty)     u q = ppBang b +++ ppLParendType u q ty
ppr_mono_ty _         (HsTyVar name)      _ q = ppDocName q (Just False) name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   u q = ppr_fun_ty ctxt_prec ty1 ty2 u q
ppr_mono_ty _         (HsTupleTy con tys) u q = tupleParens con (map (ppLType u q) tys)
ppr_mono_ty _         (HsKindSig ty kind) u q =
    parens (ppr_mono_lty pREC_TOP ty u q <+> dcolon u <+> ppLKind u q kind)
ppr_mono_ty _         (HsListTy ty)       u q = brackets (ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty _         (HsPArrTy ty)       u q = pabrackets (ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty _         (HsIParamTy n ty)   u q = brackets (ppIPName n <+> dcolon u <+> ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty _         (HsSpliceTy {})     _ _ = error "ppr_mono_ty HsSpliceTy"
ppr_mono_ty _         (HsQuasiQuoteTy {}) _ _ = error "ppr_mono_ty HsQuasiQuoteTy"
ppr_mono_ty _         (HsRecTy {})        _ _ = error "ppr_mono_ty HsRecTy"
ppr_mono_ty _         (HsCoreTy {})       _ _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty _         (HsExplicitListTy _ tys) u q = quote $ brackets $ hsep $ punctuate comma $ map (ppLType u q) tys
ppr_mono_ty _         (HsExplicitTupleTy _ tys) u q = quote $ parenList $ map (ppLType u q) tys
ppr_mono_ty _         (HsWrapTy {})       _ _ = error "ppr_mono_ty HsWrapTy"

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2) unicode qual
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_lty pREC_OP ty1 unicode qual <+> char '~' <+> ppr_mono_lty pREC_OP ty2 unicode qual

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode qual
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode qual, ppr_mono_lty pREC_CON arg_ty unicode qual]

ppr_mono_ty ctxt_prec (HsOpTy ty1 (_, op) ty2) unicode qual
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode qual <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode qual
  where
    ppr_op = ppLDocName qual (Just True) op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode qual
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode qual

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode qual
  = ppr_mono_lty ctxt_prec ty unicode qual

ppr_mono_ty _ (HsTyLit n) _ _ = ppr_tylit n

ppr_tylit :: HsTyLit -> Html
ppr_tylit (HsNumTy n) = toHtml (show n)
ppr_tylit (HsStrTy s) = toHtml (show s)


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Bool -> Qualification -> Html
ppr_fun_ty ctxt_prec ty1 ty2 unicode qual
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode qual
        p2 = ppr_mono_lty pREC_TOP ty2 unicode qual
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow unicode <+> p2]
