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

import           Control.Monad         ( join )
import qualified Data.Map as Map
import           Data.Maybe
import           Text.XHtml hiding     ( name, title, p, quote )

import BasicTypes            ( IPName(..), Boxity(..) )
import GHC
import Name
import Outputable            ( ppr, showSDoc, Outputable )


-- TODO: use DeclInfo DocName or something
ppDecl :: Bool -> LinksInfo -> LHsDecl DocName ->
          DocForDecl DocName -> [DocInstance DocName] -> [(DocName, DocForDecl DocName)] ->
          Bool -> Qualification -> Html
ppDecl summ links (L loc decl) (mbDoc, fnArgsDoc) instances subdocs unicode quali = case decl of
  TyClD d@(TyFamily {})          -> ppTyFam summ False links loc mbDoc d unicode quali
  TyClD d@(TyData {})
    | Nothing <- tcdTyPats d     -> ppDataDecl summ links instances subdocs loc mbDoc d unicode quali
    | Just _  <- tcdTyPats d     -> ppDataInst summ links loc mbDoc d
  TyClD d@(TySynonym {})
    | Nothing <- tcdTyPats d     -> ppTySyn summ links loc (mbDoc, fnArgsDoc) d unicode quali
    | Just _  <- tcdTyPats d     -> ppTyInst summ False links loc mbDoc d unicode quali
  TyClD d@(ClassDecl {})         -> ppClassDecl summ links instances loc mbDoc subdocs d unicode quali
  SigD (TypeSig (L _ n) (L _ t)) -> ppFunSig summ links loc (mbDoc, fnArgsDoc) n t unicode quali
  ForD d                         -> ppFor summ links loc (mbDoc, fnArgsDoc) d unicode quali
  InstD _                        -> noHtml
  _                              -> error "declaration not supported by ppDecl"


ppFunSig :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName ->
            DocName -> HsType DocName -> Bool -> Qualification -> Html
ppFunSig summary links loc doc docname typ unicode quali =
  ppTypeOrFunSig summary links loc docname typ doc
    (ppTypeSig summary occname typ unicode quali, ppBinder False occname, dcolon unicode)
    unicode quali
  where
    occname = docNameOcc docname


ppTypeOrFunSig :: Bool -> LinksInfo -> SrcSpan -> DocName -> HsType DocName ->
                  DocForDecl DocName -> (Html, Html, Html) -> Bool -> Qualification-> Html
ppTypeOrFunSig summary links loc docname typ (doc, argDocs) (pref1, pref2, sep) unicode quali
  | summary = pref1
  | Map.null argDocs = topDeclElem links loc docname pref1 +++ maybeDocSection quali doc
  | otherwise = topDeclElem links loc docname pref2 +++
      subArguments quali (do_args 0 sep typ) +++ maybeDocSection quali doc
  where
    argDoc n = Map.lookup n argDocs

    do_largs n leader (L _ t) = do_args n leader t
    do_args :: Int -> Html -> (HsType DocName) -> [SubDecl]
    do_args n leader (HsForAllTy Explicit tvs lctxt ltype)
      = (leader <+>
          hsep (forallSymbol unicode : ppTyVars tvs ++ [dot]) <+>
          ppLContextNoArrow lctxt unicode quali,
          Nothing, [])
        : do_largs n (darrow unicode) ltype
    do_args n leader (HsForAllTy Implicit _ lctxt ltype)
      | not (null (unLoc lctxt))
      = (leader <+> ppLContextNoArrow lctxt unicode quali,
          Nothing, [])
        : do_largs n (darrow unicode) ltype
      -- if we're not showing any 'forall' or class constraints or
      -- anything, skip having an empty line for the context.
      | otherwise
      = do_largs n leader ltype
    do_args n leader (HsFunTy lt r)
      = (leader <+> ppLFunLhType unicode quali lt, argDoc n, [])
        : do_largs (n+1) (arrow unicode) r
    do_args n leader t
      = (leader <+> ppType unicode quali t, argDoc n, []) : []


ppTyVars :: [LHsTyVarBndr DocName] -> [Html]
ppTyVars tvs = map ppTyName (tyvarNames tvs)


tyvarNames :: [LHsTyVarBndr DocName] -> [Name]
tyvarNames = map (getName . hsTyVarName . unLoc)


ppFor :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName -> ForeignDecl DocName -> Bool 
      -> Qualification -> Html
ppFor summary links loc doc (ForeignImport (L _ name) (L _ typ) _) unicode quali
  = ppFunSig summary links loc doc name typ unicode quali
ppFor _ _ _ _ _ _ _ = error "ppFor"


-- we skip type patterns for now
ppTySyn :: Bool -> LinksInfo -> SrcSpan -> DocForDecl DocName -> TyClDecl DocName -> Bool
        -> Qualification -> Html
ppTySyn summary links loc doc (TySynonym (L _ name) ltyvars _ ltype) unicode quali
  = ppTypeOrFunSig summary links loc name (unLoc ltype) doc
                   (full, hdr, spaceHtml +++ equals) unicode quali
  where
    hdr  = hsep ([keyword "type", ppBinder summary occ] ++ ppTyVars ltyvars)
    full = hdr <+> equals <+> ppLType unicode quali ltype
    occ  = docNameOcc name
ppTySyn _ _ _ _ _ _ _ = error "declaration not supported by ppTySyn"


ppTypeSig :: Bool -> OccName -> HsType DocName  -> Bool -> Qualification -> Html
ppTypeSig summary nm ty unicode quali =
    ppBinder summary nm <+> dcolon unicode <+> ppType unicode quali ty


ppTyName :: Name -> Html
ppTyName name
  | isNameSym name = parens (ppName name)
  | otherwise = ppName name


--------------------------------------------------------------------------------
-- * Type families
--------------------------------------------------------------------------------


ppTyFamHeader :: Bool -> Bool -> TyClDecl DocName -> Bool -> Html
ppTyFamHeader summary associated decl unicode =

  (case tcdFlavour decl of
     TypeFamily
       | associated -> keyword "type"
       | otherwise  -> keyword "type family"
     DataFamily
       | associated -> keyword "data"
       | otherwise  -> keyword "data family"
  ) <+>

  ppTyClBinderWithVars summary decl <+>

  case tcdKind decl of
    Just kind -> dcolon unicode  <+> ppKind kind
    Nothing -> noHtml


ppTyFam :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (Doc DocName) ->
              TyClDecl DocName -> Bool -> Qualification -> Html
ppTyFam summary associated links loc mbDoc decl unicode quali

  | summary   = ppTyFamHeader True associated decl unicode
  | otherwise = header_ +++ maybeDocSection quali mbDoc +++ instancesBit

  where
    docname = tcdName decl

    header_ = topDeclElem links loc docname (ppTyFamHeader summary associated decl unicode)

    instancesBit = ppInstances instances docname unicode quali

    -- TODO: get the instances
    instances = []


--------------------------------------------------------------------------------
-- * Indexed data types
--------------------------------------------------------------------------------


ppDataInst :: a
ppDataInst = undefined


--------------------------------------------------------------------------------
-- * Indexed newtypes
--------------------------------------------------------------------------------

-- TODO
-- ppNewTyInst = undefined


--------------------------------------------------------------------------------
-- * Indexed types
--------------------------------------------------------------------------------


ppTyInst :: Bool -> Bool -> LinksInfo -> SrcSpan -> Maybe (Doc DocName) ->
            TyClDecl DocName -> Bool -> Qualification -> Html
ppTyInst summary associated links loc mbDoc decl unicode quali

  | summary   = ppTyInstHeader True associated decl unicode quali
  | otherwise = header_ +++ maybeDocSection quali mbDoc

  where
    docname = tcdName decl

    header_ = topDeclElem links loc docname
        (ppTyInstHeader summary associated decl unicode quali)


ppTyInstHeader :: Bool -> Bool -> TyClDecl DocName -> Bool -> Qualification -> Html
ppTyInstHeader _ _ decl unicode quali =
  keyword "type instance" <+>
  ppAppNameTypes (tcdName decl) typeArgs unicode quali
  where
    typeArgs = map unLoc . fromJust . tcdTyPats $ decl


--------------------------------------------------------------------------------
-- * Associated Types
--------------------------------------------------------------------------------


ppAssocType :: Bool -> LinksInfo -> DocForDecl DocName -> LTyClDecl DocName -> Bool
            -> Qualification -> Html
ppAssocType summ links doc (L loc decl) unicode quali =
  case decl of
    TyFamily  {} -> ppTyFam summ True links loc (fst doc) decl unicode quali
    TySynonym {} -> ppTySyn summ links loc doc decl unicode quali
    _            -> error "declaration type not supported by ppAssocType"


--------------------------------------------------------------------------------
-- * TyClDecl helpers
--------------------------------------------------------------------------------


-- | Print a type family / newtype / data / class binder and its variables 
ppTyClBinderWithVars :: Bool -> TyClDecl DocName -> Html
ppTyClBinderWithVars summ decl =
  ppAppDocNameNames summ (unLoc $ tcdLName decl) (tyvarNames $ tcdTyVars decl)


--------------------------------------------------------------------------------
-- * Type applications
--------------------------------------------------------------------------------


-- | Print an application of a DocName and a list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocName] -> Bool -> Qualification -> Html
ppAppNameTypes n ts unicode quali =
    ppTypeApp n ts (ppDocName quali) (ppParendType unicode quali)


-- | Print an application of a DocName and a list of Names 
ppAppDocNameNames :: Bool -> DocName -> [Name] -> Html
ppAppDocNameNames summ n ns =
  ppTypeApp n ns (ppBinder summ . docNameOcc) ppTyName


-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (DocName -> Html) -> (a -> Html) -> Html
ppTypeApp n (t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN n <+> ppT t2

ppTypeApp n ts ppDN ppT = ppDN n <+> hsep (map ppT ts)


-------------------------------------------------------------------------------
-- * Contexts
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Bool
                              -> Qualification -> Html
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc


ppContextNoArrow :: HsContext DocName -> Bool -> Qualification -> Html
ppContextNoArrow []  _       _     = noHtml
ppContextNoArrow cxt unicode quali = pp_hs_context (map unLoc cxt) unicode quali


ppContextNoLocs :: [HsPred DocName] -> Bool -> Qualification -> Html
ppContextNoLocs []  _       _     = noHtml
ppContextNoLocs cxt unicode quali = pp_hs_context cxt unicode quali 
    <+> darrow unicode


ppContext :: HsContext DocName -> Bool -> Qualification -> Html
ppContext cxt unicode quali = ppContextNoLocs (map unLoc cxt) unicode quali


pp_hs_context :: [HsPred DocName] -> Bool -> Qualification-> Html
pp_hs_context []  _       _     = noHtml
pp_hs_context [p] unicode quali = ppPred unicode quali p
pp_hs_context cxt unicode quali = parenList (map (ppPred unicode quali) cxt)


ppPred :: Bool -> Qualification -> HsPred DocName -> Html
ppPred unicode quali (HsClassP n ts) = ppAppNameTypes n (map unLoc ts) unicode quali
ppPred unicode quali (HsEqualP t1 t2) = ppLType unicode quali t1 <+> toHtml "~"
    <+> ppLType unicode quali t2
ppPred unicode quali (HsIParam (IPName n) t)
  = toHtml "?" +++ ppDocName quali n <+> dcolon unicode <+> ppLType unicode quali t


-------------------------------------------------------------------------------
-- * Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsPred DocName] -> DocName
           -> [Located (HsTyVarBndr DocName)] -> [Located ([DocName], [DocName])]
           -> Bool -> Qualification -> Html
ppClassHdr summ lctxt n tvs fds unicode quali =
  keyword "class"
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode quali else noHtml)
  <+> ppAppDocNameNames summ n (tyvarNames $ tvs)
        <+> ppFds fds unicode quali


ppFds :: [Located ([DocName], [DocName])] -> Bool -> Qualification -> Html
ppFds fds unicode quali =
  if null fds then noHtml else
        char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
        fundep (vars1,vars2) = hsep (map (ppDocName quali) vars1) <+> arrow unicode <+>
                               hsep (map (ppDocName quali) vars2)


ppShortClassDecl :: Bool -> LinksInfo -> TyClDecl DocName -> SrcSpan
                 -> [(DocName, DocForDecl DocName)] -> Bool -> Qualification
                 -> Html
ppShortClassDecl summary links (ClassDecl lctxt lname tvs fds sigs _ ats _) loc
    subdocs unicode quali = 
  if null sigs && null ats
    then (if summary then id else topDeclElem links loc nm) hdr
    else (if summary then id else topDeclElem links loc nm) (hdr <+> keyword "where")
      +++ shortSubDecls
          (
            [ ppAssocType summary links doc at unicode quali | at <- ats
              , let doc = lookupAnySubdoc (tcdName $ unL at) subdocs ]  ++

            [ ppFunSig summary links loc doc n typ unicode quali
              | L _ (TypeSig (L _ n) (L _ typ)) <- sigs
              , let doc = lookupAnySubdoc n subdocs ]
          )
  where
    hdr = ppClassHdr summary lctxt (unLoc lname) tvs fds unicode quali
    nm  = unLoc lname
ppShortClassDecl _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"



ppClassDecl :: Bool -> LinksInfo -> [DocInstance DocName] -> SrcSpan
            -> Maybe (Doc DocName) -> [(DocName, DocForDecl DocName)]
            -> TyClDecl DocName -> Bool -> Qualification -> Html
ppClassDecl summary links instances loc mbDoc subdocs
        decl@(ClassDecl lctxt lname ltyvars lfds lsigs _ ats _) unicode quali
  | summary = ppShortClassDecl summary links decl loc subdocs unicode quali
  | otherwise = classheader +++ maybeDocSection quali mbDoc
                  +++ atBit +++ methodBit  +++ instancesBit
  where
    classheader
      | null lsigs = topDeclElem links loc nm (hdr unicode quali)
      | otherwise  = topDeclElem links loc nm (hdr unicode quali <+> keyword "where")

    nm   = unLoc $ tcdLName decl

    hdr = ppClassHdr summary lctxt (unLoc lname) ltyvars lfds

    atBit = subAssociatedTypes [ ppAssocType summary links doc at unicode quali
                      | at <- ats
                      , let doc = lookupAnySubdoc (tcdName $ unL at) subdocs ]

    methodBit = subMethods [ ppFunSig summary links loc doc n typ unicode quali
                      | L _ (TypeSig (L _ n) (L _ typ)) <- lsigs
                      , let doc = lookupAnySubdoc n subdocs ]

    instancesBit = ppInstances instances nm unicode quali 

ppClassDecl _ _ _ _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


ppInstances :: [DocInstance DocName] -> DocName -> Bool -> Qualification -> Html
ppInstances instances baseName unicode quali
  = subInstances quali instName (map instDecl instances)
  where
    instName = getOccString $ getName baseName
    instDecl :: DocInstance DocName -> SubDecl
    instDecl (inst, maybeDoc) = (instHead inst, maybeDoc, [])
    instHead ([],   n, ts) = ppAppNameTypes n ts unicode quali
    instHead (ctxt, n, ts) = ppContextNoLocs ctxt unicode quali
        <+> ppAppNameTypes n ts unicode quali


lookupAnySubdoc :: (Eq name1) =>
                   name1 -> [(name1, DocForDecl name2)] -> DocForDecl name2
lookupAnySubdoc n subdocs = case lookup n subdocs of
  Nothing -> noDocForDecl
  Just docs -> docs


-------------------------------------------------------------------------------
-- * Data & newtype declarations
-------------------------------------------------------------------------------


-- TODO: print contexts
ppShortDataDecl :: Bool -> LinksInfo -> SrcSpan -> TyClDecl DocName -> Bool
                -> Qualification -> Html
ppShortDataDecl summary _links _loc dataDecl unicode quali

  | [] <- cons = dataHeader 

  | [lcon] <- cons, ResTyH98 <- resTy,
    (cHead,cBody,cFoot) <- ppShortConstrParts summary (unLoc lcon) unicode quali
       = (dataHeader <+> equals <+> cHead) +++ cBody +++ cFoot

  | ResTyH98 <- resTy = dataHeader
      +++ shortSubDecls (zipWith doConstr ('=':repeat '|') cons)

  | otherwise = (dataHeader <+> keyword "where")
      +++ shortSubDecls (map doGADTConstr cons)

  where
    dataHeader = ppDataHeader summary dataDecl unicode quali
    doConstr c con = toHtml [c] <+> ppShortConstr summary (unLoc con) unicode quali
    doGADTConstr con = ppShortConstr summary (unLoc con) unicode quali

    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons


ppDataDecl :: Bool -> LinksInfo -> [DocInstance DocName] ->
              [(DocName, DocForDecl DocName)] ->
              SrcSpan -> Maybe (Doc DocName) -> TyClDecl DocName -> Bool ->
              Qualification -> Html
ppDataDecl summary links instances subdocs loc mbDoc dataDecl unicode quali

  | summary   = ppShortDataDecl summary links loc dataDecl unicode quali
  | otherwise = header_ +++ maybeDocSection quali mbDoc +++ constrBit +++ instancesBit

  where
    docname   = unLoc . tcdLName $ dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons

    header_ = topDeclElem links loc docname (ppDataHeader summary dataDecl unicode quali
             <+> whereBit)

    whereBit
      | null cons = noHtml
      | otherwise = case resTy of
        ResTyGADT _ -> keyword "where"
        _ -> noHtml

    constrBit = subConstructors quali
      (map (ppSideBySideConstr subdocs unicode quali) cons)

    instancesBit = ppInstances instances docname unicode quali



ppShortConstr :: Bool -> ConDecl DocName -> Bool -> Qualification -> Html
ppShortConstr summary con unicode quali = cHead <+> cBody <+> cFoot
  where
    (cHead,cBody,cFoot) = ppShortConstrParts summary con unicode quali


-- returns three pieces: header, body, footer so that header & footer can be
-- incorporated into the declaration
ppShortConstrParts :: Bool -> ConDecl DocName -> Bool -> Qualification -> (Html, Html, Html)
ppShortConstrParts summary con unicode quali = case con_res con of
  ResTyH98 -> case con_details con of
    PrefixCon args ->
      (header_ unicode quali +++ hsep (ppBinder summary occ
            : map (ppLParendType unicode quali) args), noHtml, noHtml)
    RecCon fields ->
      (header_ unicode quali +++ ppBinder summary occ <+> char '{',
       doRecordFields fields,
       char '}')
    InfixCon arg1 arg2 ->
      (header_ unicode quali +++ hsep [ppLParendType unicode quali arg1,
            ppBinder summary occ, ppLParendType unicode quali arg2],
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
                            ppForAll forall ltvs lcontext unicode quali <+> char '{',
                            doRecordFields fields,
                            char '}' <+> arrow unicode <+> ppLType unicode quali resTy)
    InfixCon arg1 arg2 -> (doGADTCon [arg1, arg2] resTy, noHtml, noHtml)

  where
    doRecordFields fields = shortSubDecls (map (ppShortField summary unicode quali) fields)
    doGADTCon args resTy = ppBinder summary occ <+> dcolon unicode <+> hsep [
                             ppForAll forall ltvs lcontext unicode quali,
                             ppLType unicode quali (foldr mkFunTy resTy args) ]

    header_  = ppConstrHdr forall tyVars context
    occ      = docNameOcc . unLoc . con_name $ con
    ltvs     = con_qvars con
    tyVars   = tyvarNames ltvs
    lcontext = con_cxt con
    context  = unLoc (con_cxt con)
    forall   = con_explicit con
    mkFunTy a b = noLoc (HsFunTy a b)


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
#if __GLASGOW_HASKELL__ == 612
ppConstrHdr :: HsExplicitForAll -> [Name] -> HsContext DocName -> Bool
            -> Qualification -> Html
#else
ppConstrHdr :: HsExplicitFlag -> [Name] -> HsContext DocName -> Bool
            -> Qualification -> Html
#endif
ppConstrHdr forall tvs ctxt unicode quali
 = (if null tvs then noHtml else ppForall)
   +++
   (if null ctxt then noHtml else ppContextNoArrow ctxt unicode quali
        <+> darrow unicode +++ toHtml " ")
  where
    ppForall = case forall of
      Explicit -> forallSymbol unicode <+> hsep (map ppName tvs) <+> toHtml ". "
      Implicit -> noHtml


ppSideBySideConstr :: [(DocName, DocForDecl DocName)] -> Bool -> Qualification
                   -> LConDecl DocName -> SubDecl
ppSideBySideConstr subdocs unicode quali (L _ con) = (decl, mbDoc, fieldPart)
 where
    decl = case con_res con of
      ResTyH98 -> case con_details con of
        PrefixCon args ->
          hsep ((header_ unicode quali +++ ppBinder False occ)
            : map (ppLParendType unicode quali) args)

        RecCon _ -> header_ unicode quali +++ ppBinder False occ

        InfixCon arg1 arg2 ->
          hsep [header_ unicode quali +++ ppLParendType unicode quali arg1,
            ppBinder False occ,
            ppLParendType unicode quali arg2]

      ResTyGADT resTy -> case con_details con of
        -- prefix & infix could also use hsConDeclArgTys if it seemed to
        -- simplify the code.
        PrefixCon args -> doGADTCon args resTy
        cd@(RecCon _) -> doGADTCon (hsConDeclArgTys cd) resTy
        InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy

    fieldPart = case con_details con of
        RecCon fields -> [doRecordFields fields]
        _ -> []

    doRecordFields fields = subFields quali
      (map (ppSideBySideField subdocs unicode quali) fields)
    doGADTCon :: [LHsType DocName] -> Located (HsType DocName) -> Html
    doGADTCon args resTy =
      ppBinder False occ <+> dcolon unicode
        <+> hsep [ppForAll forall ltvs (con_cxt con) unicode quali,
                  ppLType unicode quali (foldr mkFunTy resTy args) ]

    header_ = ppConstrHdr forall tyVars context
    occ     = docNameOcc . unLoc . con_name $ con
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall  = con_explicit con
    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    -- 'join' is in Maybe.
    mbDoc = join $ fmap fst $ lookup (unLoc $ con_name con) subdocs
    mkFunTy a b = noLoc (HsFunTy a b)


ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Bool -> Qualification
                  -> ConDeclField DocName ->  SubDecl
ppSideBySideField subdocs unicode quali (ConDeclField (L _ name) ltype _) =
  (ppBinder False (docNameOcc name) <+> dcolon unicode <+> ppLType unicode quali ltype,
    mbDoc,
    [])
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    mbDoc = join $ fmap fst $ lookup name subdocs


ppShortField :: Bool -> Bool -> Qualification -> ConDeclField DocName -> Html
ppShortField summary unicode quali (ConDeclField (L _ name) ltype _)
  = ppBinder summary (docNameOcc name)
    <+> dcolon unicode <+> ppLType unicode quali ltype


-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: Bool -> TyClDecl DocName -> Bool -> Qualification -> Html
ppDataHeader summary decl unicode quali
  | not (isDataDecl decl) = error "ppDataHeader: illegal argument"
  | otherwise =
    -- newtype or data
    (if tcdND decl == NewType then keyword "newtype" else keyword "data") <+>
    -- context
    ppLContext (tcdCtxt decl) unicode quali <+>
    -- T a b c ..., or a :+: b
    ppTyClBinderWithVars summary decl


--------------------------------------------------------------------------------
-- * Types and contexts
--------------------------------------------------------------------------------


ppKind :: Outputable a => a -> Html
ppKind k = toHtml $ showSDoc (ppr k)


ppBang :: HsBang -> Html
ppBang HsNoBang = noHtml
ppBang _        = toHtml "!" -- Unpacked args is an implementation detail,
                             -- so we just show the strictness annotation


tupleParens :: Boxity -> [Html] -> Html
tupleParens Boxed   = parenList
tupleParens Unboxed = ubxParenList


--------------------------------------------------------------------------------
-- * Rendering of HsType
--------------------------------------------------------------------------------


pREC_TOP, pREC_FUN, pREC_OP, pREC_CON :: Int

pREC_TOP = (0 :: Int)   -- type in ParseIface.y in GHC
pREC_FUN = (1 :: Int)   -- btype in ParseIface.y in GHC
                        -- Used for LH arg of (->)
pREC_OP  = (2 :: Int)   -- Used for arg of any infix operator
                        -- (we don't keep their fixities around)
pREC_CON = (3 :: Int)   -- Used for arg of type applicn:
                        -- always parenthesise unless atomic

maybeParen :: Int           -- Precedence of context
           -> Int           -- Precedence of top-level operator
           -> Html -> Html  -- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
                               | otherwise            = p


ppLType, ppLParendType, ppLFunLhType :: Bool -> Qualification
                                     -> Located (HsType DocName) -> Html
ppLType       unicode quali y = ppType unicode quali (unLoc y)
ppLParendType unicode quali y = ppParendType unicode quali (unLoc y)
ppLFunLhType  unicode quali y = ppFunLhType unicode quali (unLoc y)


ppType, ppParendType, ppFunLhType :: Bool -> Qualification-> HsType DocName -> Html
ppType       unicode quali ty = ppr_mono_ty pREC_TOP ty unicode quali
ppParendType unicode quali ty = ppr_mono_ty pREC_CON ty unicode quali
ppFunLhType  unicode quali ty = ppr_mono_ty pREC_FUN ty unicode quali


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

#if __GLASGOW_HASKELL__ == 612
ppForAll :: HsExplicitForAll -> [Located (HsTyVarBndr DocName)]
#else
ppForAll :: HsExplicitFlag -> [Located (HsTyVarBndr DocName)]
#endif
         -> Located (HsContext DocName) -> Bool -> Qualification -> Html
ppForAll expl tvs cxt unicode quali
  | show_forall = forall_part <+> ppLContext cxt unicode quali
  | otherwise   = ppLContext cxt unicode quali
  where
    show_forall = not (null tvs) && is_explicit
    is_explicit = case expl of {Explicit -> True; Implicit -> False}
    forall_part = hsep (forallSymbol unicode : ppTyVars tvs) +++ dot


ppr_mono_lty :: Int -> LHsType DocName -> Bool -> Qualification -> Html
ppr_mono_lty ctxt_prec ty unicode quali = ppr_mono_ty ctxt_prec (unLoc ty) unicode quali


ppr_mono_ty :: Int -> HsType DocName -> Bool -> Qualification -> Html
ppr_mono_ty ctxt_prec (HsForAllTy expl tvs ctxt ty) unicode quali
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll expl tvs ctxt unicode quali, ppr_mono_lty pREC_TOP ty unicode quali]

ppr_mono_ty _         (HsBangTy b ty)     u q = ppBang b +++ ppLParendType u q ty
ppr_mono_ty _         (HsTyVar name)      _ q = ppDocName q name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   u q = ppr_fun_ty ctxt_prec ty1 ty2 u q
ppr_mono_ty _         (HsTupleTy con tys) u q = tupleParens con (map (ppLType u q) tys)
ppr_mono_ty _         (HsKindSig ty kind) u q =
    parens (ppr_mono_lty pREC_TOP ty u q <+> dcolon u <+> ppKind kind)
ppr_mono_ty _         (HsListTy ty)       u q = brackets (ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty _         (HsPArrTy ty)       u q = pabrackets (ppr_mono_lty pREC_TOP ty u q)
ppr_mono_ty _         (HsPredTy p)        u q = parens (ppPred u q p)
ppr_mono_ty _         (HsNumTy n)         _ _ = toHtml (show n) -- generics only
ppr_mono_ty _         (HsSpliceTy {})     _ _ = error "ppr_mono_ty HsSpliceTy"
#if __GLASGOW_HASKELL__ == 612
ppr_mono_ty _         (HsSpliceTyOut {})  _ _ = error "ppr_mono_ty HsQuasiQuoteTy"
#else
ppr_mono_ty _         (HsQuasiQuoteTy {}) _ _ = error "ppr_mono_ty HsQuasiQuoteTy"
#endif
ppr_mono_ty _         (HsRecTy {})        _ _ = error "ppr_mono_ty HsRecTy"

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode quali
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode quali, ppr_mono_lty pREC_CON arg_ty unicode quali]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2) unicode quali
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode quali <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode quali
  where
    ppr_op = if not (isSymOcc occName) then quote (ppLDocName quali op) else ppLDocName quali op
    occName = docNameOcc . unLoc $ op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode quali
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode quali

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode quali
  = ppr_mono_lty ctxt_prec ty unicode quali


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Bool -> Qualification -> Html
ppr_fun_ty ctxt_prec ty1 ty2 unicode quali
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode quali
        p2 = ppr_mono_lty pREC_TOP ty2 unicode quali
    in
    maybeParen ctxt_prec pREC_FUN $
    hsep [p1, arrow unicode <+> p2]


