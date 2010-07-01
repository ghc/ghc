{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.LaTeX
-- Copyright   :  (c) Simon Marlow 2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.LaTeX (
  ppLaTeX
) where


import Haddock.Types
import Haddock.Utils
import Haddock.GhcUtils
import Pretty hiding (Doc)
import qualified Pretty

import GHC
import OccName
import Name                 ( isTyConName, nameOccName )
import RdrName              ( rdrNameOcc, isRdrTc )
import BasicTypes           ( IPName(..), Boxity(..) )
import Outputable           ( Outputable, ppr, showSDoc )
import FastString           ( unpackFS, unpackLitString )

import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Data.Char
import Control.Monad
import Data.Maybe
import Data.List
-- import Debug.Trace

{- SAMPLE OUTPUT

\haddockmoduleheading{\texttt{Data.List}}
\hrulefill
{\haddockverb\begin{verbatim}
module Data.List (
    (++),  head,  last,  tail,  init,  null,  length,  map,  reverse, 
  ) where\end{verbatim}}
\hrulefill

\section{Basic functions}
\begin{haddockdesc}
\item[\begin{tabular}{@{}l}
head\ ::\ {\char 91}a{\char 93}\ ->\ a
\end{tabular}]\haddockbegindoc
Extract the first element of a list, which must be non-empty.
\par

\end{haddockdesc}
\begin{haddockdesc}
\item[\begin{tabular}{@{}l}
last\ ::\ {\char 91}a{\char 93}\ ->\ a
\end{tabular}]\haddockbegindoc
Extract the last element of a list, which must be finite and non-empty.
\par

\end{haddockdesc}
-}


{- TODO
 * don't forget fixity!!
-}

ppLaTeX :: String                       -- Title
        -> Maybe String                 -- Package name
        -> [Interface]
        -> FilePath                     -- destination directory
        -> Maybe (Doc GHC.RdrName)      -- prologue text, maybe
        -> Maybe String                 -- style file
        -> FilePath
        -> IO ()

ppLaTeX title packageStr visible_ifaces odir prologue maybe_style libdir
 = do
   createDirectoryIfMissing True odir
   when (isNothing maybe_style) $
     copyFile (libdir </> "latex" </> haddockSty) (odir </> haddockSty)
   ppLaTeXTop title packageStr odir prologue maybe_style visible_ifaces
   mapM_ (ppLaTeXModule title odir) visible_ifaces

haddockSty = "haddock.sty"

type LaTeX = Pretty.Doc

ppLaTeXTop
   :: String
   -> Maybe String
   -> FilePath
   -> Maybe (Doc GHC.RdrName)
   -> Maybe String
   -> [Interface]
   -> IO ()

ppLaTeXTop doctitle packageStr odir prologue maybe_style ifaces = do

  let tex = vcat [
        text "\\documentclass{book}",
        text "\\usepackage" <> braces (maybe (text "haddock") text maybe_style),
        text "\\begin{document}",
        text "\\begin{titlepage}",
        text "\\begin{haddocktitle}",
        text doctitle,
        text "\\end{haddocktitle}",
        case prologue of
           Nothing -> empty
           Just d  -> vcat [text "\\begin{haddockprologue}",
                            rdrDocToLaTeX d,
                            text "\\end{haddockprologue}"],
        text "\\end{titlepage}",
        text "\\tableofcontents",
        vcat [ text "\\input" <> braces (text mdl) | mdl <- mods ],
        text "\\end{document}"
        ]

      mods = sort (map (moduleBasename.ifaceMod) ifaces)

      filename = odir </> (fromMaybe "haddock" packageStr <.> "tex")

  writeFile filename (render tex)

ppLaTeXModule :: String -> FilePath -> Interface -> IO ()
ppLaTeXModule _title odir iface = do
  createDirectoryIfMissing True odir
  let
      mdl = ifaceMod iface
      mdl_str = moduleString mdl

      exports = ifaceRnExportItems iface

      tex = vcat [
        text "\\haddockmoduleheading" <> braces (text mdl_str),
        text "\\label{module:" <> text mdl_str <> char '}',
        text "\\haddockbeginheader",
        verb $ vcat [
           text "module" <+> text mdl_str <+> lparen,
           text "    " <> fsep (punctuate (text ", ") $
                               map exportListItem $
                               filter forSummary exports),
           text "  ) where"
         ],
        text "\\haddockendheader" $$ text "",
        description,
        body
       ]

      description
          = case ifaceRnDoc iface of
              Nothing -> empty
              Just doc -> docToLaTeX doc

      body = processExports exports
  --
  writeFile (odir </> moduleLaTeXFile mdl) (fullRender PageMode 80 1 string_txt "" tex)


string_txt :: TextDetails -> String -> String
string_txt (Chr c)   s  = c:s
string_txt (Str s1)  s2 = s1 ++ s2
string_txt (PStr s1) s2 = unpackFS s1 ++ s2
string_txt (LStr s1 _) s2 = unpackLitString s1 ++ s2

exportListItem :: ExportItem DocName -> LaTeX
exportListItem (ExportDecl decl _doc subdocs _insts)
  = ppDocBinder (declName decl) <>
     case subdocs of
       [] -> empty
       xs -> parens (sep (punctuate comma (map (ppDocBinder . fst) subdocs)))
exportListItem (ExportNoDecl y [])
  = ppDocBinder y
exportListItem (ExportNoDecl y subs)
  = ppDocBinder y <> parens (sep (punctuate comma (map ppDocBinder subs)))
exportListItem (ExportModule mdl)
  = text "module" <+> text (moduleString mdl)
exportListItem _
  = error "exportListItem"

-- Deal with a group of undocumented exports together, to avoid lots
-- of blank vertical space between them.
processExports :: [ExportItem DocName] -> LaTeX
processExports [] = empty
processExports (decl : es)
  | Just sig <- isSimpleSig decl
  = multiDecl [ ppTypeSig (getName name) typ False
              | (name,typ) <- sig:sigs ] $$
    processExports es'
  where (sigs, es') = spanWith isSimpleSig es
processExports (ExportModule mdl : es)
  = declWithDoc (vcat [ text "module" <+> text (moduleString m) | m <- mdl:mdls ]) Nothing $$
    processExports es'
  where (mdls, es') = spanWith isExportModule es
processExports (e : es) =
  processExport e $$ processExports es

isSimpleSig :: ExportItem DocName -> Maybe (DocName, HsType DocName)
isSimpleSig (ExportDecl (L _ (SigD (TypeSig (L _ n) (L _ t))))
                        (Nothing, argDocs) _ _)
  | Map.null argDocs = Just (n, t)
isSimpleSig _ = Nothing

isExportModule :: ExportItem DocName -> Maybe Module
isExportModule (ExportModule m) = Just m
isExportModule _ = Nothing

processExport :: ExportItem DocName -> LaTeX
processExport (ExportGroup lev _id0 doc)
  = ppDocGroup lev (docToLaTeX doc)
processExport (ExportDecl decl doc subdocs insts)
  = ppDecl decl doc insts subdocs
processExport (ExportNoDecl y [])
  = ppDocName y
processExport (ExportNoDecl y subs)
  = ppDocName y <> parens (sep (punctuate comma (map ppDocName subs)))
processExport (ExportModule mdl)
  = declWithDoc (text "module" <+> text (moduleString mdl)) Nothing
processExport (ExportDoc doc)
  = docToLaTeX doc

ppDocGroup :: Int -> LaTeX -> LaTeX
ppDocGroup lev doc = sec lev <> braces doc
  where sec 1 = text "\\section"
        sec 2 = text "\\subsection"
        sec 3 = text "\\subsubsection"
        sec _ = text "\\paragraph"

declName :: LHsDecl DocName -> DocName
declName (L _ decl) = case decl of
  TyClD d  -> unLoc $ tcdLName d
  SigD (TypeSig (L _ n) _) -> n
  _ -> error "declaration not supported by declName"

forSummary :: (ExportItem DocName) -> Bool
forSummary (ExportGroup _ _ _) = False
forSummary (ExportDoc _)       = False
forSummary _                    = True

moduleLaTeXFile :: Module -> FilePath
moduleLaTeXFile mdl = moduleBasename mdl ++ ".tex"

moduleBasename :: Module -> FilePath
moduleBasename mdl = map (\c -> if c == '.' then '-' else c)
                         (moduleNameString (moduleName mdl))

-- -----------------------------------------------------------------------------
-- Decls

ppDecl :: LHsDecl DocName
       -> DocForDecl DocName
       -> [DocInstance DocName]
       -> [(DocName, DocForDecl DocName)]
       -> LaTeX

ppDecl (L loc decl) (mbDoc, fnArgsDoc) instances subdocs = case decl of
  TyClD d@(TyFamily {})          -> ppTyFam False loc mbDoc d unicode
  TyClD d@(TyData {})
    | Nothing <- tcdTyPats d     -> ppDataDecl instances subdocs loc mbDoc d unicode
    | Just _  <- tcdTyPats d     -> ppDataInst loc mbDoc d
  TyClD d@(TySynonym {})
    | Nothing <- tcdTyPats d     -> ppTySyn loc (mbDoc, fnArgsDoc) d unicode
    | Just _  <- tcdTyPats d     -> ppTyInst False loc mbDoc d unicode
  TyClD d@(ClassDecl {})         -> ppClassDecl instances loc mbDoc subdocs d unicode
  SigD (TypeSig (L _ n) (L _ t)) -> ppFunSig loc (mbDoc, fnArgsDoc) n t unicode False
  ForD d                         -> ppFor loc (mbDoc, fnArgsDoc) d unicode
  InstD _                        -> empty
  _                              -> error "declaration not supported by ppDecl"
  where
    unicode = False

ppTyFam _ _ _ _ _ =
  error "type family declarations are currently not supported by --latex"

ppDataInst _ _ _ =
  error "data instance declarations are currently not supported by --latex"

ppTyInst _ _ _ _ _ =
  error "type instance declarations are currently not supported by --latex"

ppFor _ _ _ _ =
  error "foreign declarations are currently not supported by --latex"

-- -----------------------------------------------------------------------------
-- Type Synonyms

-- we skip type patterns for now
ppTySyn :: SrcSpan -> DocForDecl DocName -> TyClDecl DocName -> Bool -> LaTeX

ppTySyn loc doc (TySynonym (L _ name) ltyvars _ ltype) unicode
  = ppTypeOrFunSig loc name (unLoc ltype) doc
                   (full, hdr, char '=') unicode False
  where
    hdr  = hsep (keyword "type" : ppDocBinder name : ppTyVars ltyvars)
    full = hdr <+> char '=' <+> ppLType unicode ltype

ppTySyn _ _ _ _ = error "declaration not supported by ppTySyn"

-- -----------------------------------------------------------------------------
-- Function signatures

ppFunSig :: SrcSpan -> DocForDecl DocName -> DocName -> HsType DocName
         -> Bool -> Bool
         -> LaTeX
ppFunSig loc doc docname typ unicode methods =
  ppTypeOrFunSig loc docname typ doc
    (ppTypeSig name typ False, ppSymName name, dcolon unicode)
    unicode methods
 where
   name = getName docname

ppTypeOrFunSig :: SrcSpan -> DocName -> HsType DocName ->
                  DocForDecl DocName -> (LaTeX, LaTeX, LaTeX)
               -> Bool -> Bool -> LaTeX
ppTypeOrFunSig _loc _docname typ (doc, argDocs) (pref1, pref2, sep0)
               unicode methods
  | Map.null argDocs =
      declWithDoc pref1 (fmap docToLaTeX doc)
  | otherwise        =
      declWithDoc pref2 $ Just $
        text "\\haddockbeginargs" $$
        do_args 0 sep0 typ $$
        text "\\end{tabulary}\\par" $$
        maybe empty docToLaTeX doc
  where
     do_largs n leader (L _ t) = do_args n leader t

     arg_doc n = rDoc (Map.lookup n argDocs)

     do_args :: Int -> LaTeX -> (HsType DocName) -> LaTeX
     do_args n leader (HsForAllTy Explicit tvs lctxt ltype)
       = decltt leader <->
             decltt (hsep (forallSymbol unicode : ppTyVars tvs ++ [dot]) <+>
                ppLContextNoArrow lctxt unicode) <+> nl $$
         do_largs n (darrow unicode) ltype

     do_args n leader (HsForAllTy Implicit _ lctxt ltype)
       | not (null (unLoc lctxt))
       = decltt leader <-> decltt (ppLContextNoArrow lctxt unicode) <+> nl $$
         do_largs n (darrow unicode) ltype
         -- if we're not showing any 'forall' or class constraints or
         -- anything, skip having an empty line for the context.
       | otherwise
       = do_largs n leader ltype
     do_args n leader (HsFunTy lt r)
       = decltt leader <-> decltt (ppLFunLhType unicode lt) <-> arg_doc n <+> nl $$
         do_largs (n+1) (arrow unicode) r
     do_args n leader t
       = decltt leader <-> decltt (ppType unicode t) <-> arg_doc n <+> nl

ppTypeSig :: Name -> HsType DocName  -> Bool -> LaTeX
ppTypeSig nm ty unicode =
  ppSymName nm <+> dcolon unicode <+> ppType unicode ty

ppTyVars :: [LHsTyVarBndr DocName] -> [LaTeX]
ppTyVars tvs = map ppSymName (tyvarNames tvs)

tyvarNames :: [LHsTyVarBndr DocName] -> [Name]
tyvarNames = map (getName . hsTyVarName . unLoc)

declWithDoc :: LaTeX -> Maybe LaTeX -> LaTeX
declWithDoc decl doc =
   text "\\begin{haddockdesc}" $$
   text "\\item[\\begin{tabular}{@{}l}" $$
   text (latexMonoFilter (render decl)) $$
   text "\\end{tabular}]" <>
       (if isNothing doc then empty else text "\\haddockbegindoc") $$
   maybe empty id doc $$
   text "\\end{haddockdesc}"

-- in a group of decls, we don't put them all in the same tabular,
-- because that would prevent the group being broken over a page
-- boundary (breaks Foreign.C.Error for example).
multiDecl :: [LaTeX] -> LaTeX
multiDecl decls =
   text "\\begin{haddockdesc}" $$
   vcat [
      text "\\item[" $$
      text (latexMonoFilter (render decl)) $$
      text "]"
      | decl <- decls ] $$
   text "\\end{haddockdesc}"

-------------------------------------------------------------------------------
-- Rendering Doc

maybeDoc :: Maybe (Doc DocName) -> LaTeX
maybeDoc = maybe empty docToLaTeX

-- for table cells, we strip paragraphs out to avoid extra vertical space
-- and don't add a quote environment.
rDoc  :: Maybe (Doc DocName) -> LaTeX
rDoc = maybeDoc . fmap latexStripTrailingWhitespace

-------------------------------------------------------------------------------
-- Class declarations
-------------------------------------------------------------------------------

ppClassHdr :: Bool -> Located [LHsPred DocName] -> DocName
           -> [Located (HsTyVarBndr DocName)] -> [Located ([DocName], [DocName])]
           -> Bool -> LaTeX
ppClassHdr summ lctxt n tvs fds unicode =
  keyword "class"
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode else empty)
  <+> ppAppDocNameNames summ n (tyvarNames $ tvs)
  <+> ppFds fds unicode


ppFds :: [Located ([DocName], [DocName])] -> Bool -> LaTeX
ppFds fds unicode =
  if null fds then empty else
    char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
    fundep (vars1,vars2) = hsep (map ppDocName vars1) <+> arrow unicode <+>
                           hsep (map ppDocName vars2)


ppClassDecl :: [DocInstance DocName] -> SrcSpan
            -> Maybe (Doc DocName) -> [(DocName, DocForDecl DocName)]
            -> TyClDecl DocName -> Bool -> LaTeX
ppClassDecl instances loc mbDoc subdocs
  decl@(ClassDecl lctxt lname ltyvars lfds lsigs _ ats _) unicode
  = declWithDoc classheader (if null body then Nothing else Just (vcat body)) $$
    instancesBit
  where
    classheader
      | null lsigs = hdr unicode
      | otherwise  = hdr unicode <+> keyword "where"

    nm   = unLoc $ tcdLName decl

    hdr = ppClassHdr False lctxt (unLoc lname) ltyvars lfds

    body = catMaybes [fmap docToLaTeX mbDoc, body_]

    body_
      | null lsigs, null ats = Nothing
      | null ats  = Just methodTable
--      | otherwise = atTable $$ methodTable 
 
    methodTable =
      text "\\haddockpremethods{}\\textbf{Methods}" $$
      vcat  [ ppFunSig loc doc n typ unicode True
            | L _ (TypeSig (L _ n) (L _ typ)) <- lsigs
            , let doc = lookupAnySubdoc n subdocs ]

--    atTable = abovesSep s8 $ [ ppAssocType summary links doc at unicode | at <- ats
--                             , let doc = lookupAnySubdoc (tcdName $ unL at) subdocs ]

    instancesBit
      | null instances = empty
      | all (isNothing . snd) instances =
        declWithDoc (vcat (map (ppInstDecl unicode) (map fst instances))) Nothing
      | otherwise = vcat (map (ppDocInstance unicode) instances)

ppClassDecl _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"


-- | Print a possibly commented instance. The instance header is printed inside
-- an 'argBox'. The comment is printed to the right of the box in normal comment
-- style.
ppDocInstance :: Bool -> DocInstance DocName -> LaTeX
ppDocInstance unicode (instHead, mbDoc) =
  declWithDoc (ppInstDecl unicode instHead) (fmap docToLaTeX mbDoc)

ppInstDecl :: Bool -> InstHead DocName -> LaTeX
ppInstDecl unicode instHead = keyword "instance" <+> ppInstHead unicode instHead

ppInstHead :: Bool -> InstHead DocName -> LaTeX
ppInstHead unicode ([],   n, ts) = ppAppNameTypes n ts unicode
ppInstHead unicode (ctxt, n, ts) = ppContextNoLocs ctxt unicode <+> ppAppNameTypes n ts unicode

lookupAnySubdoc :: (Eq name1) =>
                   name1 -> [(name1, DocForDecl name2)] -> DocForDecl name2
lookupAnySubdoc n subdocs = case lookup n subdocs of
  Nothing -> noDocForDecl
  Just docs -> docs

-- -----------------------------------------------------------------------------
-- Data & newtype declarations


ppDataDecl :: [DocInstance DocName] ->
              [(DocName, DocForDecl DocName)] ->
              SrcSpan -> Maybe (Doc DocName) -> TyClDecl DocName -> Bool ->
              LaTeX
ppDataDecl instances subdocs loc mbDoc dataDecl unicode

   =  declWithDoc (ppDataHeader dataDecl unicode <+> whereBit)
                  (if null body then Nothing else Just (vcat body))
   $$ instancesBit

  where
    docname   = unLoc . tcdLName $ dataDecl
    cons      = tcdCons dataDecl
    resTy     = (con_res . unLoc . head) cons

    body = catMaybes [constrBit, fmap docToLaTeX mbDoc]

    (whereBit, leaders)
      | null cons = (empty,[])
      | otherwise = case resTy of
        ResTyGADT _ -> (decltt (keyword "where"), repeat empty)
        _           -> (empty, (decltt (text "=") : repeat (decltt (text "|"))))

    constrBit
      | null cons = Nothing
      | otherwise = Just $
          text "\\haddockbeginconstrs" $$
          vcat (zipWith (ppSideBySideConstr subdocs unicode) leaders cons) $$
          text "\\end{tabulary}\\par"

    instancesBit
      | null instances = empty
      | all (isNothing . snd) instances =
        declWithDoc (vcat (map (ppInstDecl unicode) (map fst instances))) Nothing
      | otherwise = vcat (map (ppDocInstance unicode) instances)

isRecCon :: Located (ConDecl a) -> Bool
isRecCon lcon = case con_details (unLoc lcon) of
  RecCon _ -> True
  _ -> False


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
#if __GLASGOW_HASKELL__ == 612
ppConstrHdr :: HsExplicitForAll -> [Name] -> HsContext DocName -> Bool -> LaTeX
#else
ppConstrHdr :: HsExplicitFlag -> [Name] -> HsContext DocName -> Bool -> LaTeX
#endif
ppConstrHdr forall tvs ctxt unicode
 = (if null tvs then empty else ppForall)
   <+>
   (if null ctxt then empty else ppContextNoArrow ctxt unicode <+> darrow unicode <+> text " ")
  where
    ppForall = case forall of
      Explicit -> forallSymbol unicode <+> hsep (map ppName tvs) <+> text ". "
      Implicit -> empty

ppSideBySideConstr :: [(DocName, DocForDecl DocName)] -> Bool -> LaTeX
                   -> LConDecl DocName -> LaTeX
ppSideBySideConstr subdocs unicode leader (L _ con) =
  leader <->
  case con_res con of
  ResTyH98 -> case con_details con of

    PrefixCon args ->
      decltt (hsep ((header_ unicode <+> ppBinder occ) :
                 map (ppLParendType unicode) args))
      <-> rDoc mbDoc <+> nl

    RecCon fields ->
      (decltt (header_ unicode <+> ppBinder occ)
        <-> rDoc mbDoc <+> nl)
      $$
      doRecordFields fields

    InfixCon arg1 arg2 ->
      decltt (hsep [ header_ unicode <+> ppLParendType unicode arg1,
                 ppBinder occ,
                 ppLParendType unicode arg2 ])
      <-> rDoc mbDoc <+> nl

  ResTyGADT resTy -> case con_details con of
    -- prefix & infix could also use hsConDeclArgTys if it seemed to
    -- simplify the code.
    PrefixCon args -> doGADTCon args resTy
    cd@(RecCon fields) -> doGADTCon (hsConDeclArgTys cd) resTy <+> nl $$
                                     doRecordFields fields
    InfixCon arg1 arg2 -> doGADTCon [arg1, arg2] resTy

 where
    doRecordFields fields =
        vcat (map (ppSideBySideField subdocs unicode) fields)

    doGADTCon args resTy = decltt (ppBinder occ <+> dcolon unicode <+> hsep [
                               ppForAll forall ltvs (con_cxt con) unicode,
                               ppLType unicode (foldr mkFunTy resTy args) ]
                            ) <-> rDoc mbDoc


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

ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Bool -> ConDeclField DocName ->  LaTeX
ppSideBySideField subdocs unicode (ConDeclField (L _ name) ltype _) =
  decltt (ppBinder (docNameOcc name)
    <+> dcolon unicode <+> ppLType unicode ltype) <-> rDoc mbDoc
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    mbDoc = join $ fmap fst $ lookup name subdocs

-- {-
-- ppHsFullConstr :: HsConDecl -> LaTeX
-- ppHsFullConstr (HsConDecl _ nm tvs ctxt typeList doc) = 
--      declWithDoc False doc (
-- 	hsep ((ppHsConstrHdr tvs ctxt +++ 
-- 		ppHsBinder False nm) : map ppHsBangType typeList)
--       )
-- ppHsFullConstr (HsRecDecl _ nm tvs ctxt fields doc) =
--    td << vanillaTable << (
--      case doc of
--        Nothing -> aboves [hdr, fields_html]
--        Just _  -> aboves [hdr, constr_doc, fields_html]
--    )
-- 
--   where hdr = declBox (ppHsConstrHdr tvs ctxt +++ ppHsBinder False nm)
-- 
-- 	constr_doc	
-- 	  | isJust doc = docBox (docToLaTeX (fromJust doc))
-- 	  | otherwise  = LaTeX.emptyTable
-- 
-- 	fields_html = 
-- 	   td << 
-- 	      table ! [width "100%", cellpadding 0, cellspacing 8] << (
-- 		   aboves (map ppFullField (concat (map expandField fields)))
-- 		)
-- -}
-- 
-- ppShortField :: Bool -> Bool -> ConDeclField DocName -> LaTeX
-- ppShortField summary unicode (ConDeclField (L _ name) ltype _)
--   = tda [theclass "recfield"] << (
--       ppBinder summary (docNameOcc name)
--       <+> dcolon unicode <+> ppLType unicode ltype
--     )
-- 
-- {-
-- ppFullField :: HsFieldDecl -> LaTeX
-- ppFullField (HsFieldDecl [n] ty doc) 
--   = declWithDoc False doc (
-- 	ppHsBinder False n <+> dcolon <+> ppHsBangType ty
--     )
-- ppFullField _ = error "ppFullField"
-- 
-- expandField :: HsFieldDecl -> [HsFieldDecl]
-- expandField (HsFieldDecl ns ty doc) = [ HsFieldDecl [n] ty doc | n <- ns ]
-- -}

-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: TyClDecl DocName -> Bool -> LaTeX
ppDataHeader decl unicode
  | not (isDataDecl decl) = error "ppDataHeader: illegal argument"
  | otherwise =
    -- newtype or data
    (if tcdND decl == NewType then keyword "newtype" else keyword "data") <+>
    -- context
    ppLContext (tcdCtxt decl) unicode <+>
    -- T a b c ..., or a :+: b
    ppTyClBinderWithVars False decl


--------------------------------------------------------------------------------
-- TyClDecl helpers
--------------------------------------------------------------------------------


-- | Print a type family / newtype / data / class binder and its variables 
ppTyClBinderWithVars :: Bool -> TyClDecl DocName -> LaTeX
ppTyClBinderWithVars summ decl =
  ppAppDocNameNames summ (unLoc $ tcdLName decl) (tyvarNames $ tcdTyVars decl)


--------------------------------------------------------------------------------
-- Type applications
--------------------------------------------------------------------------------


-- | Print an application of a DocName and a list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocName] -> Bool -> LaTeX
ppAppNameTypes n ts unicode = ppTypeApp n ts ppDocName (ppParendType unicode)


-- | Print an application of a DocName and a list of Names 
ppAppDocNameNames :: Bool -> DocName -> [Name] -> LaTeX
ppAppDocNameNames _summ n ns =
  ppTypeApp n ns (ppBinder . docNameOcc) ppSymName


-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (DocName -> LaTeX) -> (a -> LaTeX) -> LaTeX
ppTypeApp n (t1:t2:rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator                    = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN n <+> ppT t2

ppTypeApp n ts ppDN ppT = ppDN n <+> hsep (map ppT ts)


-------------------------------------------------------------------------------
-- Contexts
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Bool -> LaTeX
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc


ppContextNoArrow :: HsContext DocName -> Bool -> LaTeX
ppContextNoArrow []  _ = empty
ppContextNoArrow cxt unicode = pp_hs_context (map unLoc cxt) unicode


ppContextNoLocs :: [HsPred DocName] -> Bool -> LaTeX
ppContextNoLocs []  _ = empty
ppContextNoLocs cxt unicode = pp_hs_context cxt unicode <+> darrow unicode


ppContext :: HsContext DocName -> Bool -> LaTeX
ppContext cxt unicode = ppContextNoLocs (map unLoc cxt) unicode


pp_hs_context :: [HsPred DocName] -> Bool -> LaTeX
pp_hs_context []  _       = empty
pp_hs_context [p] unicode = ppPred unicode p
pp_hs_context cxt unicode = parenList (map (ppPred unicode) cxt)


ppPred :: Bool -> HsPred DocName -> LaTeX
ppPred unicode (HsClassP n ts) = ppAppNameTypes n (map unLoc ts) unicode
ppPred unicode (HsEqualP t1 t2) = ppLType unicode t1 <> text "~" <> ppLType unicode t2
ppPred unicode (HsIParam (IPName n) t)
  = char '?' <> ppDocName n <> dcolon unicode <> ppLType unicode t


-- ----------------------------------------------------------------------------
-- Types and contexts

ppKind :: Outputable a => a -> LaTeX
ppKind k = text (showSDoc (ppr k))

ppBang :: HsBang -> LaTeX
ppBang HsNoBang = empty
ppBang _        = char '!' -- Unpacked args is an implementation detail,

tupleParens :: Boxity -> [LaTeX] -> LaTeX
tupleParens Boxed   = parenList
tupleParens Unboxed = ubxParenList

-- -----------------------------------------------------------------------------
-- Rendering of HsType
--
-- Stolen from Html and tweaked for LaTeX generation

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
           -> LaTeX -> LaTeX  -- Wrap in parens if (ctxt >= op)
maybeParen ctxt_prec op_prec p | ctxt_prec >= op_prec = parens p
                               | otherwise            = p


ppLType, ppLParendType, ppLFunLhType :: Bool -> Located (HsType DocName) -> LaTeX
ppLType       unicode y = ppType unicode (unLoc y)
ppLParendType unicode y = ppParendType unicode (unLoc y)
ppLFunLhType  unicode y = ppFunLhType unicode (unLoc y)


ppType, ppParendType, ppFunLhType :: Bool -> HsType DocName -> LaTeX
ppType       unicode ty = ppr_mono_ty pREC_TOP ty unicode
ppParendType unicode ty = ppr_mono_ty pREC_CON ty unicode
ppFunLhType  unicode ty = ppr_mono_ty pREC_FUN ty unicode


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

#if __GLASGOW_HASKELL__ == 612
ppForAll :: HsExplicitForAll -> [Located (HsTyVarBndr DocName)]
#else
ppForAll :: HsExplicitFlag -> [Located (HsTyVarBndr DocName)]
#endif
         -> Located (HsContext DocName) -> Bool -> LaTeX
ppForAll expl tvs cxt unicode
  | show_forall = forall_part <+> ppLContext cxt unicode
  | otherwise   = ppLContext cxt unicode
  where
    show_forall = not (null tvs) && is_explicit
    is_explicit = case expl of {Explicit -> True; Implicit -> False}
    forall_part = hsep (forallSymbol unicode : ppTyVars tvs) <> dot


ppr_mono_lty :: Int -> LHsType DocName -> Bool -> LaTeX
ppr_mono_lty ctxt_prec ty unicode = ppr_mono_ty ctxt_prec (unLoc ty) unicode


ppr_mono_ty :: Int -> HsType DocName -> Bool -> LaTeX
ppr_mono_ty ctxt_prec (HsForAllTy expl tvs ctxt ty) unicode
  = maybeParen ctxt_prec pREC_FUN $
    hsep [ppForAll expl tvs ctxt unicode, ppr_mono_lty pREC_TOP ty unicode]

ppr_mono_ty _         (HsBangTy b ty)     u = ppBang b <> ppLParendType u ty
ppr_mono_ty _         (HsTyVar name)      _ = ppDocName name
ppr_mono_ty ctxt_prec (HsFunTy ty1 ty2)   u = ppr_fun_ty ctxt_prec ty1 ty2 u
ppr_mono_ty _         (HsTupleTy con tys) u = tupleParens con (map (ppLType u) tys)
ppr_mono_ty _         (HsKindSig ty kind) u = parens (ppr_mono_lty pREC_TOP ty u <+> dcolon u <+> ppKind kind)
ppr_mono_ty _         (HsListTy ty)       u = brackets (ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsPArrTy ty)       u = pabrackets (ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsPredTy p)        u = parens (ppPred u p)
ppr_mono_ty _         (HsNumTy n)         _ = text (show n) -- generics only
ppr_mono_ty _         (HsSpliceTy {})     _ = error "ppr_mono_ty HsSpliceTy"
#if __GLASGOW_HASKELL__ == 612
ppr_mono_ty _         (HsSpliceTyOut {})  _ = error "ppr_mono_ty HsQuasiQuoteTy"
#else
ppr_mono_ty _         (HsQuasiQuoteTy {}) _ = error "ppr_mono_ty HsQuasiQuoteTy"
#endif
ppr_mono_ty _         (HsRecTy {})        _ = error "ppr_mono_ty HsRecTy"

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode, ppr_mono_lty pREC_CON arg_ty unicode]

ppr_mono_ty ctxt_prec (HsOpTy ty1 op ty2) unicode
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode
  where
    ppr_op = if not (isSymOcc occName) then char '`' <> ppLDocName op <> char '`' else ppLDocName op
    occName = docNameOcc . unLoc $ op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode
  = ppr_mono_lty ctxt_prec ty unicode


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Bool -> LaTeX
ppr_fun_ty ctxt_prec ty1 ty2 unicode
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode
        p2 = ppr_mono_lty pREC_TOP ty2 unicode
    in
    maybeParen ctxt_prec pREC_FUN $
    sep [p1, arrow unicode <+> p2]

-- -----------------------------------------------------------------------------
-- Names

ppBinder :: OccName -> LaTeX
ppBinder n
  | isVarSym n = parens $ ppOccName n
  | otherwise  = ppOccName n

ppVerbBinder :: OccName -> LaTeX
ppVerbBinder n
  | isVarSym n = parens $ ppVerbOccName n
  | otherwise  = ppVerbOccName n

ppSymName :: Name -> LaTeX
ppSymName name
  | isNameSym name = parens $ ppName name
  | otherwise = ppName name

ppVerbOccName :: OccName -> LaTeX
ppVerbOccName = text . latexFilter . occNameString

ppOccName :: OccName -> LaTeX
ppOccName = text . occNameString

ppVerbDocName :: DocName -> LaTeX
ppVerbDocName = ppVerbOccName . docNameOcc

ppVerbRdrName :: RdrName -> LaTeX
ppVerbRdrName = ppVerbOccName . rdrNameOcc

ppDocName :: DocName -> LaTeX
ppDocName = ppOccName . docNameOcc

ppLDocName :: Located DocName -> LaTeX
ppLDocName (L _ d) = ppDocName d

ppDocBinder :: DocName -> LaTeX
ppDocBinder = ppBinder . docNameOcc

ppVerbDocBinder :: DocName -> LaTeX
ppVerbDocBinder = ppVerbBinder . docNameOcc

ppName :: Name -> LaTeX
ppName = ppOccName . nameOccName

ppVerbName :: Name -> LaTeX
ppVerbName = ppVerbOccName . nameOccName

latexFilter :: String -> String
latexFilter = foldr latexMunge ""

latexMonoFilter :: String -> String
latexMonoFilter = foldr latexMonoMunge ""

latexMunge '#'  s = "{\\char '43}" ++ s
latexMunge '$'  s = "{\\char '44}" ++ s
latexMunge '%'  s = "{\\char '45}" ++ s
latexMunge '&'  s = "{\\char '46}" ++ s
latexMunge '~'  s = "{\\char '176}" ++ s
latexMunge '_'  s = "{\\char '137}" ++ s
latexMunge '^'  s = "{\\char '136}" ++ s
latexMunge '\\' s = "{\\char '134}" ++ s
latexMunge '{'  s = "{\\char '173}" ++ s
latexMunge '}'  s = "{\\char '175}" ++ s
latexMunge '['  s = "{\\char 91}" ++ s
latexMunge ']'  s = "{\\char 93}" ++ s
latexMunge c    s = c : s

latexMonoMunge ' ' s = '\\' : ' ' : s
latexMonoMunge '\n' s = '\\' : '\\' : s
latexMonoMunge c   s = latexMunge c s

-- -----------------------------------------------------------------------------
-- Doc Markup

parLatexMarkup :: (a -> LaTeX) -> (a -> Bool)
               -> DocMarkup a (StringContext -> LaTeX)
parLatexMarkup ppId isTyCon = Markup {
  markupParagraph     = \p v -> p v <> text "\\par" $$ text "",
  markupEmpty         = \_ -> empty,
  markupString        = \s v -> text (fixString v s),
  markupAppend        = \l r v -> l v <> r v,
  markupIdentifier    = markupId,
  markupModule        = \m v -> let (mdl,_ref) = break (=='#') m in tt (text mdl),
  markupEmphasis      = \p v -> emph (p v),
  markupMonospaced    = \p v -> tt (p Mono),
  markupUnorderedList = \p v -> itemizedList (map ($v) p) $$ text "",
  markupPic           = \path v -> parens (text "image: " <> text path),
  markupOrderedList   = \p v -> enumeratedList (map ($v) p) $$ text "",
  markupDefList       = \l v -> descriptionList (map (\(a,b) -> (a v, b v)) l),
  markupCodeBlock     = \p _ -> quote (verb (p Verb)) $$ text "",
  markupURL           = \u _ -> text "\\url" <> braces (text u),
  markupAName         = \_ _ -> empty,
  markupExample       = \e _ -> quote $ verb $ text $ unlines $ map exampleToString e
  }
  where
    fixString Plain s = latexFilter s
    fixString Verb  s = s
    fixString Mono  s = latexMonoFilter s

    markupId id v =
      case v of
        Verb  -> theid
        Mono  -> theid
        Plain -> text "\\haddockid" <> braces theid
      where theid = ppId (choose id)

    -- If an id can refer to multiple things, we give precedence to type
    -- constructors.  This should ideally be done during renaming from RdrName
    -- to Name, but since we will move this process from GHC into Haddock in
    -- the future, we fix it here in the meantime.
    -- TODO: mention this rule in the documentation.
    choose [] = error "empty identifier list in HsDoc"
    choose [x] = x
    choose (x:y:_)
      | isTyCon x = x
      | otherwise = y

latexMarkup :: DocMarkup DocName (StringContext -> LaTeX)
latexMarkup = parLatexMarkup ppVerbDocName (isTyConName . getName)

rdrLatexMarkup :: DocMarkup RdrName (StringContext -> LaTeX)
rdrLatexMarkup = parLatexMarkup ppVerbRdrName isRdrTc

docToLaTeX :: Doc DocName -> LaTeX
docToLaTeX doc = markup latexMarkup doc Plain

rdrDocToLaTeX :: Doc RdrName -> LaTeX
rdrDocToLaTeX doc = markup rdrLatexMarkup doc Plain

data StringContext = Plain | Verb | Mono

latexStripTrailingWhitespace :: Doc a -> Doc a
latexStripTrailingWhitespace (DocString s)
  | null s'   = DocEmpty
  | otherwise = DocString s
  where s' = reverse (dropWhile isSpace (reverse s))
latexStripTrailingWhitespace (DocAppend l r)
  | DocEmpty <- r' = latexStripTrailingWhitespace l
  | otherwise      = DocAppend l r'
  where
    r' = latexStripTrailingWhitespace r
latexStripTrailingWhitespace (DocParagraph p) =
  latexStripTrailingWhitespace p
latexStripTrailingWhitespace other = other

latexStripLeadingPara :: Doc a -> Doc a
latexStripLeadingPara (DocParagraph p) = p
latexStripLeadingPara (DocAppend l r) = DocAppend (latexStripLeadingPara l) r
latexStripLeadingPara d = d

-- -----------------------------------------------------------------------------
-- LaTeX utils

itemizedList :: [LaTeX] -> LaTeX
itemizedList items =
  text "\\begin{itemize}" $$
  vcat (map (text "\\item" $$) items) $$
  text "\\end{itemize}"

enumeratedList :: [LaTeX] -> LaTeX
enumeratedList items =
  text "\\begin{enumerate}" $$
  vcat (map (text "\\item " $$) items) $$
  text "\\end{enumerate}"

descriptionList :: [(LaTeX,LaTeX)] -> LaTeX
descriptionList items =
  text "\\begin{description}" $$
  vcat (map (\(a,b) -> text "\\item" <> brackets a <+> b) items) $$
  text "\\end{description}"

tt :: LaTeX -> LaTeX
tt ltx = text "\\haddocktt" <> braces ltx

decltt :: LaTeX -> LaTeX
decltt ltx = text "\\haddockdecltt" <> braces ltx

emph :: LaTeX -> LaTeX
emph ltx = text "\\emph" <> braces ltx

verb :: LaTeX -> LaTeX
verb doc = text "{\\haddockverb\\begin{verbatim}" $$ doc <> text "\\end{verbatim}}"
   -- NB. swallow a trailing \n in the verbatim text by appending the
   -- \end{verbatim} directly, otherwise we get spurious blank lines at the
   -- end of code blocks.

quote :: LaTeX -> LaTeX
quote doc = text "\\begin{quote}" $$ doc $$ text "\\end{quote}"

dcolon, arrow, darrow, forallSymbol :: Bool -> LaTeX
dcolon unicode = text (if unicode then "∷" else "::")
arrow  unicode = text (if unicode then "→" else "->")
darrow unicode = text (if unicode then "⇒" else "=>")
forallSymbol unicode = text (if unicode then "∀" else "forall")

dot :: LaTeX
dot = char '.'

parenList :: [LaTeX] -> LaTeX
parenList = parens . hsep . punctuate comma

ubxParenList :: [LaTeX] -> LaTeX
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens :: LaTeX -> LaTeX
ubxparens h = text "(#" <> h <> text "#)"

pabrackets :: LaTeX -> LaTeX
pabrackets h = text "[:" <> h <> text ":]"

nl :: LaTeX
nl = text "\\\\"

keyword :: String -> LaTeX
keyword = text

infixr 4 <->  -- combining table cells
(<->) :: LaTeX -> LaTeX -> LaTeX
a <-> b = a <+> char '&' <+> b
