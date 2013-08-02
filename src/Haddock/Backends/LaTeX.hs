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
import Pretty hiding (Doc, quote)
import qualified Pretty

import GHC
import OccName
import Name                 ( nameOccName )
import RdrName              ( rdrNameOcc )
import FastString           ( unpackFS, unpackLitString, zString )

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


haddockSty :: FilePath
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
          = (fromMaybe empty . documentationToLaTeX . ifaceRnDoc) iface

      body = processExports exports
  --
  writeFile (odir </> moduleLaTeXFile mdl) (fullRender PageMode 80 1 string_txt "" tex)


string_txt :: TextDetails -> String -> String
string_txt (Chr c)   s  = c:s
string_txt (Str s1)  s2 = s1 ++ s2
string_txt (PStr s1) s2 = unpackFS s1 ++ s2
string_txt (ZStr s1) s2 = zString s1 ++ s2
string_txt (LStr s1 _) s2 = unpackLitString s1 ++ s2


exportListItem :: ExportItem DocName -> LaTeX
exportListItem (ExportDecl decl _doc subdocs _insts)
  = sep (punctuate comma . map ppDocBinder $ declNames decl) <>
     case subdocs of
       [] -> empty
       _  -> parens (sep (punctuate comma (map (ppDocBinder . fst) subdocs)))
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
  = multiDecl [ ppTypeSig (map getName names) typ False
              | (names,typ) <- sig:sigs ] $$
    processExports es'
  where (sigs, es') = spanWith isSimpleSig es
processExports (ExportModule mdl : es)
  = declWithDoc (vcat [ text "module" <+> text (moduleString m) | m <- mdl:mdls ]) Nothing $$
    processExports es'
  where (mdls, es') = spanWith isExportModule es
processExports (e : es) =
  processExport e $$ processExports es


isSimpleSig :: ExportItem DocName -> Maybe ([DocName], HsType DocName)
isSimpleSig (ExportDecl (L _ (SigD (TypeSig lnames (L _ t))))
                        (Documentation Nothing Nothing, argDocs) _ _)
  | Map.null argDocs = Just (map unLoc lnames, t)
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


declNames :: LHsDecl DocName -> [DocName]
declNames (L _ decl) = case decl of
  TyClD d  -> [tcdName d]
  SigD (TypeSig lnames _) -> map unLoc lnames
  _ -> error "declaration not supported by declNames"


forSummary :: (ExportItem DocName) -> Bool
forSummary (ExportGroup _ _ _) = False
forSummary (ExportDoc _)       = False
forSummary _                    = True


moduleLaTeXFile :: Module -> FilePath
moduleLaTeXFile mdl = moduleBasename mdl ++ ".tex"


moduleBasename :: Module -> FilePath
moduleBasename mdl = map (\c -> if c == '.' then '-' else c)
                         (moduleNameString (moduleName mdl))


-------------------------------------------------------------------------------
-- * Decls
-------------------------------------------------------------------------------


ppDecl :: LHsDecl DocName
       -> DocForDecl DocName
       -> [DocInstance DocName]
       -> [(DocName, DocForDecl DocName)]
       -> LaTeX

ppDecl (L loc decl) (doc, fnArgsDoc) instances subdocs = case decl of
  TyClD d@(FamDecl {})          -> ppTyFam False loc doc d unicode
  TyClD d@(DataDecl {})   
                                -> ppDataDecl instances subdocs loc doc d unicode
  TyClD d@(SynDecl {})          -> ppTySyn loc (doc, fnArgsDoc) d unicode
-- Family instances happen via FamInst now
--  TyClD d@(TySynonym {})         
--    | Just _  <- tcdTyPats d    -> ppTyInst False loc doc d unicode
-- Family instances happen via FamInst now
  TyClD d@(ClassDecl {})         -> ppClassDecl instances loc doc subdocs d unicode
  SigD (TypeSig lnames (L _ t))  -> ppFunSig loc (doc, fnArgsDoc) (map unLoc lnames) t unicode
  ForD d                         -> ppFor loc (doc, fnArgsDoc) d unicode
  InstD _                        -> empty
  _                              -> error "declaration not supported by ppDecl"
  where
    unicode = False


ppTyFam :: Bool -> SrcSpan -> Documentation DocName ->
              TyClDecl DocName -> Bool -> LaTeX
ppTyFam _ _ _ _ _ =
  error "type family declarations are currently not supported by --latex"


ppFor :: SrcSpan -> DocForDecl DocName -> ForeignDecl DocName -> Bool -> LaTeX
ppFor _ _ _ _ =
  error "foreign declarations are currently not supported by --latex"


-------------------------------------------------------------------------------
-- * Type Synonyms
-------------------------------------------------------------------------------


-- we skip type patterns for now
ppTySyn :: SrcSpan -> DocForDecl DocName -> TyClDecl DocName -> Bool -> LaTeX

ppTySyn loc doc (SynDecl { tcdLName = L _ name, tcdTyVars = ltyvars
                         , tcdRhs = ltype }) unicode
  = ppTypeOrFunSig loc [name] (unLoc ltype) doc (full, hdr, char '=') unicode
  where
    hdr  = hsep (keyword "type" : ppDocBinder name : ppTyVars ltyvars)
    full = hdr <+> char '=' <+> ppLType unicode ltype

ppTySyn _ _ _ _ = error "declaration not supported by ppTySyn"


-------------------------------------------------------------------------------
-- * Function signatures
-------------------------------------------------------------------------------


ppFunSig :: SrcSpan -> DocForDecl DocName -> [DocName] -> HsType DocName
         -> Bool -> LaTeX
ppFunSig loc doc docnames typ unicode =
  ppTypeOrFunSig loc docnames typ doc
    ( ppTypeSig names typ False
    , hsep . punctuate comma $ map ppSymName names
    , dcolon unicode)
    unicode
 where
   names = map getName docnames


ppTypeOrFunSig :: SrcSpan -> [DocName] -> HsType DocName
               -> DocForDecl DocName -> (LaTeX, LaTeX, LaTeX)
               -> Bool -> LaTeX
ppTypeOrFunSig _ _ typ (doc, argDocs) (pref1, pref2, sep0)
               unicode
  | Map.null argDocs =
      declWithDoc pref1 (documentationToLaTeX doc)
  | otherwise        =
      declWithDoc pref2 $ Just $
        text "\\haddockbeginargs" $$
        do_args 0 sep0 typ $$
        text "\\end{tabulary}\\par" $$
        fromMaybe empty (documentationToLaTeX doc)
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


ppTypeSig :: [Name] -> HsType DocName  -> Bool -> LaTeX
ppTypeSig nms ty unicode =
  hsep (punctuate comma $ map ppSymName nms)
    <+> dcolon unicode
    <+> ppType unicode ty


ppTyVars :: LHsTyVarBndrs DocName -> [LaTeX]
ppTyVars tvs = map ppSymName (tyvarNames tvs)


tyvarNames :: LHsTyVarBndrs DocName -> [Name]
tyvarNames = map getName . hsLTyVarNames


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
-- * Rendering Doc
-------------------------------------------------------------------------------


maybeDoc :: Maybe (Doc DocName) -> LaTeX
maybeDoc = maybe empty docToLaTeX


-- for table cells, we strip paragraphs out to avoid extra vertical space
-- and don't add a quote environment.
rDoc  :: Maybe (Doc DocName) -> LaTeX
rDoc = maybeDoc . fmap latexStripTrailingWhitespace


-------------------------------------------------------------------------------
-- * Class declarations
-------------------------------------------------------------------------------


ppClassHdr :: Bool -> Located [LHsType DocName] -> DocName
           -> LHsTyVarBndrs DocName -> [Located ([DocName], [DocName])]
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
            -> Documentation DocName -> [(DocName, DocForDecl DocName)]
            -> TyClDecl DocName -> Bool -> LaTeX
ppClassDecl instances loc doc subdocs
  (ClassDecl { tcdCtxt = lctxt, tcdLName = lname, tcdTyVars = ltyvars, tcdFDs = lfds 
             , tcdSigs = lsigs, tcdATs = ats, tcdATDefs = at_defs }) unicode
  = declWithDoc classheader (if null body then Nothing else Just (vcat body)) $$
    instancesBit
  where
    classheader
      | null lsigs = hdr unicode
      | otherwise  = hdr unicode <+> keyword "where"

    hdr = ppClassHdr False lctxt (unLoc lname) ltyvars lfds

    body = catMaybes [documentationToLaTeX doc, body_]

    body_
      | null lsigs, null ats, null at_defs = Nothing
      | null ats, null at_defs = Just methodTable
---     | otherwise = atTable $$ methodTable
      | otherwise = error "LaTeX.ppClassDecl"

    methodTable =
      text "\\haddockpremethods{}\\textbf{Methods}" $$
      vcat  [ ppFunSig loc doc names typ unicode
            | L _ (TypeSig lnames (L _ typ)) <- lsigs
            , let doc = lookupAnySubdoc (head names) subdocs
                  names = map unLoc lnames ]
              -- FIXME: is taking just the first name ok? Is it possible that
              -- there are different subdocs for different names in a single
              -- type signature?

    instancesBit = ppDocInstances unicode instances

ppClassDecl _ _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"

ppDocInstances :: Bool -> [DocInstance DocName] -> LaTeX
ppDocInstances _unicode [] = empty
ppDocInstances unicode (i : rest)
  | Just ihead <- isUndocdInstance i
  = declWithDoc (vcat (map (ppInstDecl unicode) (ihead:is))) Nothing $$
    ppDocInstances unicode rest'
  | otherwise 
  = ppDocInstance unicode i $$ ppDocInstances unicode rest
  where
    (is, rest') = spanWith isUndocdInstance rest

isUndocdInstance :: DocInstance a -> Maybe (InstHead a)
isUndocdInstance (i,Nothing) = Just i
isUndocdInstance _ = Nothing

-- | Print a possibly commented instance. The instance header is printed inside
-- an 'argBox'. The comment is printed to the right of the box in normal comment
-- style.
ppDocInstance :: Bool -> DocInstance DocName -> LaTeX
ppDocInstance unicode (instHead, doc) =
  declWithDoc (ppInstDecl unicode instHead) (fmap docToLaTeX doc)


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


-------------------------------------------------------------------------------
-- * Data & newtype declarations
-------------------------------------------------------------------------------


ppDataDecl :: [DocInstance DocName] ->
              [(DocName, DocForDecl DocName)] ->
              SrcSpan -> Documentation DocName -> TyClDecl DocName -> Bool ->
              LaTeX
ppDataDecl instances subdocs _loc doc dataDecl unicode

   =  declWithDoc (ppDataHeader dataDecl unicode <+> whereBit)
                  (if null body then Nothing else Just (vcat body))
   $$ instancesBit

  where
    cons      = dd_cons (tcdDataDefn dataDecl)
    resTy     = (con_res . unLoc . head) cons

    body = catMaybes [constrBit, documentationToLaTeX doc]

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

    instancesBit = ppDocInstances unicode instances


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr :: HsExplicitFlag -> [Name] -> HsContext DocName -> Bool -> LaTeX
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
    occ     = nameOccName . getName . unLoc . con_name $ con
    ltvs    = con_qvars con
    tyVars  = tyvarNames (con_qvars con)
    context = unLoc (con_cxt con)
    forall  = con_explicit con
    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc = lookup (unLoc $ con_name con) subdocs >>= combineDocumentation . fst
    mkFunTy a b = noLoc (HsFunTy a b)


ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Bool -> ConDeclField DocName ->  LaTeX
ppSideBySideField subdocs unicode (ConDeclField (L _ name) ltype _) =
  decltt (ppBinder (nameOccName . getName $ name)
    <+> dcolon unicode <+> ppLType unicode ltype) <-> rDoc mbDoc
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    mbDoc = lookup name subdocs >>= combineDocumentation . fst

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
ppDataHeader (DataDecl { tcdLName = L _ name, tcdTyVars = tyvars
                       , tcdDataDefn = HsDataDefn { dd_ND = nd, dd_ctxt = ctxt } }) unicode
  = -- newtype or data
    (case nd of { NewType -> keyword "newtype"; DataType -> keyword "data" }) <+>
    -- context
    ppLContext ctxt unicode <+>
    -- T a b c ..., or a :+: b
    ppAppDocNameNames False name (tyvarNames tyvars)
ppDataHeader _ _ = error "ppDataHeader: illegal argument"

--------------------------------------------------------------------------------
-- * Type applications
--------------------------------------------------------------------------------


-- | Print an application of a DocName and a list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocName] -> Bool -> LaTeX
ppAppNameTypes n ts unicode = ppTypeApp n ts ppDocName (ppParendType unicode)


-- | Print an application of a DocName and a list of Names 
ppAppDocNameNames :: Bool -> DocName -> [Name] -> LaTeX
ppAppDocNameNames _summ n ns =
  ppTypeApp n ns (ppBinder . nameOccName . getName) ppSymName


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
-- * Contexts
-------------------------------------------------------------------------------


ppLContext, ppLContextNoArrow :: Located (HsContext DocName) -> Bool -> LaTeX
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc


ppContextNoArrow :: HsContext DocName -> Bool -> LaTeX
ppContextNoArrow []  _ = empty
ppContextNoArrow cxt unicode = pp_hs_context (map unLoc cxt) unicode


ppContextNoLocs :: [HsType DocName] -> Bool -> LaTeX
ppContextNoLocs []  _ = empty
ppContextNoLocs cxt unicode = pp_hs_context cxt unicode <+> darrow unicode


ppContext :: HsContext DocName -> Bool -> LaTeX
ppContext cxt unicode = ppContextNoLocs (map unLoc cxt) unicode


pp_hs_context :: [HsType DocName] -> Bool -> LaTeX
pp_hs_context []  _       = empty
pp_hs_context [p] unicode = ppType unicode p
pp_hs_context cxt unicode = parenList (map (ppType unicode) cxt)


-------------------------------------------------------------------------------
-- * Types and contexts
-------------------------------------------------------------------------------


ppBang :: HsBang -> LaTeX
ppBang HsNoBang = empty
ppBang _        = char '!' -- Unpacked args is an implementation detail,


tupleParens :: HsTupleSort -> [LaTeX] -> LaTeX
tupleParens HsUnboxedTuple = ubxParenList
tupleParens _              = parenList


-------------------------------------------------------------------------------
-- * Rendering of HsType
--
-- Stolen from Html and tweaked for LaTeX generation
-------------------------------------------------------------------------------


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

ppLKind :: Bool -> LHsKind DocName -> LaTeX
ppLKind unicode y = ppKind unicode (unLoc y)

ppKind :: Bool -> HsKind DocName -> LaTeX
ppKind unicode ki = ppr_mono_ty pREC_TOP ki unicode


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppForAll :: HsExplicitFlag -> LHsTyVarBndrs DocName
         -> Located (HsContext DocName) -> Bool -> LaTeX
ppForAll expl tvs cxt unicode
  | show_forall = forall_part <+> ppLContext cxt unicode
  | otherwise   = ppLContext cxt unicode
  where
    show_forall = not (null (hsQTvBndrs tvs)) && is_explicit
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
ppr_mono_ty _         (HsKindSig ty kind) u = parens (ppr_mono_lty pREC_TOP ty u <+> dcolon u <+> ppLKind u kind)
ppr_mono_ty _         (HsRoleAnnot {})    _ = error "ppr_mono_ty HsRoleAnnot"
ppr_mono_ty _         (HsListTy ty)       u = brackets (ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsPArrTy ty)       u = pabrackets (ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsIParamTy n ty)   u = brackets (ppIPName n <+> dcolon u <+> ppr_mono_lty pREC_TOP ty u)
ppr_mono_ty _         (HsSpliceTy {})     _ = error "ppr_mono_ty HsSpliceTy"
ppr_mono_ty _         (HsQuasiQuoteTy {}) _ = error "ppr_mono_ty HsQuasiQuoteTy"
ppr_mono_ty _         (HsRecTy {})        _ = error "ppr_mono_ty HsRecTy"
ppr_mono_ty _         (HsCoreTy {})       _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty _         (HsExplicitListTy _ tys) u = Pretty.quote $ brackets $ hsep $ punctuate comma $ map (ppLType u) tys
ppr_mono_ty _         (HsExplicitTupleTy _ tys) u = Pretty.quote $ parenList $ map (ppLType u) tys
ppr_mono_ty _         (HsWrapTy {})       _ = error "ppr_mono_ty HsWrapTy"

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2) unicode
  = maybeParen ctxt_prec pREC_OP $
    ppr_mono_lty pREC_OP ty1 unicode <+> char '~' <+> ppr_mono_lty pREC_OP ty2 unicode

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty) unicode
  = maybeParen ctxt_prec pREC_CON $
    hsep [ppr_mono_lty pREC_FUN fun_ty unicode, ppr_mono_lty pREC_CON arg_ty unicode]

ppr_mono_ty ctxt_prec (HsOpTy ty1 (_, op) ty2) unicode
  = maybeParen ctxt_prec pREC_FUN $
    ppr_mono_lty pREC_OP ty1 unicode <+> ppr_op <+> ppr_mono_lty pREC_OP ty2 unicode
  where
    ppr_op = if not (isSymOcc occName) then char '`' <> ppLDocName op <> char '`' else ppLDocName op
    occName = nameOccName . getName . unLoc $ op

ppr_mono_ty ctxt_prec (HsParTy ty) unicode
--  = parens (ppr_mono_lty pREC_TOP ty)
  = ppr_mono_lty ctxt_prec ty unicode

ppr_mono_ty ctxt_prec (HsDocTy ty _) unicode
  = ppr_mono_lty ctxt_prec ty unicode

ppr_mono_ty _ (HsTyLit t) u = ppr_tylit t u


ppr_tylit :: HsTyLit -> Bool -> LaTeX
ppr_tylit (HsNumTy n) _ = integer n
ppr_tylit (HsStrTy s) _ = text (show s)
  -- XXX: Ok in verbatim, but not otherwise
  -- XXX: Do something with Unicode parameter?


ppr_fun_ty :: Int -> LHsType DocName -> LHsType DocName -> Bool -> LaTeX
ppr_fun_ty ctxt_prec ty1 ty2 unicode
  = let p1 = ppr_mono_lty pREC_FUN ty1 unicode
        p2 = ppr_mono_lty pREC_TOP ty2 unicode
    in
    maybeParen ctxt_prec pREC_FUN $
    sep [p1, arrow unicode <+> p2]


-------------------------------------------------------------------------------
-- * Names
-------------------------------------------------------------------------------


ppBinder :: OccName -> LaTeX
ppBinder n
  | isVarSym n = parens $ ppOccName n
  | otherwise  = ppOccName n


ppSymName :: Name -> LaTeX
ppSymName name
  | isNameSym name = parens $ ppName name
  | otherwise = ppName name


ppVerbOccName :: OccName -> LaTeX
ppVerbOccName = text . latexFilter . occNameString

ppIPName :: HsIPName -> LaTeX
ppIPName ip = text $ unpackFS $ hsIPNameFS ip

ppOccName :: OccName -> LaTeX
ppOccName = text . occNameString


ppVerbDocName :: DocName -> LaTeX
ppVerbDocName = ppVerbOccName . nameOccName . getName


ppVerbRdrName :: RdrName -> LaTeX
ppVerbRdrName = ppVerbOccName . rdrNameOcc


ppDocName :: DocName -> LaTeX
ppDocName = ppOccName . nameOccName . getName


ppLDocName :: Located DocName -> LaTeX
ppLDocName (L _ d) = ppDocName d


ppDocBinder :: DocName -> LaTeX
ppDocBinder = ppBinder . nameOccName . getName


ppName :: Name -> LaTeX
ppName = ppOccName . nameOccName


latexFilter :: String -> String
latexFilter = foldr latexMunge ""


latexMonoFilter :: String -> String
latexMonoFilter = foldr latexMonoMunge ""


latexMunge :: Char -> String -> String
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


latexMonoMunge :: Char -> String -> String
latexMonoMunge ' ' s = '\\' : ' ' : s
latexMonoMunge '\n' s = '\\' : '\\' : s
latexMonoMunge c   s = latexMunge c s


-------------------------------------------------------------------------------
-- * Doc Markup
-------------------------------------------------------------------------------


parLatexMarkup :: (a -> LaTeX) -> DocMarkup a (StringContext -> LaTeX)
parLatexMarkup ppId = Markup {
  markupParagraph            = \p v -> p v <> text "\\par" $$ text "",
  markupEmpty                = \_ -> empty,
  markupString               = \s v -> text (fixString v s),
  markupAppend               = \l r v -> l v <> r v,
  markupIdentifier           = markupId ppId,
  markupIdentifierUnchecked  = markupId (ppVerbOccName . snd),
  markupModule               = \m _ -> let (mdl,_ref) = break (=='#') m in tt (text mdl),
  markupWarning              = \p v -> emph (p v),
  markupEmphasis             = \p v -> emph (p v),
  markupMonospaced           = \p _ -> tt (p Mono),
  markupUnorderedList        = \p v -> itemizedList (map ($v) p) $$ text "",
  markupPic                  = \path _ -> parens (text "image: " <> text path),
  markupOrderedList          = \p v -> enumeratedList (map ($v) p) $$ text "",
  markupDefList              = \l v -> descriptionList (map (\(a,b) -> (a v, b v)) l),
  markupCodeBlock            = \p _ -> quote (verb (p Verb)) $$ text "",
  markupHyperlink            = \l _ -> markupLink l,
  markupAName                = \_ _ -> empty,
  markupProperty             = \p _ -> quote $ verb $ text p,
  markupExample              = \e _ -> quote $ verb $ text $ unlines $ map exampleToString e
  }
  where
    fixString Plain s = latexFilter s
    fixString Verb  s = s
    fixString Mono  s = latexMonoFilter s

    markupLink (Hyperlink url mLabel) = case mLabel of
      Just label -> text "\\href" <> braces (text url) <> braces (text label)
      Nothing    -> text "\\url"  <> braces (text url)

    markupId ppId_ id v =
      case v of
        Verb  -> theid
        Mono  -> theid
        Plain -> text "\\haddockid" <> braces theid
      where theid = ppId_ id


latexMarkup :: DocMarkup DocName (StringContext -> LaTeX)
latexMarkup = parLatexMarkup ppVerbDocName


rdrLatexMarkup :: DocMarkup RdrName (StringContext -> LaTeX)
rdrLatexMarkup = parLatexMarkup ppVerbRdrName


docToLaTeX :: Doc DocName -> LaTeX
docToLaTeX doc = markup latexMarkup doc Plain


documentationToLaTeX :: Documentation DocName -> Maybe LaTeX
documentationToLaTeX = fmap docToLaTeX . combineDocumentation


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


-------------------------------------------------------------------------------
-- * LaTeX utils
-------------------------------------------------------------------------------


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
