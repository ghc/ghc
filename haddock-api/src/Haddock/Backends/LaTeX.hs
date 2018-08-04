{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Backends.LaTeX
-- Copyright   :  (c) Simon Marlow      2010,
--                    Mateusz Kowalczyk 2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Backends.LaTeX (
  ppLaTeX,
) where

import Documentation.Haddock.Markup
import Haddock.Types
import Haddock.Utils
import Haddock.GhcUtils
import Pretty hiding (Doc, quote)
import qualified Pretty

import GHC
import OccName
import Name                 ( nameOccName )
import RdrName              ( rdrNameOcc )
import FastString           ( unpackFS )
import Outputable           ( panic)

import qualified Data.Map as Map
import System.Directory
import System.FilePath
import Data.Char
import Control.Monad
import Data.Maybe
import Data.List
import Prelude hiding ((<>))

import Haddock.Doc (combineDocumentation)

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

  writeFile filename (show tex)


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
  writeFile (odir </> moduleLaTeXFile mdl) (fullRender PageMode 80 1 txtPrinter "" tex)

-- | Prints out an entry in a module export list.
exportListItem :: ExportItem DocNameI -> LaTeX
exportListItem ExportDecl { expItemDecl = decl, expItemSubDocs = subdocs }
  = let (leader, names) = declNames decl
    in sep (punctuate comma [ leader <+> ppDocBinder name | name <- names ]) <>
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
processExports :: [ExportItem DocNameI] -> LaTeX
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


isSimpleSig :: ExportItem DocNameI -> Maybe ([DocName], HsType DocNameI)
isSimpleSig ExportDecl { expItemDecl = L _ (SigD _ (TypeSig _ lnames t))
                       , expItemMbDoc = (Documentation Nothing Nothing, argDocs) }
  | Map.null argDocs = Just (map unLoc lnames, unLoc (hsSigWcType t))
isSimpleSig _ = Nothing


isExportModule :: ExportItem DocNameI -> Maybe Module
isExportModule (ExportModule m) = Just m
isExportModule _ = Nothing


processExport :: ExportItem DocNameI -> LaTeX
processExport (ExportGroup lev _id0 doc)
  = ppDocGroup lev (docToLaTeX doc)
processExport (ExportDecl decl pats doc subdocs insts fixities _splice)
  = ppDecl decl pats doc insts subdocs fixities
processExport (ExportNoDecl y [])
  = ppDocName y
processExport (ExportNoDecl y subs)
  = ppDocName y <> parens (sep (punctuate comma (map ppDocName subs)))
processExport (ExportModule mdl)
  = declWithDoc (text "module" <+> text (moduleString mdl)) Nothing
processExport (ExportDoc doc)
  = docToLaTeX $ _doc doc


ppDocGroup :: Int -> LaTeX -> LaTeX
ppDocGroup lev doc = sec lev <> braces doc
  where sec 1 = text "\\section"
        sec 2 = text "\\subsection"
        sec 3 = text "\\subsubsection"
        sec _ = text "\\paragraph"


-- | Given a declaration, extract out the names being declared
declNames :: LHsDecl DocNameI
          -> ( LaTeX           -- ^ to print before each name in an export list
             , [DocName]       -- ^ names being declared
             )
declNames (L _ decl) = case decl of
  TyClD _ d  -> (empty, [tcdName d])
  SigD _ (TypeSig _ lnames _ ) -> (empty, map unLoc lnames)
  SigD _ (PatSynSig _ lnames _) -> (text "pattern", map unLoc lnames)
  ForD _ (ForeignImport _ (L _ n) _ _) -> (empty, [n])
  ForD _ (ForeignExport _ (L _ n) _ _) -> (empty, [n])
  _ -> error "declaration not supported by declNames"


forSummary :: (ExportItem DocNameI) -> Bool
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

-- | Pretty print a declaration
ppDecl :: LHsDecl DocNameI                         -- ^ decl to print
       -> [(HsDecl DocNameI, DocForDecl DocName)]  -- ^ all pattern decls
       -> DocForDecl DocName                       -- ^ documentation for decl
       -> [DocInstance DocNameI]                   -- ^ all instances
       -> [(DocName, DocForDecl DocName)]          -- ^ all subdocs
       -> [(DocName, Fixity)]                      -- ^ all fixities
       -> LaTeX

ppDecl decl pats (doc, fnArgsDoc) instances subdocs _fxts = case unLoc decl of
  TyClD _ d@FamDecl {}         -> ppFamDecl doc instances d unicode
  TyClD _ d@DataDecl {}        -> ppDataDecl pats instances subdocs (Just doc) d unicode
  TyClD _ d@SynDecl {}         -> ppTySyn (doc, fnArgsDoc) d unicode
-- Family instances happen via FamInst now
--  TyClD _ d@TySynonym{}
--    | Just _  <- tcdTyPats d    -> ppTyInst False loc doc d unicode
-- Family instances happen via FamInst now
  TyClD _ d@ClassDecl{}          -> ppClassDecl instances doc subdocs d unicode
  SigD _ (TypeSig _ lnames ty)   -> ppFunSig (doc, fnArgsDoc) (map unLoc lnames) (hsSigWcType ty) unicode
  SigD _ (PatSynSig _ lnames ty) -> ppLPatSig (doc, fnArgsDoc) (map unLoc lnames) ty unicode
  ForD _ d                       -> ppFor (doc, fnArgsDoc) d unicode
  InstD _ _                      -> empty
  DerivD _ _                     -> empty
  _                              -> error "declaration not supported by ppDecl"
  where
    unicode = False


ppFor :: DocForDecl DocName -> ForeignDecl DocNameI -> Bool -> LaTeX
ppFor doc (ForeignImport _ (L _ name) typ _) unicode =
  ppFunSig doc [name] (hsSigType typ) unicode
ppFor _ _ _ = error "ppFor error in Haddock.Backends.LaTeX"
--  error "foreign declarations are currently not supported by --latex"


-------------------------------------------------------------------------------
-- * Type families
-------------------------------------------------------------------------------

-- | Pretty-print a data\/type family declaration
ppFamDecl :: Documentation DocName    -- ^ this decl's docs
          -> [DocInstance DocNameI]   -- ^ relevant instances
          -> TyClDecl DocNameI        -- ^ family to print
          -> Bool                     -- ^ unicode
          -> LaTeX
ppFamDecl doc instances decl unicode =
  declWithDoc (ppFamHeader (tcdFam decl) unicode <+> whereBit)
              (if null body then Nothing else Just (vcat body))
  $$ instancesBit
  where
    body = catMaybes [familyEqns, documentationToLaTeX doc]

    whereBit = case fdInfo (tcdFam decl) of
      ClosedTypeFamily _ -> keyword "where"
      _                  -> empty

    familyEqns
      | FamilyDecl { fdInfo = ClosedTypeFamily (Just eqns) } <- tcdFam decl
      = Just (text "\\haddockbeginargs" $$
              vcat [ decltt (ppFamDeclEqn eqn) <+> nl | L _ eqn <- eqns ] $$
              text "\\end{tabulary}\\par")
      | otherwise = Nothing

    -- Individual equations of a closed type family
    ppFamDeclEqn :: TyFamInstEqn DocNameI -> LaTeX
    ppFamDeclEqn (HsIB { hsib_body = FamEqn { feqn_tycon = L _ n
                                            , feqn_rhs = rhs
                                            , feqn_pats = ts } })
      = hsep [ ppAppNameTypes n (map unLoc ts) unicode
             , equals
             , ppType unicode (unLoc rhs)
             ]
    ppFamDeclEqn (XHsImplicitBndrs _) = panic "haddock:ppFamDecl"
    ppFamDeclEqn (HsIB { hsib_body = XFamEqn _}) = panic "haddock:ppFamDecl"

    instancesBit = ppDocInstances unicode instances

-- | Print the LHS of a type\/data family declaration.
ppFamHeader :: FamilyDecl DocNameI  -- ^ family header to print
              -> Bool                 -- ^ unicode
              -> LaTeX
ppFamHeader (XFamilyDecl _) _ = panic "haddock;ppFamHeader"
ppFamHeader (FamilyDecl { fdLName = L _ name
                        , fdTyVars = tvs
                        , fdInfo = info
                        , fdResultSig = L _ result
                        , fdInjectivityAnn = injectivity })
              unicode =
  leader <+> keyword "family" <+> famName <+> famSig <+> injAnn
  where
    leader = case info of
      OpenTypeFamily     -> keyword "type"
      ClosedTypeFamily _ -> keyword "type"
      DataFamily         -> keyword "data"

    famName = ppAppDocNameTyVarBndrs unicode name (hsq_explicit tvs)

    famSig = case result of
      NoSig _               -> empty
      KindSig _ kind        -> dcolon unicode <+> ppLKind unicode kind
      TyVarSig _ (L _ bndr) -> equals <+> ppHsTyVarBndr unicode bndr
      XFamilyResultSig _    -> panic "haddock:ppFamHeader"

    injAnn = case injectivity of
      Nothing -> empty
      Just (L _ (InjectivityAnn lhs rhs)) -> hsep ( decltt (text "|")
                                                  : ppLDocName lhs
                                                  : arrow unicode
                                                  : map ppLDocName rhs)



-------------------------------------------------------------------------------
-- * Type Synonyms
-------------------------------------------------------------------------------


-- we skip type patterns for now
ppTySyn :: DocForDecl DocName -> TyClDecl DocNameI -> Bool -> LaTeX

ppTySyn doc (SynDecl { tcdLName = L _ name, tcdTyVars = ltyvars
                         , tcdRhs = ltype }) unicode
  = ppTypeOrFunSig (unLoc ltype) doc (full, hdr, char '=') unicode
  where
    hdr  = hsep (keyword "type"
                 : ppDocBinder name
                 : map ppSymName (tyvarNames ltyvars))
    full = hdr <+> char '=' <+> ppLType unicode ltype

ppTySyn _ _ _ = error "declaration not supported by ppTySyn"


-------------------------------------------------------------------------------
-- * Function signatures
-------------------------------------------------------------------------------


ppFunSig :: DocForDecl DocName -> [DocName] -> LHsType DocNameI
         -> Bool -> LaTeX
ppFunSig doc docnames (L _ typ) unicode =
  ppTypeOrFunSig typ doc
    ( ppTypeSig names typ False
    , hsep . punctuate comma $ map ppSymName names
    , dcolon unicode
    )
    unicode
 where
   names = map getName docnames

-- | Pretty-print a pattern synonym
ppLPatSig :: DocForDecl DocName  -- ^ documentation
          -> [DocName]           -- ^ pattern names in the pattern signature
          -> LHsSigType DocNameI -- ^ type of the pattern synonym
          -> Bool                -- ^ unicode
          -> LaTeX
ppLPatSig doc docnames ty unicode
  = ppTypeOrFunSig typ doc
      ( keyword "pattern" <+> ppTypeSig names typ False
      , keyword "pattern" <+> (hsep . punctuate comma $ map ppSymName names)
      , dcolon unicode
      )
      unicode
  where
    typ = unLoc (hsSigType ty)
    names = map getName docnames

-- | Pretty-print a type, adding documentation to the whole type and its
-- arguments as needed.
ppTypeOrFunSig :: HsType DocNameI
               -> DocForDecl DocName  -- ^ documentation
               -> ( LaTeX             -- ^ first-line (no-argument docs only)
                  , LaTeX             -- ^ first-line (argument docs only)
                  , LaTeX             -- ^ type prefix (argument docs only)
                  )
               -> Bool                -- ^ unicode
               -> LaTeX
ppTypeOrFunSig typ (doc, argDocs) (pref1, pref2, sep0) unicode
  | Map.null argDocs = declWithDoc pref1 (documentationToLaTeX doc)
  | otherwise        = declWithDoc pref2 $ Just $
        text "\\haddockbeginargs" $$
        vcat (map (uncurry (<->)) (ppSubSigLike unicode typ argDocs [] sep0)) $$
        text "\\end{tabulary}\\par" $$
        fromMaybe empty (documentationToLaTeX doc)

-- This splits up a type signature along `->` and adds docs (when they exist)
-- to the arguments. The output is a list of (leader/seperator, argument and
-- its doc)
ppSubSigLike :: Bool                  -- ^ unicode
             -> HsType DocNameI       -- ^ type signature
             -> FnArgsDoc DocName     -- ^ docs to add
             -> [(DocName, DocForDecl DocName)] -- ^ all subdocs (useful when we have `HsRecTy`)
             -> LaTeX                 -- ^ seperator (beginning of first line)
             -> [(LaTeX, LaTeX)]      -- ^ arguments (leader/sep, type)
ppSubSigLike unicode typ argDocs subdocs leader = do_args 0 leader typ
  where
    do_largs n leader (L _ t) = do_args n leader t

    arg_doc n = rDoc . fmap _doc $ Map.lookup n argDocs

    do_args :: Int -> LaTeX -> HsType DocNameI -> [(LaTeX, LaTeX)]
    do_args _n leader (HsForAllTy _ tvs ltype)
      = [ ( decltt leader
          , decltt (hsep (forallSymbol unicode : ppTyVars tvs ++ [dot]))
              <+> ppLType unicode ltype
          ) ]
    do_args n leader (HsQualTy _ lctxt ltype)
      = (decltt leader, ppLContextNoArrow lctxt unicode <+> nl)
        : do_largs n (darrow unicode) ltype

    do_args n leader (HsFunTy _ (L _ (HsRecTy _ fields)) r)
      = [ (decltt ldr, latex <+> nl)
        | (L _ field, ldr) <- zip fields (leader <+> gadtOpen : repeat gadtComma)
        , let latex = ppSideBySideField subdocs unicode field
        ]
        ++ do_largs (n+1) (gadtEnd <+> arrow unicode) r
    do_args n leader (HsFunTy _ lt r)
      = (decltt leader, decltt (ppLFunLhType unicode lt) <-> arg_doc n <+> nl)
        : do_largs (n+1) (arrow unicode) r
    do_args n leader t
      = [ (decltt leader, decltt (ppType unicode t) <-> arg_doc n <+> nl) ]

    -- FIXME: this should be done more elegantly
    --
    -- We need 'gadtComma' and 'gadtEnd' to line up with the `{` from
    -- 'gadtOpen', so we add 3 spaces to cover for `-> `/`:: ` (3 in unicode
    -- mode since `->` and `::` are rendered as single characters.
    gadtComma = hcat (replicate (if unicode then 3 else 4) (text "\\ ")) <> text ","
    gadtEnd = hcat (replicate (if unicode then 3 else 4) (text "\\ ")) <> text "\\}"
    gadtOpen = text "\\{"


ppTypeSig :: [Name] -> HsType DocNameI  -> Bool -> LaTeX
ppTypeSig nms ty unicode =
  hsep (punctuate comma $ map ppSymName nms)
    <+> dcolon unicode
    <+> ppType unicode ty


ppTyVars :: [LHsTyVarBndr DocNameI] -> [LaTeX]
ppTyVars = map (ppSymName . getName . hsLTyVarName)


tyvarNames :: LHsQTyVars DocNameI -> [Name]
tyvarNames = map (getName . hsLTyVarName) . hsQTvExplicit


declWithDoc :: LaTeX -> Maybe LaTeX -> LaTeX
declWithDoc decl doc =
   text "\\begin{haddockdesc}" $$
   text "\\item[\\begin{tabular}{@{}l}" $$
   text (latexMonoFilter (show decl)) $$
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
      text (latexMonoFilter (show decl)) $$
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


ppClassHdr :: Bool -> Located [LHsType DocNameI] -> DocName
           -> LHsQTyVars DocNameI -> [Located ([Located DocName], [Located DocName])]
           -> Bool -> LaTeX
ppClassHdr summ lctxt n tvs fds unicode =
  keyword "class"
  <+> (if not . null . unLoc $ lctxt then ppLContext lctxt unicode else empty)
  <+> ppAppDocNameNames summ n (tyvarNames tvs)
  <+> ppFds fds unicode


ppFds :: [Located ([Located DocName], [Located DocName])] -> Bool -> LaTeX
ppFds fds unicode =
  if null fds then empty else
    char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
    fundep (vars1,vars2) = hsep (map (ppDocName . unLoc) vars1) <+> arrow unicode <+>
                           hsep (map (ppDocName . unLoc) vars2)


ppClassDecl :: [DocInstance DocNameI]
            -> Documentation DocName -> [(DocName, DocForDecl DocName)]
            -> TyClDecl DocNameI -> Bool -> LaTeX
ppClassDecl instances doc subdocs
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
      text "\\haddockpremethods{}" <> emph (text "Methods") $$
      vcat  [ ppFunSig doc names (hsSigWcType typ) unicode
            | L _ (TypeSig _ lnames typ) <- lsigs
            , let doc = lookupAnySubdoc (head names) subdocs
                  names = map unLoc lnames ]
              -- FIXME: is taking just the first name ok? Is it possible that
              -- there are different subdocs for different names in a single
              -- type signature?

    instancesBit = ppDocInstances unicode instances

ppClassDecl _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"

ppDocInstances :: Bool -> [DocInstance DocNameI] -> LaTeX
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
isUndocdInstance (i,Nothing,_,_) = Just i
isUndocdInstance _ = Nothing

-- | Print a possibly commented instance. The instance header is printed inside
-- an 'argBox'. The comment is printed to the right of the box in normal comment
-- style.
ppDocInstance :: Bool -> DocInstance DocNameI -> LaTeX
ppDocInstance unicode (instHead, doc, _, _) =
  declWithDoc (ppInstDecl unicode instHead) (fmap docToLaTeX $ fmap _doc doc)


ppInstDecl :: Bool -> InstHead DocNameI -> LaTeX
ppInstDecl unicode (InstHead {..}) = case ihdInstType of
  ClassInst ctx _ _ _ -> keyword "instance" <+> ppContextNoLocs ctx unicode <+> typ
  TypeInst rhs -> keyword "type" <+> keyword "instance" <+> typ <+> tibody rhs
  DataInst dd ->
    let nd = dd_ND (tcdDataDefn dd)
        pref = case nd of { NewType -> keyword "newtype"; DataType -> keyword "data" }
    in pref <+> keyword "instance" <+> typ
  where
    typ = ppAppNameTypes ihdClsName ihdTypes unicode
    tibody = maybe empty (\t -> equals <+> ppType unicode t)

lookupAnySubdoc :: (Eq name1) =>
                   name1 -> [(name1, DocForDecl name2)] -> DocForDecl name2
lookupAnySubdoc n subdocs = case lookup n subdocs of
  Nothing -> noDocForDecl
  Just docs -> docs


-------------------------------------------------------------------------------
-- * Data & newtype declarations
-------------------------------------------------------------------------------

-- | Pretty-print a data declaration
ppDataDecl :: [(HsDecl DocNameI, DocForDecl DocName)] -- ^ relevant patterns
           -> [DocInstance DocNameI]                  -- ^ relevant instances
           -> [(DocName, DocForDecl DocName)]         -- ^ relevant decl docs
           -> Maybe (Documentation DocName)           -- ^ this decl's docs
           -> TyClDecl DocNameI                       -- ^ data decl to print
           -> Bool                                    -- ^ unicode
           -> LaTeX
ppDataDecl pats instances subdocs doc dataDecl unicode =
   declWithDoc (ppDataHeader dataDecl unicode <+> whereBit)
               (if null body then Nothing else Just (vcat body))
   $$ instancesBit

  where
    cons      = dd_cons (tcdDataDefn dataDecl)
    resTy     = (unLoc . head) cons

    body = catMaybes [doc >>= documentationToLaTeX, constrBit,patternBit]

    (whereBit, leaders)
      | null cons
      , null pats = (empty,[])
      | null cons = (text "where", repeat empty)
      | otherwise = case resTy of
        ConDeclGADT{} -> (text "where", repeat empty)
        _             -> (empty, (decltt (text "=") : repeat (decltt (text "|"))))

    constrBit
      | null cons = Nothing
      | otherwise = Just $
          text "\\enspace" <+> emph (text "Constructors") <> text "\\par" $$
          text "\\haddockbeginconstrs" $$
          vcat (zipWith (ppSideBySideConstr subdocs unicode) leaders cons) $$
          text "\\end{tabulary}\\par"

    patternBit
      | null pats = Nothing
      | otherwise = Just $
          text "\\enspace" <+> emph (text "Bundled Patterns") <> text "\\par" $$
          text "\\haddockbeginconstrs" $$
          vcat [ empty <-> ppSideBySidePat lnames typ d unicode
               | (SigD _ (PatSynSig _ lnames typ), d) <- pats
               ] $$
          text "\\end{tabulary}\\par"

    instancesBit = ppDocInstances unicode instances


-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr :: Bool -> [Name] -> HsContext DocNameI -> Bool -> LaTeX
ppConstrHdr forall tvs ctxt unicode
 = (if null tvs then empty else ppForall)
   <+>
   (if null ctxt then empty else ppContextNoArrow ctxt unicode <+> darrow unicode <+> text " ")
  where
    ppForall = case forall of
      True  -> forallSymbol unicode <+> hsep (map ppName tvs) <+> text ". "
      False -> empty


-- | Pretty-print a constructor
ppSideBySideConstr :: [(DocName, DocForDecl DocName)]  -- ^ all decl docs
                   -> Bool                             -- ^ unicode
                   -> LaTeX                            -- ^ prefix to decl
                   -> LConDecl DocNameI                -- ^ constructor decl
                   -> LaTeX
ppSideBySideConstr subdocs unicode leader (L _ con) =
  leader <-> decltt decl <-> rDoc mbDoc <+> nl
  $$ fieldPart
  where
    -- Find the name of a constructors in the decl (`getConName` always returns
    -- a non-empty list)
    aConName = unLoc (head (getConNames con))

    occ      = map (nameOccName . getName . unLoc) $ getConNames con

    ppOcc      = cat (punctuate comma (map ppBinder occ))
    ppOccInfix = cat (punctuate comma (map ppBinderInfix occ))

    -- Extract out the map of of docs corresponding to the constructors arguments
    argDocs = maybe Map.empty snd (lookup aConName subdocs)
    hasArgDocs = not $ Map.null argDocs

    -- First line of the constructor (no doc, no fields, single-line)
    decl = case con of
      ConDeclH98{ con_args = det
                , con_ex_tvs = vars
                , con_mb_cxt = cxt
                } -> let tyVars = map (getName . hsLTyVarName) vars
                         context = unLoc (fromMaybe (noLoc []) cxt)
                         forall_ = False
                         header_ = ppConstrHdr forall_ tyVars context unicode
                     in case det of
        -- Prefix constructor, e.g. 'Just a'
        PrefixCon args
          | hasArgDocs -> header_ <+> ppOcc
          | otherwise -> hsep [ header_
                              , ppOcc
                              , hsep (map (ppLParendType unicode) args)
                              ]

        -- Record constructor, e.g. 'Identity { runIdentity :: a }'
        RecCon _ ->  header_ <+> ppOcc

        -- Infix constructor, e.g. 'a :| [a]'
        InfixCon arg1 arg2
          | hasArgDocs -> header_ <+> ppOcc
          | otherwise -> hsep [ header_
                              , ppLParendType unicode arg1
                              , ppOccInfix
                              , ppLParendType unicode arg2
                              ]

      ConDeclGADT{}
        | hasArgDocs || not (isEmpty fieldPart) -> ppOcc
        | otherwise -> hsep [ ppOcc
                            , dcolon unicode
                            -- ++AZ++ make this prepend "{..}" when it is a record style GADT
                            , ppLType unicode (getGADTConType con)
                            ]
      XConDecl{} -> panic "haddock:ppSideBySideConstr"

    fieldPart = case (con, getConArgs con) of
        -- Record style GADTs
        (ConDeclGADT{}, RecCon _)            -> doConstrArgsWithDocs []

        -- Regular record declarations
        (_, RecCon (L _ fields))             -> doRecordFields fields

        -- Any GADT or a regular H98 prefix data constructor
        (_, PrefixCon args)     | hasArgDocs -> doConstrArgsWithDocs args

        -- An infix H98 data constructor
        (_, InfixCon arg1 arg2) | hasArgDocs -> doConstrArgsWithDocs [arg1,arg2]

        _ -> empty

    doRecordFields fields =
      vcat [ empty <-> tt (text begin) <+> ppSideBySideField subdocs unicode field <+> nl
           | (begin, L _ field) <- zip ("\\qquad \\{" : repeat "\\qquad ,") fields
           ]
      $$
      empty <-> tt (text "\\qquad \\}") <+> nl

    doConstrArgsWithDocs args = vcat $ map (\l -> empty <-> text "\\qquad" <+> l) $ case con of
      ConDeclH98{} ->
        [ decltt (ppLParendType unicode arg) <-> rDoc (fmap _doc mdoc) <+> nl
        | (i, arg) <- zip [0..] args
        , let mdoc = Map.lookup i argDocs
        ]
      ConDeclGADT{} ->
        [ l <+> text "\\enspace" <+> r
        | (l,r) <- ppSubSigLike unicode (unLoc (getGADTConType con)) argDocs subdocs (dcolon unicode)
        ]
      XConDecl{} -> panic "haddock:doConstrArgsWithDocs"


    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc = case getConNames con of
              [] -> panic "empty con_names"
              (cn:_) -> lookup (unLoc cn) subdocs >>=
                        fmap _doc . combineDocumentation . fst


-- | Pretty-print a record field
ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Bool -> ConDeclField DocNameI ->  LaTeX
ppSideBySideField subdocs unicode (ConDeclField _ names ltype _) =
  decltt (cat (punctuate comma (map (ppBinder . rdrNameOcc . unLoc . rdrNameFieldOcc . unLoc) names))
    <+> dcolon unicode <+> ppLType unicode ltype) <-> rDoc mbDoc
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    -- Where there is more than one name, they all have the same documentation
    mbDoc = lookup (extFieldOcc $ unLoc $ head names) subdocs >>= fmap _doc . combineDocumentation . fst
ppSideBySideField _ _ (XConDeclField _) = panic "haddock:ppSideBySideField"


-- | Pretty-print a bundled pattern synonym
ppSideBySidePat :: [Located DocName]    -- ^ pattern name(s)
                -> LHsSigType DocNameI  -- ^ type of pattern(s)
                -> DocForDecl DocName   -- ^ doc map
                -> Bool                 -- ^ unicode
                -> LaTeX
ppSideBySidePat lnames typ (doc, argDocs) unicode =
  decltt decl <-> rDoc mDoc <+> nl
  $$ fieldPart
  where
    hasArgDocs = not $ Map.null argDocs
    ppOcc = hsep (punctuate comma (map (ppDocBinder . unLoc) lnames))

    decl | hasArgDocs = keyword "pattern" <+> ppOcc
         | otherwise = hsep [ keyword "pattern"
                            , ppOcc
                            , dcolon unicode
                            , ppLType unicode (hsSigType typ)
                            ]

    fieldPart
      | not hasArgDocs = empty
      | otherwise = vcat
          [ empty <-> text "\\qquad" <+> l <+> text "\\enspace" <+> r
          | (l,r) <- ppSubSigLike unicode (unLoc patTy) argDocs [] (dcolon unicode)
          ]

    patTy = hsSigType typ

    mDoc = fmap _doc $ combineDocumentation doc


-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: TyClDecl DocNameI -> Bool -> LaTeX
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

ppAppDocNameTyVarBndrs :: Bool -> DocName -> [LHsTyVarBndr DocNameI] -> LaTeX
ppAppDocNameTyVarBndrs unicode n vs =
    ppTypeApp n vs ppDN (ppHsTyVarBndr unicode . unLoc)
  where
    ppDN = ppBinder . nameOccName . getName


-- | Print an application of a DocName to its list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocNameI] -> Bool -> LaTeX
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


ppLContext, ppLContextNoArrow :: Located (HsContext DocNameI) -> Bool -> LaTeX
ppLContext        = ppContext        . unLoc
ppLContextNoArrow = ppContextNoArrow . unLoc

ppContextNoLocsMaybe :: [HsType DocNameI] -> Bool -> Maybe LaTeX
ppContextNoLocsMaybe [] _ = Nothing
ppContextNoLocsMaybe cxt unicode = Just $ pp_hs_context cxt unicode

ppContextNoArrow :: HsContext DocNameI -> Bool -> LaTeX
ppContextNoArrow cxt unicode = fromMaybe empty $
                               ppContextNoLocsMaybe (map unLoc cxt) unicode


ppContextNoLocs :: [HsType DocNameI] -> Bool -> LaTeX
ppContextNoLocs cxt unicode = maybe empty (<+> darrow unicode) $
                              ppContextNoLocsMaybe cxt unicode


ppContext :: HsContext DocNameI -> Bool -> LaTeX
ppContext cxt unicode = ppContextNoLocs (map unLoc cxt) unicode


pp_hs_context :: [HsType DocNameI] -> Bool -> LaTeX
pp_hs_context []  _       = empty
pp_hs_context [p] unicode = ppType unicode p
pp_hs_context cxt unicode = parenList (map (ppType unicode) cxt)


-------------------------------------------------------------------------------
-- * Types and contexts
-------------------------------------------------------------------------------


ppBang :: HsSrcBang -> LaTeX
ppBang (HsSrcBang _ _ SrcStrict) = char '!'
ppBang (HsSrcBang _ _ SrcLazy)   = char '~'
ppBang _                         = empty


tupleParens :: HsTupleSort -> [LaTeX] -> LaTeX
tupleParens HsUnboxedTuple = ubxParenList
tupleParens _              = parenList


sumParens :: [LaTeX] -> LaTeX
sumParens = ubxparens . hsep . punctuate (text " | ")


-------------------------------------------------------------------------------
-- * Rendering of HsType
--
-- Stolen from Html and tweaked for LaTeX generation
-------------------------------------------------------------------------------

ppLType, ppLParendType, ppLFunLhType :: Bool -> Located (HsType DocNameI) -> LaTeX
ppLType       unicode y = ppType unicode (unLoc y)
ppLParendType unicode y = ppParendType unicode (unLoc y)
ppLFunLhType  unicode y = ppFunLhType unicode (unLoc y)


ppType, ppParendType, ppFunLhType :: Bool -> HsType DocNameI -> LaTeX
ppType       unicode ty = ppr_mono_ty (reparenTypePrec PREC_TOP ty) unicode
ppParendType unicode ty = ppr_mono_ty (reparenTypePrec PREC_TOP ty) unicode
ppFunLhType  unicode ty = ppr_mono_ty (reparenTypePrec PREC_FUN ty) unicode

ppHsTyVarBndr :: Bool -> HsTyVarBndr DocNameI -> LaTeX
ppHsTyVarBndr _ (UserTyVar _ (L _ name)) = ppDocName name
ppHsTyVarBndr unicode (KindedTyVar _ (L _ name) kind) =
  parens (ppDocName name) <+> dcolon unicode <+> ppLKind unicode kind
ppHsTyVarBndr _ (XTyVarBndr _) = panic "haddock:ppHsTyVarBndr"

ppLKind :: Bool -> LHsKind DocNameI -> LaTeX
ppLKind unicode y = ppKind unicode (unLoc y)

ppKind :: Bool -> HsKind DocNameI -> LaTeX
ppKind unicode ki = ppr_mono_ty (reparenTypePrec PREC_TOP ki) unicode


-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppr_mono_lty :: LHsType DocNameI -> Bool -> LaTeX
ppr_mono_lty ty unicode = ppr_mono_ty (unLoc ty) unicode


ppr_mono_ty :: HsType DocNameI -> Bool -> LaTeX
ppr_mono_ty (HsForAllTy _ tvs ty) unicode
  = sep [ hsep (forallSymbol unicode : ppTyVars tvs) <> dot
        , ppr_mono_lty ty unicode ]
ppr_mono_ty (HsQualTy _ ctxt ty) unicode
  = sep [ ppLContext ctxt unicode
        , ppr_mono_lty ty unicode ]
ppr_mono_ty (HsFunTy _ ty1 ty2)   u
  = sep [ ppr_mono_lty ty1 u
        , arrow u <+> ppr_mono_lty ty2 u ]

ppr_mono_ty (HsBangTy _ b ty)     u = ppBang b <> ppLParendType u ty
ppr_mono_ty (HsTyVar _ NotPromoted (L _ name)) _ = ppDocName name
ppr_mono_ty (HsTyVar _ Promoted    (L _ name)) _ = char '\'' <> ppDocName name
ppr_mono_ty (HsTupleTy _ con tys) u = tupleParens con (map (ppLType u) tys)
ppr_mono_ty (HsSumTy _ tys) u       = sumParens (map (ppLType u) tys)
ppr_mono_ty (HsKindSig _ ty kind) u = parens (ppr_mono_lty ty u <+> dcolon u <+> ppLKind u kind)
ppr_mono_ty (HsListTy _ ty)       u = brackets (ppr_mono_lty ty u)
ppr_mono_ty (HsIParamTy _ (L _ n) ty) u = brackets (ppIPName n <+> dcolon u <+> ppr_mono_lty ty u)
ppr_mono_ty (HsSpliceTy {})     _ = error "ppr_mono_ty HsSpliceTy"
ppr_mono_ty (HsRecTy {})        _ = text "{..}"
ppr_mono_ty (XHsType (NHsCoreTy {}))  _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty (HsExplicitListTy _ Promoted tys) u = Pretty.quote $ brackets $ hsep $ punctuate comma $ map (ppLType u) tys
ppr_mono_ty (HsExplicitListTy _ NotPromoted tys) u = brackets $ hsep $ punctuate comma $ map (ppLType u) tys
ppr_mono_ty (HsExplicitTupleTy _ tys) u = Pretty.quote $ parenList $ map (ppLType u) tys

ppr_mono_ty (HsAppTy _ fun_ty arg_ty) unicode
  = hsep [ppr_mono_lty fun_ty unicode, ppr_mono_lty arg_ty unicode]

ppr_mono_ty (HsOpTy _ ty1 op ty2) unicode
  = ppr_mono_lty ty1 unicode <+> ppr_op <+> ppr_mono_lty ty2 unicode
  where
    ppr_op = if not (isSymOcc occName) then char '`' <> ppLDocName op <> char '`' else ppLDocName op
    occName = nameOccName . getName . unLoc $ op

ppr_mono_ty (HsParTy _ ty) unicode
  = parens (ppr_mono_lty ty unicode)
--  = ppr_mono_lty ty unicode

ppr_mono_ty (HsDocTy _ ty _) unicode
  = ppr_mono_lty ty unicode

ppr_mono_ty (HsWildCardTy (AnonWildCard _)) _ = text "\\_"

ppr_mono_ty (HsTyLit _ t) u = ppr_tylit t u
ppr_mono_ty (HsStarTy _ isUni) unicode = starSymbol (isUni || unicode)


ppr_tylit :: HsTyLit -> Bool -> LaTeX
ppr_tylit (HsNumTy _ n) _ = integer n
ppr_tylit (HsStrTy _ s) _ = text (show s)
  -- XXX: Ok in verbatim, but not otherwise
  -- XXX: Do something with Unicode parameter?


-------------------------------------------------------------------------------
-- * Names
-------------------------------------------------------------------------------


ppBinder :: OccName -> LaTeX
ppBinder n
  | isInfixName n = parens $ ppOccName n
  | otherwise     = ppOccName n

ppBinderInfix :: OccName -> LaTeX
ppBinderInfix n
  | isInfixName n = ppOccName n
  | otherwise     = cat [ char '`', ppOccName n, char '`' ]

isInfixName :: OccName -> Bool
isInfixName n = isVarSym n || isConSym n

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
  markupBold                 = \p v -> bold (p v),
  markupMonospaced           = \p _ -> tt (p Mono),
  markupUnorderedList        = \p v -> itemizedList (map ($v) p) $$ text "",
  markupPic                  = \p _ -> markupPic p,
  markupMathInline           = \p _ -> markupMathInline p,
  markupMathDisplay          = \p _ -> markupMathDisplay p,
  markupOrderedList          = \p v -> enumeratedList (map ($v) p) $$ text "",
  markupDefList              = \l v -> descriptionList (map (\(a,b) -> (a v, b v)) l),
  markupCodeBlock            = \p _ -> quote (verb (p Verb)) $$ text "",
  markupHyperlink            = \l _ -> markupLink l,
  markupAName                = \_ _ -> empty,
  markupProperty             = \p _ -> quote $ verb $ text p,
  markupExample              = \e _ -> quote $ verb $ text $ unlines $ map exampleToString e,
  markupHeader               = \(Header l h) p -> header l (h p),
  markupTable                = \(Table h b) p -> table h b p
  }
  where
    header 1 d = text "\\section*" <> braces d
    header 2 d = text "\\subsection*" <> braces d
    header l d
      | l > 0 && l <= 6 = text "\\subsubsection*" <> braces d
    header l _ = error $ "impossible header level in LaTeX generation: " ++ show l

    table _ _ _ = text "{TODO: Table}"

    fixString Plain s = latexFilter s
    fixString Verb  s = s
    fixString Mono  s = latexMonoFilter s

    markupLink (Hyperlink url mLabel) = case mLabel of
      Just label -> text "\\href" <> braces (text url) <> braces (text label)
      Nothing    -> text "\\url"  <> braces (text url)

    -- Is there a better way of doing this? Just a space is an aribtrary choice.
    markupPic (Picture uri title) = parens (imageText title)
      where
        imageText Nothing = beg
        imageText (Just t) = beg <> text " " <> text t

        beg = text "image: " <> text uri

    markupMathInline mathjax = text "\\(" <> text mathjax <> text "\\)"

    markupMathDisplay mathjax = text "\\[" <> text mathjax <> text "\\]"

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
documentationToLaTeX = fmap docToLaTeX . fmap _doc . combineDocumentation


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

bold :: LaTeX -> LaTeX
bold ltx = text "\\textbf" <> braces ltx

verb :: LaTeX -> LaTeX
verb doc = text "{\\haddockverb\\begin{verbatim}" $$ doc <> text "\\end{verbatim}}"
   -- NB. swallow a trailing \n in the verbatim text by appending the
   -- \end{verbatim} directly, otherwise we get spurious blank lines at the
   -- end of code blocks.


quote :: LaTeX -> LaTeX
quote doc = text "\\begin{quote}" $$ doc $$ text "\\end{quote}"


dcolon, arrow, darrow, forallSymbol, starSymbol :: Bool -> LaTeX
dcolon unicode = text (if unicode then "∷" else "::")
arrow  unicode = text (if unicode then "→" else "->")
darrow unicode = text (if unicode then "⇒" else "=>")
forallSymbol unicode = text (if unicode then "∀" else "forall")
starSymbol unicode = text (if unicode then "★" else "*")

dot :: LaTeX
dot = char '.'


parenList :: [LaTeX] -> LaTeX
parenList = parens . hsep . punctuate comma


ubxParenList :: [LaTeX] -> LaTeX
ubxParenList = ubxparens . hsep . punctuate comma


ubxparens :: LaTeX -> LaTeX
ubxparens h = text "(#" <> h <> text "#)"


nl :: LaTeX
nl = text "\\\\"


keyword :: String -> LaTeX
keyword = text


infixr 4 <->  -- combining table cells
(<->) :: LaTeX -> LaTeX -> LaTeX
a <-> b = a <+> char '&' <+> b
