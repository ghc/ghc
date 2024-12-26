{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-----------------------------------------------------------------------------

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
module Haddock.Backends.LaTeX
  ( ppLaTeX
  ) where

import Control.Monad
import Data.Char
import Data.Foldable (toList)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import GHC hiding (fromMaybeContext)
import GHC.Core.Type (Specificity (..))
import GHC.Data.FastString (unpackFS)
import GHC.Types.Name (getOccString, nameOccName, tidyNameOcc)
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader (rdrNameOcc)
import GHC.Utils.Ppr hiding (Doc, quote)
import qualified GHC.Utils.Ppr as Pretty
import System.Directory
import System.FilePath
import Prelude hiding ((<>))

import Documentation.Haddock.Markup
import Haddock.Doc (combineDocumentation)
import Haddock.GhcUtils
import Haddock.Types
import Haddock.Utils

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

ppLaTeX
  :: String -- Title
  -> Maybe String -- Package name
  -> [Interface]
  -> FilePath -- destination directory
  -> Maybe (Doc GHC.RdrName) -- prologue text, maybe
  -> Maybe String -- style file
  -> FilePath
  -> IO ()
ppLaTeX title packageStr visible_ifaces odir prologue maybe_style libdir =
  do
    createDirectoryIfMissing True odir
    when (Maybe.isNothing maybe_style) $
      copyFile (libdir </> "latex" </> haddockSty) (odir </> haddockSty)
    ppLaTeXTop title packageStr odir prologue maybe_style visible_ifaces
    mapM_ (ppLaTeXModule title odir) visible_ifaces

haddockSty :: FilePath
haddockSty = "haddock.sty"

type LaTeX = Pretty.Doc

-- | Default way of rendering a 'LaTeX'. The width is 90 by default (since 100
-- often overflows the line).
latex2String :: LaTeX -> String
latex2String = fullRender (PageMode True) 90 1 txtPrinter ""

ppLaTeXTop
  :: String
  -> Maybe String
  -> FilePath
  -> Maybe (Doc GHC.RdrName)
  -> Maybe String
  -> [Interface]
  -> IO ()
ppLaTeXTop doctitle packageStr odir prologue maybe_style ifaces = do
  let tex =
        vcat
          [ text "\\documentclass{book}"
          , text "\\usepackage" <> braces (maybe (text "haddock") text maybe_style)
          , text "\\begin{document}"
          , text "\\begin{titlepage}"
          , text "\\begin{haddocktitle}"
          , text doctitle
          , text "\\end{haddocktitle}"
          , case prologue of
              Nothing -> empty
              Just d ->
                vcat
                  [ text "\\begin{haddockprologue}"
                  , rdrDocToLaTeX d
                  , text "\\end{haddockprologue}"
                  ]
          , text "\\end{titlepage}"
          , text "\\tableofcontents"
          , vcat [text "\\input" <> braces (text mdl) | mdl <- mods]
          , text "\\end{document}"
          ]

      mods = sort (map (moduleBasename . ifaceMod) ifaces)

      filename = odir </> (Maybe.fromMaybe "haddock" packageStr <.> "tex")

  writeUtf8File filename (show tex)

ppLaTeXModule :: String -> FilePath -> Interface -> IO ()
ppLaTeXModule _title odir iface = do
  createDirectoryIfMissing True odir
  let
    mdl = ifaceMod iface
    mdl_str = moduleString mdl

    exports = ifaceRnExportItems iface

    tex =
      vcat
        [ text "\\haddockmoduleheading" <> braces (text mdl_str)
        , text "\\label{module:" <> text mdl_str <> char '}'
        , text "\\haddockbeginheader"
        , verb $
            vcat
              [ text "module" <+> text mdl_str <+> lparen
              , text "    "
                  <> fsep
                    ( punctuate (char ',') $
                        map exportListItem $
                          filter forSummary exports
                    )
              , text "  ) where"
              ]
        , text "\\haddockendheader" $$ text ""
        , description
        , body
        ]

    description =
      (Maybe.fromMaybe empty . documentationToLaTeX . ifaceRnDoc) iface

    body = processExports exports
  --
  writeUtf8File (odir </> moduleLaTeXFile mdl) (fullRender (PageMode True) 80 1 txtPrinter "" tex)

-- | Prints out an entry in a module export list.
exportListItem :: ExportItem DocNameI -> LaTeX
exportListItem
  ( ExportDecl
      ( RnExportD
          { rnExpDExpD =
            ( ExportD
                { expDDecl = decl
                , expDSubDocs = subdocs
                }
              )
          }
        )
    ) =
    let (leader, names) = declNames decl
        go (n, _)
          | isDefaultMethodOcc (occName n) = Nothing
          | otherwise = Just $ ppDocBinder n
     in sep (punctuate comma [leader <+> ppDocBinder name | name <- names])
          <> case subdocs of
            [] -> empty
            _ -> parens (sep (punctuate comma (Maybe.mapMaybe go subdocs)))
exportListItem (ExportNoDecl y []) =
  ppDocBinder y
exportListItem (ExportNoDecl y subs) =
  ppDocBinder y <> parens (sep (punctuate comma (map ppDocBinder subs)))
exportListItem (ExportModule mdl) =
  text "module" <+> text (moduleString mdl)
exportListItem _ =
  error "exportListItem"

-- Deal with a group of undocumented exports together, to avoid lots
-- of blank vertical space between them.
processExports :: [ExportItem DocNameI] -> LaTeX
processExports [] = empty
processExports (decl : es)
  | Just sig <- isSimpleSig decl =
      multiDecl
        [ ppTypeSig (map getName names) typ False
        | (names, typ) <- sig : sigs
        ]
        $$ processExports es'
  where
    (sigs, es') = spanWith isSimpleSig es
processExports (ExportModule mdl : es) =
  declWithDoc (vcat [text "module" <+> text (moduleString m) | m <- mdl : mdls]) Nothing
    $$ processExports es'
  where
    (mdls, es') = spanWith isExportModule es
processExports (e : es) =
  processExport e $$ processExports es

isSimpleSig :: ExportItem DocNameI -> Maybe ([DocName], HsSigType DocNameI)
isSimpleSig
  ( ExportDecl
      ( RnExportD
          { rnExpDExpD =
            ExportD
              { expDDecl = L _ (SigD _ (TypeSig _ lnames t))
              , expDMbDoc = (Documentation Nothing Nothing, argDocs)
              }
          }
        )
    )
    | Map.null argDocs = Just (map unLoc lnames, unLoc (dropWildCardsI t))
isSimpleSig _ = Nothing

isExportModule :: ExportItem DocNameI -> Maybe Module
isExportModule (ExportModule m) = Just m
isExportModule _ = Nothing

processExport :: ExportItem DocNameI -> LaTeX
processExport (ExportGroup lev _id0 doc) =
  ppDocGroup lev (docToLaTeX doc)
processExport (ExportDecl (RnExportD (ExportD decl pats doc subdocs insts fixities _splice) _)) =
  ppDecl decl pats doc insts subdocs fixities
processExport (ExportNoDecl y []) =
  ppDocName y
processExport (ExportNoDecl y subs) =
  ppDocName y <> parens (sep (punctuate comma (map ppDocName subs)))
processExport (ExportModule mdl) =
  declWithDoc (text "module" <+> text (moduleString mdl)) Nothing
processExport (ExportDoc doc) =
  docToLaTeX $ _doc doc

ppDocGroup :: Int -> LaTeX -> LaTeX
ppDocGroup lev doc = sec lev <> braces doc
  where
    sec 1 = text "\\section"
    sec 2 = text "\\subsection"
    sec 3 = text "\\subsubsection"
    sec _ = text "\\paragraph"

-- | Given a declaration, extract out the names being declared
declNames
  :: LHsDecl DocNameI
  -> ( LaTeX --   to print before each name in an export list
     , [DocName] --   names being declared
     )
declNames (L _ decl) = case decl of
  TyClD _ d -> (empty, [tcdNameI d])
  SigD _ (TypeSig _ lnames _) -> (empty, map unLoc lnames)
  SigD _ (PatSynSig _ lnames _) -> (text "pattern", map unLoc lnames)
  ForD _ (ForeignImport _ (L _ n) _ _ _) -> (empty, [n])
  ForD _ (ForeignExport _ (L _ n) _ _ _) -> (empty, [n])
  _ -> error "declaration not supported by declNames"

forSummary :: (ExportItem DocNameI) -> Bool
forSummary (ExportGroup _ _ _) = False
forSummary (ExportDoc _) = False
forSummary _ = True

moduleLaTeXFile :: Module -> FilePath
moduleLaTeXFile mdl = moduleBasename mdl ++ ".tex"

moduleBasename :: Module -> FilePath
moduleBasename mdl =
  map
    (\c -> if c == '.' then '-' else c)
    (moduleNameString (moduleName mdl))

-------------------------------------------------------------------------------

-- * Decls

-------------------------------------------------------------------------------

-- | Pretty print a declaration
ppDecl
  :: LHsDecl DocNameI
  -- ^ decl to print
  -> [(HsDecl DocNameI, DocForDecl DocName)]
  -- ^ all pattern decls
  -> DocForDecl DocName
  -- ^ documentation for decl
  -> [DocInstance DocNameI]
  -- ^ all instances
  -> [(DocName, DocForDecl DocName)]
  -- ^ all subdocs
  -> [(DocName, Fixity)]
  -- ^ all fixities
  -> LaTeX
ppDecl decl pats (doc, fnArgsDoc) instances subdocs _fxts = case unLoc decl of
  TyClD _ d@FamDecl{} -> ppFamDecl False doc instances d unicode
  TyClD _ d@DataDecl{} -> ppDataDecl pats instances subdocs (Just doc) d unicode
  TyClD _ d@SynDecl{} -> ppTySyn (doc, fnArgsDoc) d unicode
  TyClD _ d@ClassDecl{} -> ppClassDecl instances doc subdocs d unicode
  SigD _ (TypeSig _ lnames ty) -> ppFunSig Nothing (doc, fnArgsDoc) (map unLoc lnames) (dropWildCardsI ty) unicode
  SigD _ (PatSynSig _ lnames ty) -> ppLPatSig (doc, fnArgsDoc) (map unLoc lnames) ty unicode
  ForD _ d -> ppFor (doc, fnArgsDoc) d unicode
  InstD _ _ -> empty
  DerivD _ _ -> empty
  _ -> error "declaration not supported by ppDecl"
  where
    unicode = False

ppFor :: DocForDecl DocName -> ForeignDecl DocNameI -> Bool -> LaTeX
ppFor doc (ForeignImport _ (L _ name) typ _ _) unicode =
  -- MODS_TODO need to pprint modifiers
  ppFunSig Nothing doc [name] typ unicode
ppFor _ _ _ = error "ppFor error in Haddock.Backends.LaTeX"

--  error "foreign declarations are currently not supported by --latex"

-------------------------------------------------------------------------------

-- * Type families

-------------------------------------------------------------------------------

-- | Pretty-print a data\/type family declaration
ppFamDecl
  :: Bool
  -- ^ is the family associated?
  -> Documentation DocName
  -- ^ this decl's docs
  -> [DocInstance DocNameI]
  -- ^ relevant instances
  -> TyClDecl DocNameI
  -- ^ family to print
  -> Bool
  -- ^ unicode
  -> LaTeX
ppFamDecl associated doc instances decl@(FamDecl{}) unicode =
  declWithDoc
    (ppFamHeader (tcdFam decl) unicode associated <+> whereBit)
    (if null body then Nothing else Just (vcat body))
    $$ instancesBit
  where
    body = Maybe.catMaybes [familyEqns, documentationToLaTeX doc]

    whereBit = case fdInfo (tcdFam decl) of
      ClosedTypeFamily _ -> keyword "where"
      _ -> empty

    familyEqns
      | FamilyDecl{fdInfo = ClosedTypeFamily (Just eqns)} <- tcdFam decl
      , not (null eqns) =
          Just
            ( text "\\haddockbeginargs"
                $$ vcat [decltt (ppFamDeclEqn eqn) <+> nl | L _ eqn <- eqns]
                $$ text "\\end{tabulary}\\par"
            )
      | otherwise = Nothing

    -- Individual equations of a closed type family
    ppFamDeclEqn :: TyFamInstEqn DocNameI -> LaTeX
    ppFamDeclEqn
      ( FamEqn
          { feqn_tycon = L _ n
          , feqn_rhs = rhs
          , feqn_pats = ts
          }
        ) =
        hsep
          [ ppAppNameTypeArgs n ts unicode
          , equals
          , ppType unicode (unLoc rhs)
          ]

    instancesBit = ppDocInstances unicode instances

ppFamDecl _ _ _ _ _ = error "ppFamDecl"
  -- Should never be called on a non-FamDecl

-- | Print the LHS of a type\/data family declaration.
ppFamHeader
  :: FamilyDecl DocNameI
  -- ^ family header to print
  -> Bool
  -- ^ unicode
  -> Bool
  -- ^ is the family associated?
  -> LaTeX
ppFamHeader
  ( FamilyDecl
      { fdLName = L _ name
      , fdTyVars = tvs
      , fdInfo = info
      , fdResultSig = L _ result
      , fdInjectivityAnn = injectivity
      }
    )
  unicode
  associated =
    famly leader <+> famName <+> famSig <+> injAnn
    where
      leader = case info of
        OpenTypeFamily -> keyword "type"
        ClosedTypeFamily _ -> keyword "type"
        DataFamily -> keyword "data"

      famly
        | associated = id
        | otherwise = (<+> keyword "family")

      famName = ppAppDocNameTyVarBndrs unicode name (hsq_explicit tvs)

      famSig = case result of
        NoSig _ -> empty
        KindSig _ kind -> dcolon unicode <+> ppLKind unicode kind
        TyVarSig _ (L _ bndr) -> equals <+> ppHsTyVarBndr unicode bndr

      injAnn = case injectivity of
        Nothing -> empty
        Just (L _ (InjectivityAnn _ lhs rhs)) ->
          hsep
            ( decltt (text "|")
                : ppLDocName lhs
                : arrow unicode
                : map ppLDocName rhs
            )
        Just _ -> empty

-------------------------------------------------------------------------------

-- * Type Synonyms

-------------------------------------------------------------------------------

-- we skip type patterns for now
ppTySyn :: DocForDecl DocName -> TyClDecl DocNameI -> Bool -> LaTeX
ppTySyn
  doc
  ( SynDecl
      { tcdLName = L _ name
      , tcdTyVars = ltyvars
      , tcdRhs = ltype
      }
    )
  unicode =
    ppTypeOrFunSig (mkHsImplicitSigTypeI ltype) doc (full, hdr, char '=') unicode
    where
      hdr =
        hsep
          ( keyword "type"
              : ppDocBinder name
              : map ppWcSymName (tyvarNames ltyvars)
          )
      full = hdr <+> char '=' <+> ppLType unicode ltype
ppTySyn _ _ _ = error "declaration not supported by ppTySyn"

-------------------------------------------------------------------------------

-- * Function signatures

-------------------------------------------------------------------------------

ppFunSig
  :: Maybe LaTeX
  -- ^ a prefix to put right before the signature
  -> DocForDecl DocName
  -- ^ documentation
  -> [DocName]
  -- ^ pattern names in the pattern signature
  -> LHsSigType DocNameI
  -- ^ type of the pattern synonym
  -> Bool
  -- ^ unicode
  -> LaTeX
ppFunSig leader doc docnames (L _ typ) unicode =
  ppTypeOrFunSig
    typ
    doc
    ( lead $ ppTypeSig names typ False
    , lead $ hsep . punctuate comma $ map ppSymName names
    , dcolon unicode
    )
    unicode
  where
    names = map getName docnames
    lead = maybe id (<+>) leader

-- | Pretty-print a pattern synonym
ppLPatSig
  :: DocForDecl DocName
  -- ^ documentation
  -> [DocName]
  -- ^ pattern names in the pattern signature
  -> LHsSigType DocNameI
  -- ^ type of the pattern synonym
  -> Bool
  -- ^ unicode
  -> LaTeX
ppLPatSig doc docnames ty unicode =
  ppFunSig (Just (keyword "pattern")) doc docnames ty unicode

-- | Pretty-print a type, adding documentation to the whole type and its
-- arguments as needed.
ppTypeOrFunSig
  :: HsSigType DocNameI
  -> DocForDecl DocName
  -- ^ documentation
  -> ( LaTeX --   first-line (no-argument docs only)
     , LaTeX --   first-line (argument docs only)
     , LaTeX --   type prefix (argument docs only)
     )
  -> Bool
  -- ^ unicode
  -> LaTeX
ppTypeOrFunSig typ (doc, argDocs) (pref1, pref2, sep0) unicode
  | Map.null argDocs = declWithDoc pref1 (documentationToLaTeX doc)
  | otherwise =
      declWithDoc pref2 $
        Just $
          text "\\haddockbeginargs"
            $$ vcat (map (uncurry (<->)) (ppSubSigLike unicode typ argDocs [] sep0))
            $$ text "\\end{tabulary}\\par"
            $$ Maybe.fromMaybe empty (documentationToLaTeX doc)

-- | This splits up a type signature along @->@ and adds docs (when they exist)
-- to the arguments. The output is a list of (leader/seperator, argument and
-- its doc)
ppSubSigLike
  :: Bool
  -- ^ unicode
  -> HsSigType DocNameI
  -- ^ type signature
  -> FnArgsDoc DocName
  -- ^ docs to add
  -> [(DocName, DocForDecl DocName)]
  -- ^ all subdocs (useful when we have `HsRecTy`)
  -> LaTeX
  -- ^ seperator (beginning of first line)
  -> [(LaTeX, LaTeX)]
  -- ^ arguments (leader/sep, type)
ppSubSigLike unicode typ argDocs subdocs leader = do_sig_args 0 leader typ
  where
    do_sig_args :: Int -> LaTeX -> HsSigType DocNameI -> [(LaTeX, LaTeX)]
    do_sig_args n leader (HsSig{sig_bndrs = outer_bndrs, sig_body = ltype}) =
      case outer_bndrs of
        HsOuterExplicit{hso_bndrs = bndrs} ->
          [
            ( decltt leader
            , decltt (ppHsForAllTelescope (mkHsForAllInvisTeleI bndrs) unicode)
                <+> ppLType unicode ltype
            )
          ]
        HsOuterImplicit{} -> do_largs n leader ltype

    do_largs :: Int -> LaTeX -> LHsType DocNameI -> [(LaTeX, LaTeX)]
    do_largs n leader (L _ t) = do_args n leader t

    arg_doc n = rDoc . fmap _doc $ Map.lookup n argDocs

    do_args :: Int -> LaTeX -> HsType DocNameI -> [(LaTeX, LaTeX)]
    do_args _n leader (HsForAllTy _ tele ltype) =
      [
        ( decltt leader
        , decltt (ppHsForAllTelescope tele unicode)
            <+> ppLType unicode ltype
        )
      ]
    do_args n leader (HsQualTy _ lctxt ltype) =
      ( decltt leader
      , decltt (ppLContextNoArrow lctxt unicode) <+> nl
      )
        : do_largs n (darrow unicode) ltype
    do_args n leader (HsFunTy _ _w (L _ (HsRecTy _ fields)) r) =
      [ (decltt ldr, latex <+> nl)
      | (L _ field, ldr) <- zip fields (leader <+> gadtOpen : repeat gadtComma)
      , let latex = ppSideBySideField subdocs unicode field
      ]
        ++ do_largs (n + 1) (gadtEnd <+> arrow unicode) r
    do_args n leader (HsFunTy _ _w lt r) =
      (decltt leader, decltt (ppLFunLhType unicode lt) <-> arg_doc n <+> nl)
        : do_largs (n + 1) (arrow unicode) r
    do_args n leader t =
      [(decltt leader, decltt (ppType unicode t) <-> arg_doc n <+> nl)]

    -- FIXME: this should be done more elegantly
    --
    -- We need 'gadtComma' and 'gadtEnd' to line up with the `{` from
    -- 'gadtOpen', so we add 3 spaces to cover for `-> `/`:: ` (3 in unicode
    -- mode since `->` and `::` are rendered as single characters.
    gadtComma = hcat (replicate (if unicode then 3 else 4) (char ' ')) <> char ','
    gadtEnd = hcat (replicate (if unicode then 3 else 4) (char ' ')) <> char '}'
    gadtOpen = char '{'

ppTypeSig :: [Name] -> HsSigType DocNameI -> Bool -> LaTeX
ppTypeSig nms ty unicode =
  hsep (punctuate comma $ map ppSymName nms)
    <+> dcolon unicode
    <+> ppSigType unicode ty

ppHsOuterTyVarBndrs :: RenderableBndrFlag flag => HsOuterTyVarBndrs flag DocNameI -> Bool -> LaTeX
ppHsOuterTyVarBndrs (HsOuterImplicit{}) _ = empty
ppHsOuterTyVarBndrs (HsOuterExplicit{hso_bndrs = bndrs}) unicode =
  hsep (forallSymbol unicode : ppTyVars unicode bndrs) <> dot

ppHsForAllTelescope :: HsForAllTelescope DocNameI -> Bool -> LaTeX
ppHsForAllTelescope tele unicode = case tele of
  HsForAllVis{hsf_vis_bndrs = bndrs} ->
    hsep (forallSymbol unicode : ppTyVars unicode bndrs) <> text "\\" <> arrow unicode
  HsForAllInvis{hsf_invis_bndrs = bndrs} ->
    hsep (forallSymbol unicode : ppTyVars unicode bndrs) <> dot

ppTyVars :: RenderableBndrFlag flag => Bool -> [LHsTyVarBndr flag DocNameI] -> [LaTeX]
ppTyVars unicode tvs = map (ppHsTyVarBndr unicode . unLoc) tvs

tyvarNames :: LHsQTyVars DocNameI -> [Maybe Name]
tyvarNames = map (fmap getName . hsLTyVarNameI) . hsQTvExplicit

declWithDoc :: LaTeX -> Maybe LaTeX -> LaTeX
declWithDoc decl doc =
  text "\\begin{haddockdesc}"
    $$ text "\\item[\\begin{tabular}{@{}l}"
    $$ text (latexMonoFilter (latex2String decl))
    $$ text "\\end{tabular}]"
    $$ maybe empty (\x -> text "{\\haddockbegindoc" $$ x <> text "}") doc
    $$ text "\\end{haddockdesc}"

-- in a group of decls, we don't put them all in the same tabular,
-- because that would prevent the group being broken over a page
-- boundary (breaks Foreign.C.Error for example).
multiDecl :: [LaTeX] -> LaTeX
multiDecl decls =
  text "\\begin{haddockdesc}"
    $$ vcat
      [ text "\\item[\\begin{tabular}{@{}l}"
        $$ text (latexMonoFilter (latex2String decl))
        $$ text "\\end{tabular}]"
      | decl <- decls
      ]
    $$ text "\\end{haddockdesc}"

-------------------------------------------------------------------------------

-- * Rendering Doc

-------------------------------------------------------------------------------

maybeDoc :: Maybe (Doc DocName) -> LaTeX
maybeDoc = maybe empty docToLaTeX

-- for table cells, we strip paragraphs out to avoid extra vertical space
-- and don't add a quote environment.
rDoc :: Maybe (Doc DocName) -> LaTeX
rDoc = maybeDoc . fmap latexStripTrailingWhitespace

-------------------------------------------------------------------------------

-- * Class declarations

-------------------------------------------------------------------------------

ppClassHdr
  :: Bool
  -> Maybe (LocatedC [LHsType DocNameI])
  -> DocName
  -> LHsQTyVars DocNameI
  -> [LHsFunDep DocNameI]
  -> Bool
  -> LaTeX
ppClassHdr summ lctxt n tvs fds unicode =
  keyword "class"
    <+> (if not (null $ fromMaybeContext lctxt) then ppLContext lctxt unicode else empty)
    <+> ppAppDocNameNames summ n (tyvarNames tvs)
    <+> ppFds fds unicode

-- ppFds :: [Located ([LocatedA DocName], [LocatedA DocName])] -> Bool -> LaTeX
ppFds :: [LHsFunDep DocNameI] -> Bool -> LaTeX
ppFds fds unicode =
  if null fds
    then empty
    else char '|' <+> hsep (punctuate comma (map (fundep . unLoc) fds))
  where
    fundep (FunDep _ vars1 vars2) =
      hsep (map (ppDocName . unLoc) vars1)
        <+> arrow unicode
        <+> hsep (map (ppDocName . unLoc) vars2)
    fundep (XFunDep _) = error "ppFds"

-- TODO: associated type defaults, docs on default methods
ppClassDecl
  :: [DocInstance DocNameI]
  -> Documentation DocName
  -> [(DocName, DocForDecl DocName)]
  -> TyClDecl DocNameI
  -> Bool
  -> LaTeX
ppClassDecl
  instances
  doc
  subdocs
  ( ClassDecl
      { tcdCtxt = lctxt
      , tcdLName = lname
      , tcdTyVars = ltyvars
      , tcdFDs = lfds
      , tcdSigs = lsigs
      , tcdATs = ats
      , tcdATDefs = at_defs
      }
    )
  unicode =
    declWithDoc classheader (if null body then Nothing else Just (vcat body))
      $$ instancesBit
    where
      classheader
        | null lsigs = hdr unicode
        | otherwise = hdr unicode <+> keyword "where"

      hdr = ppClassHdr False lctxt (unLoc lname) ltyvars lfds

      body = Maybe.catMaybes [documentationToLaTeX doc, body_]

      body_
        | null lsigs, null ats, null at_defs = Nothing
        | null ats, null at_defs = Just methodTable
        | otherwise = Just (atTable $$ methodTable)

      atTable =
        text "\\haddockpremethods{}" <> emph (text "Associated Types")
          $$ vcat
            [ ppFamDecl True (fst doc) [] (FamDecl noExtField decl) True
            | L _ decl <- ats
            , let name = unLoc . fdLName $ decl
                  doc = lookupAnySubdoc name subdocs
            ]

      methodTable =
        text "\\haddockpremethods{}" <> emph (text "Methods")
          $$ vcat
            [ ppFunSig leader doc names typ unicode
            | L _ (ClassOpSig _ is_def lnames typ) <- lsigs
            , let doc
                    | is_def = noDocForDecl
                    | otherwise = lookupAnySubdoc firstName subdocs
                  names = map (cleanName . unLoc) lnames
                  leader = if is_def then Just (keyword "default") else Nothing
                  firstName =
                    case Maybe.listToMaybe names of
                      Nothing -> error "No names. An invariant was broken. Please report this to the Haddock project"
                      Just hd -> hd
            ]
      -- N.B. taking just the first name is ok. Signatures with multiple
      -- names are expanded so that each name gets its own signature.
      -- Get rid of the ugly '$dm' prefix on default method names
      cleanName n
        | isDefaultMethodOcc (occName n)
        , '$' : 'd' : 'm' : occStr <- getOccString n =
            setName (tidyNameOcc (getName n) (mkOccName varName occStr)) n
        | otherwise = n

      instancesBit = ppDocInstances unicode instances
ppClassDecl _ _ _ _ _ = error "declaration type not supported by ppShortClassDecl"

ppDocInstances :: Bool -> [DocInstance DocNameI] -> LaTeX
ppDocInstances _unicode [] = empty
ppDocInstances unicode (i : rest)
  | Just ihead <- isUndocdInstance i =
      declWithDoc (vcat (map (ppInstDecl unicode) (ihead : is))) Nothing
        $$ ppDocInstances unicode rest'
  | otherwise =
      ppDocInstance unicode i $$ ppDocInstances unicode rest
  where
    (is, rest') = spanWith isUndocdInstance rest

isUndocdInstance :: DocInstance a -> Maybe (InstHead a)
isUndocdInstance (i, Nothing, _, _) = Just i
isUndocdInstance (i, Just (MetaDoc _ DocEmpty), _, _) = Just i
isUndocdInstance _ = Nothing

-- | Print a possibly commented instance. The instance header is printed inside
-- an 'argBox'. The comment is printed to the right of the box in normal comment
-- style.
ppDocInstance :: Bool -> DocInstance DocNameI -> LaTeX
ppDocInstance unicode (instHead, doc, _, _) =
  declWithDoc (ppInstDecl unicode instHead) (fmap docToLaTeX $ fmap _doc doc)

ppInstDecl :: Bool -> InstHead DocNameI -> LaTeX
ppInstDecl unicode (InstHead{..}) = case ihdInstType of
  ClassInst ctx _ _ _ -> keyword "instance" <+> ppContextNoLocs ctx unicode <+> typ
  TypeInst rhs -> keyword "type" <+> keyword "instance" <+> typ <+> tibody rhs
  DataInst dd@(DataDecl {}) ->
    let cons = dd_cons (tcdDataDefn dd)
        pref = case cons of NewTypeCon _ -> keyword "newtype"; DataTypeCons _ _ -> keyword "data"
     in pref <+> keyword "instance" <+> typ
  DataInst _ -> error "ppInstDecl"
  where
    typ = ppAppNameTypes ihdClsName ihdTypes unicode
    tibody = maybe empty (\t -> equals <+> ppType unicode t)

lookupAnySubdoc
  :: Eq name1
  => name1
  -> [(name1, DocForDecl name2)]
  -> DocForDecl name2
lookupAnySubdoc n subdocs = case lookup n subdocs of
  Nothing -> noDocForDecl
  Just docs -> docs

-------------------------------------------------------------------------------

-- * Data & newtype declarations

-------------------------------------------------------------------------------

-- | Pretty-print a data declaration
ppDataDecl
  :: [(HsDecl DocNameI, DocForDecl DocName)]
  -- ^ relevant patterns
  -> [DocInstance DocNameI]
  -- ^ relevant instances
  -> [(DocName, DocForDecl DocName)]
  -- ^ relevant decl docs
  -> Maybe (Documentation DocName)
  -- ^ this decl's docs
  -> TyClDecl DocNameI
  -- ^ data decl to print
  -> Bool
  -- ^ unicode
  -> LaTeX
ppDataDecl pats instances subdocs doc dataDecl@(DataDecl {}) unicode =
  declWithDoc
    (ppDataHeader dataDecl unicode <+> whereBit)
    (if null body then Nothing else Just (vcat body))
    $$ instancesBit
  where
    cons = dd_cons (tcdDataDefn dataDecl)

    body = Maybe.catMaybes [doc >>= documentationToLaTeX, constrBit, patternBit]

    (whereBit, leaders)
      | null cons
      , null pats =
          (empty, [])
      | null cons = (text "where", repeat empty)
      | otherwise = case toList cons of
          L _ ConDeclGADT{} : _ -> (text "where", repeat empty)
          _ -> (empty, (decltt (text "=") : repeat (decltt (text "|"))))

    constrBit
      | null cons = Nothing
      | otherwise =
          Just $
            text "\\enspace" <+> emph (text "Constructors") <> text "\\par"
              $$ text "\\haddockbeginconstrs"
              $$ vcat (zipWith (ppSideBySideConstr subdocs unicode) leaders (toList cons))
              $$ text "\\end{tabulary}\\par"

    patternBit
      | null pats = Nothing
      | otherwise =
          Just $
            text "\\enspace" <+> emph (text "Bundled Patterns") <> text "\\par"
              $$ text "\\haddockbeginconstrs"
              $$ vcat
                [ empty <-> ppSideBySidePat lnames typ d unicode
                | (SigD _ (PatSynSig _ lnames typ), d) <- pats
                ]
              $$ text "\\end{tabulary}\\par"

    instancesBit = ppDocInstances unicode instances
ppDataDecl _ _ _ _ _ _ = error "ppDataDecl"
  -- Should never be called on a non-DataDecl

-- ppConstrHdr is for (non-GADT) existentials constructors' syntax
ppConstrHdr
  :: Bool
  -- ^ print explicit foralls
  -> [LHsTyVarBndr Specificity DocNameI]
  -- ^ type variables
  -> HsContext DocNameI
  -- ^ context
  -> Bool
  -- ^ unicode
  -> LaTeX
ppConstrHdr forall_ tvs ctxt unicode = ppForall <> ppCtxt
  where
    ppForall
      | null tvs || not forall_ = empty
      | otherwise = ppHsForAllTelescope (mkHsForAllInvisTeleI tvs) unicode

    ppCtxt
      | null ctxt = empty
      | otherwise = ppContextNoArrow ctxt unicode <+> darrow unicode <> space

-- | Pretty-print a constructor
ppSideBySideConstr
  :: [(DocName, DocForDecl DocName)]
  -- ^ all decl docs
  -> Bool
  -- ^ unicode
  -> LaTeX
  -- ^ prefix to decl
  -> LConDecl DocNameI
  -- ^ constructor decl
  -> LaTeX
ppSideBySideConstr subdocs unicode leader (L _ con) =
  leader <-> decltt decl <-> rDoc mbDoc <+> nl
    $$ fieldPart
  where
    -- Find the name of a constructors in the decl (`getConName` always returns
    -- a non-empty list)
    L _ aConName :| _ = getConNamesI con

    occ = toList $ nameOccName . getName . unLoc <$> getConNamesI con

    ppOcc = cat (punctuate comma (map ppBinder occ))
    ppOccInfix = cat (punctuate comma (map ppBinderInfix occ))

    -- Extract out the map of of docs corresponding to the constructors arguments
    argDocs = maybe Map.empty snd (lookup aConName subdocs)
    hasArgDocs = not $ Map.null argDocs

    -- First line of the constructor (no doc, no fields, single-line)
    decl = case con of
      ConDeclH98
        { con_args = det
        , con_ex_tvs = tyVars
        , con_forall = forall_
        , con_mb_cxt = cxt
        } ->
          let context = fromMaybeContext cxt
              header_ = ppConstrHdr forall_ tyVars context unicode
           in case det of
                -- Prefix constructor, e.g. 'Just a'
                PrefixCon _ args
                  | hasArgDocs -> header_ <+> ppOcc
                  | otherwise ->
                      hsep
                        [ header_
                        , ppOcc
                        , hsep (map (ppLParendType unicode . hsScaledThing) args)
                        ]
                -- Record constructor, e.g. 'Identity { runIdentity :: a }'
                RecCon _ -> header_ <+> ppOcc
                -- Infix constructor, e.g. 'a :| [a]'
                InfixCon arg1 arg2
                  | hasArgDocs -> header_ <+> ppOcc
                  | otherwise ->
                      hsep
                        [ header_
                        , ppLParendType unicode (hsScaledThing arg1)
                        , ppOccInfix
                        , ppLParendType unicode (hsScaledThing arg2)
                        ]
      ConDeclGADT{}
        | hasArgDocs || not (isEmpty fieldPart) -> ppOcc
        | otherwise ->
            hsep
              [ ppOcc
              , dcolon unicode
              , -- ++AZ++ make this prepend "{..}" when it is a record style GADT
                ppLSigType unicode (getGADTConType con)
              ]

    fieldPart = case con of
      ConDeclGADT{con_g_args = con_args'} -> case con_args' of
        -- GADT record declarations
        RecConGADT _ _ -> doConstrArgsWithDocs []
        -- GADT prefix data constructors
        PrefixConGADT _ args | hasArgDocs -> doConstrArgsWithDocs (map hsScaledThing args)
        _ -> empty
      ConDeclH98{con_args = con_args'} -> case con_args' of
        -- H98 record declarations
        RecCon (L _ fields) -> doRecordFields fields
        -- H98 prefix data constructors
        PrefixCon _ args | hasArgDocs -> doConstrArgsWithDocs (map hsScaledThing args)
        -- H98 infix data constructor
        InfixCon arg1 arg2 | hasArgDocs -> doConstrArgsWithDocs (map hsScaledThing [arg1, arg2])
        _ -> empty

    doRecordFields fields =
      vcat
        [ empty <-> tt (text begin) <+> ppSideBySideField subdocs unicode field <+> nl
        | (begin, L _ field) <- zip ("\\qquad \\{" : repeat "\\qquad ,") fields
        ]
        $$ empty <-> tt (text "\\qquad \\}") <+> nl

    doConstrArgsWithDocs args = vcat $ map (\l -> empty <-> text "\\qquad" <+> l) $ case con of
      ConDeclH98{} ->
        [ decltt (ppLParendType unicode arg) <-> rDoc (fmap _doc mdoc) <+> nl
        | (i, arg) <- zip [0 ..] args
        , let mdoc = Map.lookup i argDocs
        ]
      ConDeclGADT{} ->
        [ l <+> text "\\enspace" <+> r
        | (l, r) <- ppSubSigLike unicode (unLoc (getGADTConType con)) argDocs subdocs (dcolon unicode)
        ]

    -- don't use "con_doc con", in case it's reconstructed from a .hi file,
    -- or also because we want Haddock to do the doc-parsing, not GHC.
    mbDoc = case getConNamesI con of
      cn :| _ ->
        lookup (unLoc cn) subdocs
          >>= fmap _doc . combineDocumentation . fst

-- | Pretty-print a record field
ppSideBySideField :: [(DocName, DocForDecl DocName)] -> Bool -> ConDeclField DocNameI -> LaTeX
ppSideBySideField subdocs unicode (ConDeclField _ names ltype _ _) =
  -- MODS_TODO need to pprint modifiers
  decltt
    ( cat (punctuate comma (map (ppBinder . rdrNameOcc . foExt . unLoc) names))
        <+> dcolon unicode
        <+> ppLType unicode ltype
    )
    <-> rDoc mbDoc
  where
    -- don't use cd_fld_doc for same reason we don't use con_doc above
    -- Where there is more than one name, they all have the same documentation
    mbDoc = lookup (unLoc . foLabel . unLoc $ name) subdocs >>= fmap _doc . combineDocumentation . fst
    name =
      case Maybe.listToMaybe names of
        Nothing -> error "No names. An invariant was broken. Please report this to the Haddock project"
        Just hd -> hd

-- | Pretty-print a bundled pattern synonym
ppSideBySidePat
  :: [LocatedN DocName]
  -- ^ pattern name(s)
  -> LHsSigType DocNameI
  -- ^ type of pattern(s)
  -> DocForDecl DocName
  -- ^ doc map
  -> Bool
  -- ^ unicode
  -> LaTeX
ppSideBySidePat lnames typ (doc, argDocs) unicode =
  decltt decl <-> rDoc mDoc <+> nl
    $$ fieldPart
  where
    hasArgDocs = not $ Map.null argDocs
    ppOcc = hsep (punctuate comma (map (ppDocBinder . unLoc) lnames))

    decl
      | hasArgDocs = keyword "pattern" <+> ppOcc
      | otherwise =
          hsep
            [ keyword "pattern"
            , ppOcc
            , dcolon unicode
            , ppLSigType unicode typ
            ]

    fieldPart
      | not hasArgDocs = empty
      | otherwise =
          vcat
            [ empty <-> text "\\qquad" <+> l <+> text "\\enspace" <+> r
            | (l, r) <- ppSubSigLike unicode (unLoc typ) argDocs [] (dcolon unicode)
            ]

    mDoc = fmap _doc $ combineDocumentation doc

-- | Print the LHS of a data\/newtype declaration.
-- Currently doesn't handle 'data instance' decls or kind signatures
ppDataHeader :: TyClDecl DocNameI -> Bool -> LaTeX
ppDataHeader
  ( DataDecl
      { tcdLName = L _ name
      , tcdTyVars = tyvars
      , tcdDataDefn = HsDataDefn{dd_cons = cons, dd_ctxt = ctxt}
      }
    )
  unicode =
    -- newtype or data
    ( case cons of
        NewTypeCon _ -> keyword "newtype"
        DataTypeCons False _ -> keyword "data"
        DataTypeCons True _ -> keyword "type" <+> keyword "data"
    )
      <+>
      -- context
      ppLContext ctxt unicode
      <+>
      -- T a b c ..., or a :+: b
      ppAppDocNameNames False name (tyvarNames tyvars)
ppDataHeader _ _ = error "ppDataHeader: illegal argument"

--------------------------------------------------------------------------------

-- * Type applications

--------------------------------------------------------------------------------

ppAppDocNameTyVarBndrs
  :: RenderableBndrFlag flag
  => Bool
  -> DocName
  -> [LHsTyVarBndr flag DocNameI]
  -> LaTeX
ppAppDocNameTyVarBndrs unicode n vs =
  ppTypeApp n vs ppDN (ppHsTyVarBndr unicode . unLoc)
  where
    ppDN = ppBinder . nameOccName . getName

-- | Print an application of a DocName to its list of HsTypes
ppAppNameTypes :: DocName -> [HsType DocNameI] -> Bool -> LaTeX
ppAppNameTypes n ts unicode = ppTypeApp n ts ppDocName (ppParendType unicode)

ppAppNameTypeArgs :: DocName -> [LHsTypeArg DocNameI] -> Bool -> LaTeX
ppAppNameTypeArgs n args@(HsValArg _ _ : HsValArg _ _ : _) unicode =
  ppTypeApp n args ppDocName (ppLHsTypeArg unicode)
ppAppNameTypeArgs n args unicode =
  ppDocName n <+> hsep (map (ppLHsTypeArg unicode) args)

-- | Print an application of a DocName and a list of Names
ppAppDocNameNames :: Bool -> DocName -> [Maybe Name] -> LaTeX
ppAppDocNameNames _summ n ns =
  ppTypeApp n ns (ppBinder . nameOccName . getName) ppWcSymName

-- | General printing of type applications
ppTypeApp :: DocName -> [a] -> (DocName -> LaTeX) -> (a -> LaTeX) -> LaTeX
ppTypeApp n (t1 : t2 : rest) ppDN ppT
  | operator, not . null $ rest = parens opApp <+> hsep (map ppT rest)
  | operator = opApp
  where
    operator = isNameSym . getName $ n
    opApp = ppT t1 <+> ppDN n <+> ppT t2
ppTypeApp n ts ppDN ppT = ppDN n <+> hsep (map ppT ts)

-------------------------------------------------------------------------------

-- * Contexts

-------------------------------------------------------------------------------

ppLContext :: Maybe (LHsContext DocNameI) -> Bool -> LaTeX
ppLContext Nothing _ = empty
ppLContext (Just ctxt) unicode = ppContext (unLoc ctxt) unicode

ppLContextNoArrow :: LHsContext DocNameI -> Bool -> LaTeX
ppLContextNoArrow ctxt unicode = ppContextNoArrow (unLoc ctxt) unicode

ppContextNoLocsMaybe :: [HsType DocNameI] -> Bool -> Maybe LaTeX
ppContextNoLocsMaybe [] _ = Nothing
ppContextNoLocsMaybe cxt unicode = Just $ pp_hs_context cxt unicode

ppContextNoArrow :: HsContext DocNameI -> Bool -> LaTeX
ppContextNoArrow cxt unicode =
  Maybe.fromMaybe empty $
    ppContextNoLocsMaybe (map unLoc cxt) unicode

ppContextNoLocs :: [HsType DocNameI] -> Bool -> LaTeX
ppContextNoLocs cxt unicode =
  maybe empty (<+> darrow unicode) $
    ppContextNoLocsMaybe cxt unicode

ppContext :: HsContext DocNameI -> Bool -> LaTeX
ppContext cxt unicode = ppContextNoLocs (map unLoc cxt) unicode

pp_hs_context :: [HsType DocNameI] -> Bool -> LaTeX
pp_hs_context [] _ = empty
pp_hs_context [p] unicode = ppCtxType unicode p
pp_hs_context cxt unicode = parenList (map (ppType unicode) cxt)

-------------------------------------------------------------------------------

-- * Types and contexts

-------------------------------------------------------------------------------

ppBang :: HsBang -> LaTeX
ppBang (HsBang _ SrcStrict) = char '!'
ppBang (HsBang _ SrcLazy) = char '~'
ppBang _ = empty

tupleParens :: HsTupleSort -> [LaTeX] -> LaTeX
tupleParens HsUnboxedTuple = ubxParenList
tupleParens _ = parenList

sumParens :: [LaTeX] -> LaTeX
sumParens = ubxparens . hsep . punctuate (text " |")

-------------------------------------------------------------------------------

-- * Rendering of HsType

--
-- Stolen from Html and tweaked for LaTeX generation
-------------------------------------------------------------------------------

ppLType, ppLParendType, ppLFunLhType :: Bool -> LHsType DocNameI -> LaTeX
ppLType unicode y = ppType unicode (unLoc y)
ppLParendType unicode y = ppParendType unicode (unLoc y)
ppLFunLhType unicode y = ppFunLhType unicode (unLoc y)

ppLSigType :: Bool -> LHsSigType DocNameI -> LaTeX
ppLSigType unicode y = ppSigType unicode (unLoc y)

ppType, ppParendType, ppFunLhType, ppCtxType :: Bool -> HsType DocNameI -> LaTeX
ppType unicode ty = ppr_mono_ty (reparenTypePrec PREC_TOP ty) unicode
ppParendType unicode ty = ppr_mono_ty (reparenTypePrec PREC_TOP ty) unicode
ppFunLhType unicode ty = ppr_mono_ty (reparenTypePrec PREC_FUN ty) unicode
ppCtxType unicode ty = ppr_mono_ty (reparenTypePrec PREC_CTX ty) unicode

ppSigType :: Bool -> HsSigType DocNameI -> LaTeX
ppSigType unicode sig_ty = ppr_sig_ty (reparenSigType sig_ty) unicode

ppLHsTypeArg :: Bool -> LHsTypeArg DocNameI -> LaTeX
ppLHsTypeArg unicode (HsValArg _ ty) = ppLParendType unicode ty
ppLHsTypeArg unicode (HsTypeArg _ ki) = atSign <> ppLParendType unicode ki
ppLHsTypeArg _ (HsArgPar _) = text ""

class RenderableBndrFlag flag where
  ppHsTyVarBndr :: Bool -> HsTyVarBndr flag DocNameI -> LaTeX

instance RenderableBndrFlag () where
  ppHsTyVarBndr unicode (HsTvb _ _ bvar bkind) =
    decorate (pp_hs_tvb unicode bvar bkind)
    where decorate :: LaTeX -> LaTeX
          decorate d = parens_if_kind bkind d

instance RenderableBndrFlag Specificity where
  ppHsTyVarBndr unicode (HsTvb _ spec bvar bkind) =
    decorate (pp_hs_tvb unicode bvar bkind)
    where decorate :: LaTeX -> LaTeX
          decorate d = case spec of
            InferredSpec  -> braces d
            SpecifiedSpec -> parens_if_kind bkind d

instance RenderableBndrFlag (HsBndrVis DocNameI) where
  ppHsTyVarBndr unicode (HsTvb _ bvis bvar bkind) =
    decorate (pp_hs_tvb unicode bvar bkind)
    where decorate :: LaTeX -> LaTeX
          decorate d = case bvis of
            HsBndrRequired  _ -> parens_if_kind bkind d
            HsBndrInvisible _ -> atSign <> parens_if_kind bkind d

ppHsBndrVar :: HsBndrVar DocNameI -> LaTeX
ppHsBndrVar (HsBndrVar _ name) = ppDocName (unLoc name)
ppHsBndrVar (HsBndrWildCard _) = char '_'

pp_hs_tvb :: Bool -> HsBndrVar DocNameI -> HsBndrKind DocNameI -> LaTeX
pp_hs_tvb _       bvar (HsBndrNoKind _) = ppHsBndrVar bvar
pp_hs_tvb unicode bvar (HsBndrKind _ k) =
  ppHsBndrVar bvar <+> dcolon unicode
                   <+> ppLKind unicode k

parens_if_kind :: HsBndrKind DocNameI -> LaTeX -> LaTeX
parens_if_kind (HsBndrNoKind _) d = d
parens_if_kind (HsBndrKind _ _) d = parens d

ppLKind :: Bool -> LHsKind DocNameI -> LaTeX
ppLKind unicode y = ppKind unicode (unLoc y)

ppKind :: Bool -> HsKind DocNameI -> LaTeX
ppKind unicode ki = ppr_mono_ty (reparenTypePrec PREC_TOP ki) unicode

-- Drop top-level for-all type variables in user style
-- since they are implicit in Haskell

ppr_sig_ty :: HsSigType DocNameI -> Bool -> LaTeX
ppr_sig_ty (HsSig{sig_bndrs = outer_bndrs, sig_body = ltype}) unicode =
  sep
    [ ppHsOuterTyVarBndrs outer_bndrs unicode
    , ppr_mono_lty ltype unicode
    ]

ppr_mono_lty :: LHsType DocNameI -> Bool -> LaTeX
ppr_mono_lty ty unicode = ppr_mono_ty (unLoc ty) unicode

ppr_mono_ty :: HsType DocNameI -> Bool -> LaTeX
ppr_mono_ty (HsForAllTy _ tele ty) unicode =
  sep
    [ ppHsForAllTelescope tele unicode
    , ppr_mono_lty ty unicode
    ]
ppr_mono_ty (HsQualTy _ ctxt ty) unicode =
  sep
    [ ppLContext (Just ctxt) unicode
    , ppr_mono_lty ty unicode
    ]
ppr_mono_ty (HsFunTy _ mult ty1 ty2) u =
  sep
    [ ppr_mono_lty ty1 u
    , arr <+> ppr_mono_lty ty2 u
    ]
  where
    -- MODS_TODO: need to ppr modifiers
    arr = case mult of
      HsStandardArrow _ _ -> arrow u
      HsLinearArrow _ _ -> lollipop u
ppr_mono_ty (HsBangTy _ b ty) u = ppBang b <> ppLParendType u ty
ppr_mono_ty (HsTyVar _ NotPromoted (L _ name)) _ = ppDocName name
ppr_mono_ty (HsTyVar _ IsPromoted (L _ name)) _ = char '\'' <> ppDocName name
ppr_mono_ty (HsTupleTy _ con tys) u = tupleParens con (map (ppLType u) tys)
ppr_mono_ty (HsSumTy _ tys) u = sumParens (map (ppLType u) tys)
ppr_mono_ty (HsKindSig _ ty kind) u = parens (ppr_mono_lty ty u <+> dcolon u <+> ppLKind u kind)
ppr_mono_ty (HsListTy _ ty) u = brackets (ppr_mono_lty ty u)
ppr_mono_ty (HsIParamTy _ (L _ n) ty) u = ppIPName n <+> dcolon u <+> ppr_mono_lty ty u
ppr_mono_ty (HsSpliceTy v _) _ = dataConCantHappen v
ppr_mono_ty (HsRecTy{}) _ = text "{..}"
ppr_mono_ty (XHsType{}) _ = error "ppr_mono_ty HsCoreTy"
ppr_mono_ty (HsExplicitListTy _ IsPromoted tys) u = Pretty.quote $ brackets $ hsep $ punctuate comma $ map (ppLType u) tys
ppr_mono_ty (HsExplicitListTy _ NotPromoted tys) u = brackets $ hsep $ punctuate comma $ map (ppLType u) tys
ppr_mono_ty (HsExplicitTupleTy _ tys) u = Pretty.quote $ parenList $ map (ppLType u) tys
ppr_mono_ty (HsAppTy _ fun_ty arg_ty) unicode =
  hsep [ppr_mono_lty fun_ty unicode, ppr_mono_lty arg_ty unicode]
ppr_mono_ty (HsAppKindTy _ fun_ty arg_ki) unicode =
  hsep [ppr_mono_lty fun_ty unicode, atSign <> ppr_mono_lty arg_ki unicode]
ppr_mono_ty (HsOpTy _ prom ty1 op ty2) unicode =
  ppr_mono_lty ty1 unicode <+> ppr_op_prom <+> ppr_mono_lty ty2 unicode
  where
    ppr_op_prom
      | isPromoted prom =
          char '\'' <> ppr_op
      | otherwise =
          ppr_op
    ppr_op
      | isSymOcc (getOccName op) = ppLDocName op
      | otherwise = char '`' <> ppLDocName op <> char '`'
ppr_mono_ty (HsParTy _ ty) unicode =
  parens (ppr_mono_lty ty unicode)
--  = ppr_mono_lty ty unicode

ppr_mono_ty (HsDocTy _ ty _) unicode =
  ppr_mono_lty ty unicode
ppr_mono_ty (HsWildCardTy _) _ = char '_'
ppr_mono_ty (HsTyLit _ t) u = ppr_tylit t u
ppr_mono_ty (HsStarTy _ isUni) unicode = starSymbol (isUni || unicode)
ppr_mono_ty (HsModifiedTy _ _ ty) unicode = ppr_mono_lty ty unicode -- MODS_TODO need to pprint modifiers

ppr_tylit :: HsTyLit DocNameI -> Bool -> LaTeX
ppr_tylit (HsNumTy _ n) _ = integer n
ppr_tylit (HsStrTy _ s) _ = text (show s)
ppr_tylit (HsCharTy _ c) _ = text (show c)

-- XXX: Ok in verbatim, but not otherwise
-- XXX: Do something with Unicode parameter?

-------------------------------------------------------------------------------

-- * Names

-------------------------------------------------------------------------------

ppBinder :: OccName -> LaTeX
ppBinder n
  | isSymOcc n = parens $ ppOccName n
  | otherwise = ppOccName n

ppBinderInfix :: OccName -> LaTeX
ppBinderInfix n
  | isSymOcc n = ppOccName n
  | otherwise = cat [char '`', ppOccName n, char '`']

ppSymName :: Name -> LaTeX
ppSymName name
  | isNameSym name = parens $ ppName name
  | otherwise = ppName name

ppWcSymName :: Maybe Name -> LaTeX
ppWcSymName = maybe (char '_') ppSymName

ppIPName :: HsIPName -> LaTeX
ppIPName = text . ('?' :) . unpackFS . hsIPNameFS

ppOccName :: OccName -> LaTeX
ppOccName = text . occNameString

ppDocName :: DocName -> LaTeX
ppDocName = ppOccName . nameOccName . getName

ppLDocName :: GenLocated l DocName -> LaTeX
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
latexMunge '#' s = "{\\char '43}" ++ s
latexMunge '$' s = "{\\char '44}" ++ s
latexMunge '%' s = "{\\char '45}" ++ s
latexMunge '&' s = "{\\char '46}" ++ s
latexMunge '~' s = "{\\char '176}" ++ s
latexMunge '_' s = "{\\char '137}" ++ s
latexMunge '^' s = "{\\char '136}" ++ s
latexMunge '\\' s = "{\\char '134}" ++ s
latexMunge '{' s = "{\\char '173}" ++ s
latexMunge '}' s = "{\\char '175}" ++ s
latexMunge '[' s = "{\\char 91}" ++ s
latexMunge ']' s = "{\\char 93}" ++ s
latexMunge c s = c : s

latexMonoMunge :: Char -> String -> String
latexMonoMunge ' ' (' ' : s) = "\\ \\ " ++ s
latexMonoMunge ' ' ('\\' : ' ' : s) = "\\ \\ " ++ s
latexMonoMunge '\n' s = '\\' : '\\' : s
latexMonoMunge c s = latexMunge c s

-------------------------------------------------------------------------------

-- * Doc Markup

-------------------------------------------------------------------------------

latexMarkup :: HasOccName a => DocMarkup (Wrap a) (StringContext -> LaTeX -> LaTeX)
latexMarkup =
  Markup
    { markupParagraph = \p v -> blockElem (p v (text "\\par"))
    , markupEmpty = \_ -> id
    , markupString = \s v -> inlineElem (text (fixString v s))
    , markupAppend = \l r v -> l v . r v
    , markupIdentifier = \i v -> inlineElem (markupId v (fmap occName i))
    , markupIdentifierUnchecked = \i v -> inlineElem (markupId v (fmap snd i))
    , markupModule =
        \(ModLink m mLabel) v ->
          case mLabel of
            Just lbl -> inlineElem . tt $ lbl v empty
            Nothing ->
              inlineElem
                ( let (mdl, _ref) = break (== '#') m
                   in (tt (text mdl))
                )
    , markupWarning = \p v -> p v
    , markupEmphasis = \p v -> inlineElem (emph (p v empty))
    , markupBold = \p v -> inlineElem (bold (p v empty))
    , markupMonospaced = \p v -> inlineElem (markupMonospace p v)
    , markupUnorderedList = \p v -> blockElem (itemizedList (map (\p' -> p' v empty) p))
    , markupPic = \p _ -> inlineElem (markupPic p)
    , markupMathInline = \p _ -> inlineElem (markupMathInline p)
    , markupMathDisplay = \p _ -> blockElem (markupMathDisplay p)
    , markupOrderedList = \p v -> blockElem (enumeratedList (map (\(_, p') -> p' v empty) p))
    , markupDefList = \l v -> blockElem (descriptionList (map (\(a, b) -> (a v empty, b v empty)) l))
    , markupCodeBlock = \p _ -> blockElem (quote (verb (p Verb empty)))
    , markupHyperlink = \(Hyperlink u l) v -> inlineElem (markupLink u (fmap (\x -> x v empty) l))
    , markupAName = \_ _ -> id -- TODO
    , markupProperty = \p _ -> blockElem (quote (verb (text p)))
    , markupExample = \e _ -> blockElem (quote (verb (text $ unlines $ map exampleToString e)))
    , markupHeader = \(Header l h) p -> blockElem (header l (h p empty))
    , markupTable = \(Table h b) p -> blockElem (table h b p)
    }
  where
    blockElem :: LaTeX -> LaTeX -> LaTeX
    blockElem = ($$)

    inlineElem :: LaTeX -> LaTeX -> LaTeX
    inlineElem = (<>)

    header 1 d = text "\\section*" <> braces d
    header 2 d = text "\\subsection*" <> braces d
    header l d
      | l > 0 && l <= 6 = text "\\subsubsection*" <> braces d
    header l _ = error $ "impossible header level in LaTeX generation: " ++ show l

    table _ _ _ = text "{TODO: Table}"

    fixString Plain s = latexFilter s
    fixString Verb s = s
    fixString Mono s = latexMonoFilter s

    markupMonospace p Verb = p Verb empty
    markupMonospace p _ = tt (p Mono empty)

    markupLink url mLabel = case mLabel of
      Just label -> text "\\href" <> braces (text url) <> braces label
      Nothing -> text "\\url" <> braces (text url)

    -- Is there a better way of doing this? Just a space is an arbitrary choice.
    markupPic (Picture uri title) = parens (imageText title)
      where
        imageText Nothing = beg
        imageText (Just t) = beg <> text " " <> text t

        beg = text "image: " <> text uri

    markupMathInline mathjax = text "\\(" <> text mathjax <> text "\\)"

    markupMathDisplay mathjax = text "\\[" <> text mathjax <> text "\\]"

    markupId v wrappedOcc =
      case v of
        Verb -> text i
        Mono -> text "\\haddockid" <> braces (text . latexMonoFilter $ i)
        Plain -> text "\\haddockid" <> braces (text . latexFilter $ i)
      where
        i = showWrapped occNameString wrappedOcc

docToLaTeX :: Doc DocName -> LaTeX
docToLaTeX doc = markup latexMarkup doc Plain empty

documentationToLaTeX :: Documentation DocName -> Maybe LaTeX
documentationToLaTeX = fmap docToLaTeX . fmap _doc . combineDocumentation

rdrDocToLaTeX :: Doc RdrName -> LaTeX
rdrDocToLaTeX doc = markup latexMarkup doc Plain empty

data StringContext
  = -- | all special characters have to be escape
    Plain
  | -- | on top of special characters, escape space characters
    Mono
  | -- | don't escape anything
    Verb

latexStripTrailingWhitespace :: Doc a -> Doc a
latexStripTrailingWhitespace (DocString s)
  | null s' = DocEmpty
  | otherwise = DocString s
  where
    s' = reverse (dropWhile isSpace (reverse s))
latexStripTrailingWhitespace (DocAppend l r)
  | DocEmpty <- r' = latexStripTrailingWhitespace l
  | otherwise = DocAppend l r'
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
  text "\\vbox{\\begin{itemize}"
    $$ vcat (map (text "\\item" $$) items)
    $$ text "\\end{itemize}}"

enumeratedList :: [LaTeX] -> LaTeX
enumeratedList items =
  text "\\vbox{\\begin{enumerate}"
    $$ vcat (map (text "\\item " $$) items)
    $$ text "\\end{enumerate}}"

descriptionList :: [(LaTeX, LaTeX)] -> LaTeX
descriptionList items =
  text "\\vbox{\\begin{description}"
    $$ vcat (map (\(a, b) -> text "\\item" <> brackets a <> text "\\hfill \\par" $$ b) items)
    $$ text "\\end{description}}"

tt :: LaTeX -> LaTeX
tt ltx = text "\\haddocktt" <> braces ltx

decltt :: LaTeX -> LaTeX
decltt ltx = text "\\haddockdecltt" <> braces (text filtered)
  where
    filtered = latexMonoFilter (latex2String ltx)

emph :: LaTeX -> LaTeX
emph ltx = text "\\emph" <> braces ltx

bold :: LaTeX -> LaTeX
bold ltx = text "\\textbf" <> braces ltx

-- TODO: @verbatim@ is too much since
--
--   * Haddock supports markup _inside_ of code blocks. Right now, the LaTeX
--     representing that markup gets printed verbatim
--   * Verbatim environments are not supported everywhere (example: not nested
--     inside a @tabulary@ environment)
verb :: LaTeX -> LaTeX
verb doc = text "{\\haddockverb\\begin{verbatim}" $$ doc <> text "\\end{verbatim}}"

-- NB. swallow a trailing \n in the verbatim text by appending the
-- \end{verbatim} directly, otherwise we get spurious blank lines at the
-- end of code blocks.

quote :: LaTeX -> LaTeX
quote doc = text "\\begin{quote}" $$ doc $$ text "\\end{quote}"

dcolon, arrow, lollipop, darrow, forallSymbol, starSymbol :: Bool -> LaTeX
dcolon unicode = text (if unicode then "∷" else "::")
arrow unicode = text (if unicode then "→" else "->")
lollipop unicode = text (if unicode then "⊸" else "%1 ->")
darrow unicode = text (if unicode then "⇒" else "=>")
forallSymbol unicode = text (if unicode then "∀" else "forall")
starSymbol unicode = text (if unicode then "★" else "*")

atSign :: LaTeX
atSign = char '@'

-- multAnnotation :: LaTeX
-- multAnnotation = char '%'

dot :: LaTeX
dot = char '.'

parenList :: [LaTeX] -> LaTeX
parenList = parens . hsep . punctuate comma

ubxParenList :: [LaTeX] -> LaTeX
ubxParenList = ubxparens . hsep . punctuate comma

ubxparens :: LaTeX -> LaTeX
ubxparens h = text "(#" <+> h <+> text "#)"

nl :: LaTeX
nl = text "\\\\"

keyword :: String -> LaTeX
keyword = text

infixr 4 <-> -- combining table cells
(<->) :: LaTeX -> LaTeX -> LaTeX
a <-> b = a <+> char '&' <+> b
