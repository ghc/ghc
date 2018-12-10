module Rules.Documentation (
    -- * Rules
    buildPackageDocumentation, documentationRules,

    -- * Utilities
    haddockDependencies
    ) where

import Hadrian.BuildPath
import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type

import Rules.Generate (ghcPrimDependencies)
import Base
import Context
import Expression (getContextData, interpretInContext, (?), package)
import Flavour
import Oracles.ModuleFiles
import Packages
import Settings
import Target
import Utilities

import Data.List (union)
import qualified Text.Parsec as Parsec

docRoot :: FilePath
docRoot = "docs"

htmlRoot :: FilePath
htmlRoot = docRoot -/- "html"

pdfRoot :: FilePath
pdfRoot = docRoot -/- "pdfs"

archiveRoot :: FilePath
archiveRoot = docRoot -/- "archives"

haddockHtmlLib :: FilePath
haddockHtmlLib = htmlRoot -/- "haddock-bundle.min.js"

manPageBuildPath :: FilePath
manPageBuildPath = "docs/users_guide/build-man/ghc.1"

-- TODO: Get rid of this hack.
docContext :: Context
docContext = vanillaContext Stage2 (library "Documentation" "docs")

docPaths :: [FilePath]
docPaths = ["libraries", "users_guide", "Haddock"]

pathPdf :: FilePath -> FilePath
pathPdf path = pdfRoot -/- path <.> ".pdf"

pathIndex :: FilePath -> FilePath
pathIndex path = htmlRoot -/- path -/- "index.html"

pathArchive :: FilePath -> FilePath
pathArchive path = archiveRoot -/- path <.> "html.tar.xz"

-- TODO: Get rid of this hack.
pathPath :: FilePath -> FilePath
pathPath "users_guide" = "docs/users_guide"
pathPath "Haddock" = "utils/haddock/doc"
pathPath _ = ""

-- | Build all documentation
documentationRules :: Rules ()
documentationRules = do
    buildDocumentationArchives
    buildHtmlDocumentation
    buildManPage
    buildPdfDocumentation

    -- a phony rule that runs Haddock for "Haskell Hierarchical Libraries" and
    -- the "GHC-API"
    "docs-haddock" ~> do
        root <- buildRoot
        need [ root -/- pathIndex "libraries" ]

    -- a phony rule that runs Haddock, builds the User's guide, builds
    -- Haddock's manual, and builds man pages
    "docs" ~> do
        root <- buildRoot
        let html     = htmlRoot -/- "index.html" -- also implies "docs-haddock"
            archives = map pathArchive docPaths
            pdfs     = map pathPdf $ docPaths \\ ["libraries"]
        need $ map (root -/-) $ [html] ++ archives ++ pdfs ++ [manPageBuildPath]

------------------------------------- HTML -------------------------------------

-- | Build rules for HTML documentation.
buildHtmlDocumentation :: Rules ()
buildHtmlDocumentation = do
    mapM_ buildSphinxHtml $ docPaths \\ ["libraries"]
    buildLibraryDocumentation
    root <- buildRootRules

    root -/- htmlRoot -/- "index.html" %> \file -> do
        need [root -/- haddockHtmlLib]
        need $ map ((root -/-) . pathIndex) docPaths
        copyFileUntracked "docs/index.html" file

-- | Compile a Sphinx ReStructured Text package to HTML.
buildSphinxHtml :: FilePath -> Rules ()
buildSphinxHtml path = do
    root <- buildRootRules
    root -/- htmlRoot -/- path -/- "index.html" %> \file -> do
        need [root -/- haddockHtmlLib]
        let dest = takeDirectory file
        build $ target docContext (Sphinx Html) [pathPath path] [dest]

------------------------------------ Haddock -----------------------------------

-- | Build the haddocks for GHC's libraries.
buildLibraryDocumentation :: Rules ()
buildLibraryDocumentation = do
    root <- buildRootRules

    -- Js and Css files for haddock output
    root -/- haddockHtmlLib %> \_ ->
        copyDirectory "utils/haddock/haddock-api/resources/html" (root -/- docRoot)

    -- Building the "Haskell Hierarchical Libraries" index
    root -/- htmlRoot -/- "libraries/index.html" %> \file -> do
        need [ root -/- haddockHtmlLib
             , "libraries/prologue.txt" ]

        -- We want Haddocks for everything except `rts` to be built, but we
        -- don't want the index to be polluted by stuff from `ghc`-the-library
        -- (there will be a seperate top-level link to those Haddocks).
        haddocks <- allHaddocks
        let neededDocs = filter (\x -> takeFileName x /= "rts.haddock") haddocks
            libDocs = filter (\x -> takeFileName x /= "ghc.haddock") neededDocs

        need neededDocs
        build $ target docContext (Haddock BuildIndex) libDocs [file]

allHaddocks :: Action [FilePath]
allHaddocks = do
    pkgs <- stagePackages Stage1
    sequence [ pkgHaddockFile $ vanillaContext Stage1 pkg
             | pkg <- pkgs, isLibrary pkg, pkgName pkg /= "rts" ]

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'docRoot' subdirectory. Pedantically tracking all
-- built files in the Shake database seems fragile and unnecessary.
buildPackageDocumentation :: Rules ()
buildPackageDocumentation = do
    root <- buildRootRules

    -- Per-package haddocks
    root -/- htmlRoot -/- "libraries/*/haddock-prologue.txt" %> \file -> do
        ctx <- getPkgDocTarget root file >>= pkgDocContext
        need [root -/- haddockHtmlLib]
        -- This is how @ghc-cabal@ used to produces "haddock-prologue.txt" files.
        syn  <- pkgSynopsis    (Context.package ctx)
        desc <- pkgDescription (Context.package ctx)
        let prologue = if null desc then syn else desc
        liftIO $ writeFile file prologue

    root -/- htmlRoot -/- "libraries/*/*.haddock" %> \file -> do
        context <- getPkgDocTarget root file >>= pkgDocContext
        need [ takeDirectory file  -/- "haddock-prologue.txt"]
        haddocks <- haddockDependencies context

        -- `ghc-prim` has a source file for 'GHC.Prim' which is generated just
        -- for Haddock. We need to 'union' (instead of '++') to avoid passing
        -- 'GHC.PrimopWrappers' (which unfortunately shows up in both
        -- `generatedSrcs` and `vanillaSrcs`) to Haddock twice.
        generatedSrcs <- interpretInContext context (Expression.package ghcPrim ? ghcPrimDependencies)
        vanillaSrcs <- hsSources context
        let srcs = vanillaSrcs `union` generatedSrcs

        need $ srcs ++ haddocks ++ [root -/- haddockHtmlLib]

        -- Build Haddock documentation
        -- TODO: Pass the correct way from Rules via Context.
        dynamicPrograms <- dynamicGhcPrograms =<< flavour
        let haddockWay = if dynamicPrograms then dynamic else vanilla
        build $ target (context {way = haddockWay}) (Haddock BuildPackage) srcs [file]

data PkgDocTarget = DotHaddock PackageName | HaddockPrologue PackageName
  deriving (Eq, Show)

pkgDocContext :: PkgDocTarget -> Action Context
pkgDocContext target = case findPackageByName pkgname of
  Nothing -> error $ "pkgDocContext: couldn't find package " ++ pkgname
  Just p  -> return (Context Stage1 p vanilla)

  where pkgname = case target of
          DotHaddock n      -> n
          HaddockPrologue n -> n

parsePkgDocTarget :: FilePath -> Parsec.Parsec String () PkgDocTarget
parsePkgDocTarget root = do
  _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
  _ <- Parsec.string (htmlRoot ++ "/")
  _ <- Parsec.string "libraries/"
  pkgname <- Parsec.manyTill Parsec.anyChar (Parsec.char '/')
  Parsec.choice
    [ Parsec.try (Parsec.string "haddock-prologue.txt")
        *> pure (HaddockPrologue pkgname)
    , Parsec.string (pkgname <.> "haddock")
        *> pure (DotHaddock pkgname)
    ]

getPkgDocTarget :: FilePath -> FilePath -> Action PkgDocTarget
getPkgDocTarget root path =
  parsePath (parsePkgDocTarget root) "<doc target>" path

-------------------------------------- PDF -------------------------------------

-- | Build all PDF documentation
buildPdfDocumentation :: Rules ()
buildPdfDocumentation = mapM_ buildSphinxPdf docPaths

-- | Compile a Sphinx ReStructured Text package to LaTeX
buildSphinxPdf :: FilePath -> Rules ()
buildSphinxPdf path = do
    root <- buildRootRules
    root -/- pdfRoot -/- path <.> "pdf" %> \file -> do
        need [root -/- haddockHtmlLib]
        withTempDir $ \dir -> do
            build $ target docContext (Sphinx Latex) [pathPath path] [dir]
            build $ target docContext Xelatex [path <.> "tex"] [dir]
            copyFileUntracked (dir -/- path <.> "pdf") file

------------------------------------ Archive -----------------------------------

-- | Build documentation archives.
buildDocumentationArchives :: Rules ()
buildDocumentationArchives = mapM_ buildArchive docPaths

buildArchive :: FilePath -> Rules ()
buildArchive path = do
    root <- buildRootRules
    root -/- pathArchive path %> \file -> do
        need [root -/- haddockHtmlLib]
        root <- buildRoot
        let src = root -/- pathIndex path
        need [src]
        build $ target docContext (Tar Create) [takeDirectory src] [file]

-- | Build the man page.
buildManPage :: Rules ()
buildManPage = do
    root <- buildRootRules
    root -/- manPageBuildPath %> \file -> do
        need [root -/- haddockHtmlLib, "docs/users_guide/ghc.rst"]
        withTempDir $ \dir -> do
            build $ target docContext (Sphinx Man) ["docs/users_guide"] [dir]
            copyFileUntracked (dir -/- "ghc.1") file

-- | Find the Haddock files for the dependencies of the current library.
haddockDependencies :: Context -> Action [FilePath]
haddockDependencies context = do
    depNames <- interpretInContext context (getContextData depNames)
    sequence [ pkgHaddockFile $ vanillaContext Stage1 depPkg
             | Just depPkg <- map findPackageByName depNames, depPkg /= rts ]
