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
import Oracles.Setting (topDirectory)
import Packages
import Settings
import Target
import Utilities

import qualified Data.Set    as Set
import qualified Text.Parsec as Parsec

docRoot :: FilePath
docRoot = "docs"

htmlRoot :: FilePath
htmlRoot = docRoot -/- "html"

pdfRoot :: FilePath
pdfRoot = docRoot -/- "pdfs"

infoRoot :: FilePath
infoRoot = docRoot -/- "info"

archiveRoot :: FilePath
archiveRoot = docRoot -/- "archives"

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
pathPath "GHCUsersGuide" = "docs/users_guide"
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
    buildSphinxInfoGuide

    -- a phony rule that runs Haddock for "Haskell Hierarchical Libraries" and
    -- the "GHC-API"
    "docs-haddock" ~> do
        root <- buildRoot
        need [ root -/- pathIndex "libraries" ]

    -- a phony rule that runs Haddock, builds the User's guide, builds
    -- Haddock's manual, and builds man pages
    "docs" ~> do
        root <- buildRoot

        -- we need to ensure that `configure` has been run (#17840)
        need [configFile]

        doctargets <- ghcDocs =<< flavour
        let html     = htmlRoot -/- "index.html" -- also implies "docs-haddock"
            archives = map pathArchive docPaths
            pdfs     = map pathPdf $ docPaths \\ ["libraries"]

            targets = -- include PDFs unless --docs=no-sphinx[-pdf] is
                      -- passed.
                      concat [ pdfs | SphinxPDFs `Set.member` doctargets ]

                      -- include manpage unless --docs=no-sphinx[-man] is given.
                   ++ [ manPageBuildPath | SphinxMan `Set.member` doctargets ]

                      -- include toplevel html target unless we neither want
                      -- haddocks nor html pages produced by sphinx.
                   ++ [ html |    Haddocks   `Set.member` doctargets
                               || SphinxHTML `Set.member` doctargets ]

                      -- include archives for whatever targets remain from
                      -- the --docs arguments we got.
                   ++ [ ar
                      | (ar, doc) <- zip archives docPaths
                      , archiveTarget doc `Set.member` doctargets ]

        need $ map (root -/-) targets

        when (SphinxHTML `Set.member` doctargets)
          $ checkUserGuideFlags $ root -/- htmlRoot -/- "users_guide" -/- "ghc-flags.txt"

    where archiveTarget "libraries"   = Haddocks
          archiveTarget _             = SphinxHTML

-- | Check Sphinx log for undefined reference target errors. Ideally we would
-- use sphinx's @-W@ flag here but unfortunately it also turns syntax
-- highlighting warnings into errors which is undesirable.
checkSphinxWarnings :: FilePath  -- ^ output directory
                    -> Action ()
checkSphinxWarnings out = do
    log <- liftIO $ readFile (out -/- ".log")
    when ("reference target not found" `isInfixOf` log)
      $ fail "Undefined reference targets found in Sphinx log."

-- | Check that all GHC flags are documented in the users guide.
checkUserGuideFlags :: FilePath -> Action ()
checkUserGuideFlags documentedFlagList = do
    scriptPath <- (</> "docs/users_guide/compare-flags.py") <$> topDirectory
    ghc <- programPath (vanillaContext Stage1 ghc)
    need [ghc]
    ghcPath <- (</>) <$> topDirectory <*> pure ghc
    runBuilder Python
      [ scriptPath
      , "--doc-flags", documentedFlagList
      , "--ghc", ghcPath
      ] [documentedFlagList] []


------------------------------------- HTML -------------------------------------

-- | Build rules for HTML documentation.
buildHtmlDocumentation :: Rules ()
buildHtmlDocumentation = do
    mapM_ buildSphinxHtml $ docPaths \\ ["libraries"]
    buildLibraryDocumentation
    root <- buildRootRules

    root -/- htmlRoot -/- "index.html" %> \file -> do
        doctargets <- ghcDocs =<< flavour

        -- We include the HTML output of haddock for libraries unless
        -- told not to (e.g with --docs=no-haddocks). Likewise for
        -- the HTML version of the users guide or the Haddock manual.
        let targets = [ "libraries" | Haddocks `Set.member` doctargets ]
                   ++ concat [ ["users_guide", "Haddock"]
                             | SphinxHTML `Set.member` doctargets ]
        need $ map ((root -/-) . pathIndex) targets

        copyFileUntracked "docs/index.html" file

-- | Compile a Sphinx ReStructured Text package to HTML.
buildSphinxHtml :: FilePath -> Rules ()
buildSphinxHtml path = do
    root <- buildRootRules
    root -/- htmlRoot -/- path -/- "index.html" %> \file -> do
        let dest = takeDirectory file
            rstFilesDir = pathPath path
        rstFiles <- getDirectoryFiles rstFilesDir ["**/*.rst"]
        need (map (rstFilesDir -/-) rstFiles)
        build $ target docContext (Sphinx HtmlMode) [pathPath path] [dest]
        checkSphinxWarnings dest

------------------------------------ Haddock -----------------------------------

-- | Build the haddocks for GHC's libraries.
buildLibraryDocumentation :: Rules ()
buildLibraryDocumentation = do
    root <- buildRootRules

    -- Building the "Haskell Hierarchical Libraries" index
    root -/- htmlRoot -/- "libraries/index.html" %> \file -> do
        need [ "libraries/prologue.txt" ]

        -- We want Haddocks for everything except `rts` to be built, but we
        -- don't want the index to be polluted by stuff from `ghc`-the-library
        -- (there will be a separate top-level link to those Haddocks).
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
        ctx <- pkgDocContext <$> getPkgDocTarget root file
        -- This is how @ghc-cabal@ used to produces "haddock-prologue.txt" files.
        syn  <- pkgSynopsis    (Context.package ctx)
        desc <- pkgDescription (Context.package ctx)
        let prologue = if null desc then syn else desc
        liftIO $ writeFile file prologue

    root -/- htmlRoot -/- "libraries/*/*.haddock" %> \file -> do
        context <- pkgDocContext <$> getPkgDocTarget root file
        need [ takeDirectory file  -/- "haddock-prologue.txt"]
        haddocks <- haddockDependencies context

        -- `ghc-prim` has a source file for 'GHC.Prim' which is generated just
        -- for Haddock. We need to 'union' (instead of '++') to avoid passing
        -- 'GHC.PrimopWrappers' (which unfortunately shows up in both
        -- `generatedSrcs` and `vanillaSrcs`) to Haddock twice.
        generatedSrcs <- interpretInContext context (Expression.package ghcPrim ? ghcPrimDependencies)
        vanillaSrcs <- hsSources context
        let srcs = vanillaSrcs `union` generatedSrcs

        need $ srcs ++ haddocks

        -- Build Haddock documentation
        -- TODO: Pass the correct way from Rules via Context.
        dynamicPrograms <- dynamicGhcPrograms =<< flavour
        let haddockWay = if dynamicPrograms then dynamic else vanilla
        statsFilesDir <- haddockStatsFilesDir
        createDirectory statsFilesDir
        build $ target (context {way = haddockWay}) (Haddock BuildPackage) srcs [file]
        produces [
          statsFilesDir </> pkgName (Context.package context) <.> "t"
          ]

data PkgDocTarget = DotHaddock PackageName | HaddockPrologue PackageName
  deriving (Eq, Show)

pkgDocContext :: PkgDocTarget -> Context
pkgDocContext target = Context Stage1 (unsafeFindPackageByName name) vanilla
  where
    name = case target of DotHaddock n      -> n
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
        withTempDir $ \dir -> do
            let rstFilesDir = pathPath path
            rstFiles <- getDirectoryFiles rstFilesDir ["**/*.rst"]
            need (map (rstFilesDir -/-) rstFiles)
            build $ target docContext (Sphinx LatexMode) [pathPath path] [dir]
            checkSphinxWarnings dir
            build $ target docContext Xelatex [path <.> "tex"] [dir]
            copyFileUntracked (dir -/- path <.> "pdf") file

------------------------------------ Info --------------------------------------

-- | Build the user guide as an Info hypertext
buildSphinxInfoGuide :: Rules ()
buildSphinxInfoGuide = do
  root <- buildRootRules
  let path = "GHCUsersGuide"
  root -/- infoRoot -/- path <.> "info" %> \ file -> do
        withTempDir $ \dir -> do
            let rstFilesDir = pathPath path
            rstFiles <- getDirectoryFiles rstFilesDir ["**/*.rst"]
            need (map (rstFilesDir -/-) rstFiles)
            build $ target docContext (Sphinx InfoMode) [pathPath path] [dir]
            checkSphinxWarnings dir
            -- Sphinx outputs texinfo source and a makefile, the
            -- default target of which actually produces the target
            -- for this build rule.
            let p = dir -/- path
            let [texipath, infopath] = map (p <.>) ["texi", "info"]
            build $ target docContext (Makeinfo) [texipath] [infopath]
            copyFileUntracked infopath file

------------------------------------ Archive -----------------------------------

-- | Build documentation archives.
buildDocumentationArchives :: Rules ()
buildDocumentationArchives = mapM_ buildArchive docPaths

buildArchive :: FilePath -> Rules ()
buildArchive path = do
    root <- buildRootRules
    root -/- pathArchive path %> \file -> do
        root <- buildRoot
        let src = root -/- pathIndex path
        need [src]
        build $ target docContext (Tar Create) [takeDirectory src] [file]

-- | Build the man page.
buildManPage :: Rules ()
buildManPage = do
    root <- buildRootRules
    root -/- manPageBuildPath %> \file -> do
        need ["docs/users_guide/ghc.rst"]
        withTempDir $ \dir -> do
            build $ target docContext (Sphinx ManMode) ["docs/users_guide"] [dir]
            checkSphinxWarnings dir
            copyFileUntracked (dir -/- "ghc.1") file

-- | Find the Haddock files for the dependencies of the current library.
haddockDependencies :: Context -> Action [FilePath]
haddockDependencies context = do
    depNames <- interpretInContext context (getContextData depNames)
    sequence [ pkgHaddockFile $ vanillaContext Stage1 depPkg
             | Just depPkg <- map findPackageByName depNames, depPkg /= rts ]
