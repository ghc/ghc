module Rules.Documentation (
    -- * Rules
    buildPackageDocumentation, documentationRules,

    -- * Utilities
    haddockDependencies
    ) where

import Hadrian.Haskell.Cabal
import Hadrian.Haskell.Cabal.Type

import Base
import Context
import Expression (getContextData, interpretInContext)
import Flavour
import Oracles.ModuleFiles
import Packages
import Settings
import Target
import Utilities

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

    "docs" ~> do
        root <- buildRoot
        let html     = htmlRoot -/- "index.html"
            archives = map pathArchive docPaths
            pdfs     = map pathPdf $ docPaths \\ ["libraries"]
        need $ map (root -/-) $ [html] ++ archives ++ pdfs
        need [ root -/- htmlRoot -/- "libraries" -/- "gen_contents_index"
             , root -/- htmlRoot -/- "libraries" -/- "prologue.txt"
             , root -/- manPageBuildPath ]

------------------------------------- HTML -------------------------------------

-- | Build rules for HTML documentation.
buildHtmlDocumentation :: Rules ()
buildHtmlDocumentation = do
    mapM_ buildSphinxHtml $ docPaths \\ ["libraries"]
    buildLibraryDocumentation
    root <- buildRootRules
    root -/- htmlRoot -/- "libraries/gen_contents_index" %>
        copyFile "libraries/gen_contents_index"

    root -/- htmlRoot -/- "libraries/prologue.txt" %>
        copyFile "libraries/prologue.txt"

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

    root -/- htmlRoot -/- "libraries/index.html" %> \file -> do
        need [root -/- haddockHtmlLib]
        haddocks <- allHaddocks
        let libDocs = filter
                (\x -> takeFileName x `notElem` ["ghc.haddock", "rts.haddock"])
                haddocks
        need (root -/- haddockHtmlLib : libDocs)
        build $ target docContext (Haddock BuildIndex) libDocs [file]

allHaddocks :: Action [FilePath]
allHaddocks = do
    pkgs <- stagePackages Stage1
    sequence [ pkgHaddockFile $ vanillaContext Stage1 pkg
             | pkg <- pkgs, isLibrary pkg ]

-- Note: this build rule creates plenty of files, not just the .haddock one.
-- All of them go into the 'docRoot' subdirectory. Pedantically tracking all
-- built files in the Shake database seems fragile and unnecessary.
buildPackageDocumentation :: Context -> Rules ()
buildPackageDocumentation context@Context {..} = when (stage == Stage1 && package /= rts) $ do
    root <- buildRootRules

    -- Per-package haddocks
    root -/- htmlRoot -/- "libraries" -/- pkgName package -/- "haddock-prologue.txt" %> \file -> do
        need [root -/- haddockHtmlLib]
        -- This is how @ghc-cabal@ used to produces "haddock-prologue.txt" files.
        syn  <- pkgSynopsis    package
        desc <- pkgDescription package
        let prologue = if null desc then syn else desc
        liftIO $ writeFile file prologue

    root -/- htmlRoot -/- "libraries" -/- pkgName package -/- pkgName package <.> "haddock" %> \file -> do
        need [root -/- htmlRoot -/- "libraries" -/- pkgName package -/- "haddock-prologue.txt"]
        haddocks <- haddockDependencies context
        srcs <- hsSources context
        need $ srcs ++ haddocks ++ [root -/- haddockHtmlLib]

        -- Build Haddock documentation
        -- TODO: Pass the correct way from Rules via Context.
        dynamicPrograms <- dynamicGhcPrograms <$> flavour
        let haddockWay = if dynamicPrograms then dynamic else vanilla
        build $ target (context {way = haddockWay}) (Haddock BuildPackage) srcs [file]

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
