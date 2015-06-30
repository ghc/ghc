module Haddock.Backends.Hyperlinker
    ( ppHyperlinkedSource
    , module Haddock.Backends.Hyperlinker.Utils
    ) where

import Haddock.Types
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Utils

import Text.XHtml hiding ((</>))
import GHC

import Data.Maybe
import System.Directory
import System.FilePath


-- | Generate hyperlinked source for given interfaces.
--
-- Note that list of interfaces should also contain interfaces normally hidden
-- when generating documentation. Otherwise this could lead to dead links in
-- produced source.
ppHyperlinkedSource :: FilePath -- ^ Output directory
                    -> FilePath -- ^ Resource directory
                    -> Maybe FilePath -- ^ Custom CSS file path
                    -> Bool -- ^ Flag indicating whether to pretty-print HTML
                    -> PackageKey -- ^ Package for which we create source
                    -> SrcMap -- ^ Paths to external sources
                    -> [Interface] -- ^ Interfaces for which we create source
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle pretty pkg srcs ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    mapM_ (ppHyperlinkedModuleSource srcdir pretty pkg srcs) ifaces
  where
    srcdir = outdir </> hypSrcDir

-- | Generate hyperlinked source for particular interface.
ppHyperlinkedModuleSource :: FilePath -> Bool
                          -> PackageKey -> SrcMap -> Interface
                          -> IO ()
ppHyperlinkedModuleSource srcdir pretty pkg srcs iface =
    case ifaceTokenizedSrc iface of
        Just tokens -> writeFile path . html . render' $ tokens
        Nothing -> return ()
  where
    render' = render (Just srcCssFile) (Just highlightScript) pkg srcs
    html = if pretty then renderHtml else showHtml
    path = srcdir </> hypSrcModuleFile (ifaceMod iface)

-- | Name of CSS file in output directory.
srcCssFile :: FilePath
srcCssFile = "style.css"

-- | Name of highlight script in output and resource directory.
highlightScript :: FilePath
highlightScript = "highlight.js"

-- | Path to default CSS file.
defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
