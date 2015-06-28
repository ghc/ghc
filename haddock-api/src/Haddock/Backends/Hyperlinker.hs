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

ppHyperlinkedSource :: FilePath -> FilePath -> Maybe FilePath
                    -> PackageKey -> SrcMap -> [Interface]
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle pkg srcs ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    mapM_ (ppHyperlinkedModuleSource srcdir pkg srcs) ifaces
  where
    srcdir = outdir </> hypSrcDir

ppHyperlinkedModuleSource :: FilePath
                          -> PackageKey -> SrcMap -> Interface
                          -> IO ()
ppHyperlinkedModuleSource srcdir pkg srcs iface =
    case ifaceTokenizedSrc iface of
        Just tokens -> writeFile path . showHtml . render' $ tokens
        Nothing -> return ()
  where
    render' = render (Just srcCssFile) (Just highlightScript) pkg srcs
    path = srcdir </> hypSrcModuleFile (ifaceMod iface)

srcCssFile :: FilePath
srcCssFile = "style.css"

highlightScript :: FilePath
highlightScript = "highlight.js"

defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
