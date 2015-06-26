module Haddock.Backends.Hyperlinker (ppHyperlinkedSource) where

import Haddock.Types
import Haddock.Backends.Xhtml.Types
import Haddock.Backends.Xhtml.Utils
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Utils

import Text.XHtml hiding ((</>))

import Data.Maybe
import System.Directory
import System.FilePath

ppHyperlinkedSource :: FilePath -> FilePath
                    -> Maybe FilePath
                    -> SourceURLs
                    -> [Interface]
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle urls ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    mapM_ (ppHyperlinkedModuleSource outdir urls) ifaces
  where
    srcdir = srcPath outdir urls

ppHyperlinkedModuleSource :: FilePath -> SourceURLs -> Interface -> IO ()
ppHyperlinkedModuleSource outdir urls iface = case ifaceTokenizedSrc iface of
    Just tokens ->
        writeFile path $ showHtml . render mCssFile mJsFile urls $ tokens
    Nothing -> return ()
  where
    mCssFile = Just $ srcCssFile
    mJsFile = Just $ highlightScript
    srcFile = spliceURL Nothing (Just $ ifaceMod iface) Nothing Nothing $
        srcModUrl urls
    path = outdir </> srcFile

srcPath :: FilePath -> SourceURLs -> FilePath
srcPath outdir urls = outdir </> takeDirectory (srcModUrl urls)

srcCssFile :: FilePath
srcCssFile = "srcstyle.css"

highlightScript :: FilePath
highlightScript = "highlight.js"

defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
