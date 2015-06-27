module Haddock.Backends.Hyperlinker
    ( ppHyperlinkedSource
    , module Haddock.Backends.Hyperlinker.Utils
    ) where

import Haddock.Types
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Utils

import Text.XHtml hiding ((</>))

import Data.Maybe
import System.Directory
import System.FilePath

ppHyperlinkedSource :: FilePath -> FilePath
                    -> Maybe FilePath
                    -> [Interface]
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    mapM_ (ppHyperlinkedModuleSource srcdir) ifaces
  where
    srcdir = outdir </> hypSrcDir

ppHyperlinkedModuleSource :: FilePath -> Interface -> IO ()
ppHyperlinkedModuleSource srcdir iface = case ifaceTokenizedSrc iface of
    Just tokens ->
        writeFile path $ showHtml . render mCssFile mJsFile $ tokens
    Nothing -> return ()
  where
    mCssFile = Just $ srcCssFile
    mJsFile = Just $ highlightScript
    path = srcdir </> hypSrcModuleFile (ifaceMod iface)

srcCssFile :: FilePath
srcCssFile = "style.css"

highlightScript :: FilePath
highlightScript = "highlight.js"

defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
