module Haddock.Backends.Hyperlinker (ppHyperlinkedSource) where

import Haddock.Types
import Haddock.Backends.Hyperlinker.Renderer

import GHC
import Text.XHtml hiding ((</>))

import Data.Maybe
import System.Directory
import System.FilePath

ppHyperlinkedSource :: FilePath -> FilePath -> Maybe FilePath -> [Interface]
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle ifaces = do
    createDirectoryIfMissing True $ srcPath outdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcPath outdir </> srcCssFile
    mapM_ (ppHyperlinkedModuleSource outdir) ifaces

ppHyperlinkedModuleSource :: FilePath -> Interface -> IO ()
ppHyperlinkedModuleSource outdir iface = case ifaceTokenizedSrc iface of
    Just tokens -> writeFile path $ showHtml . render mSrcCssFile $ tokens
    Nothing -> return ()
  where
    mSrcCssFile = Just $ srcCssFile
    path = srcPath outdir </> moduleSourceFile (ifaceMod iface)

moduleSourceFile :: Module -> FilePath
moduleSourceFile = (++ ".html") . moduleNameString . moduleName

srcPath :: FilePath -> FilePath
srcPath outdir = outdir </> "src"

srcCssFile :: FilePath
srcCssFile = "style.css"

defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
