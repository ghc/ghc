module Haddock.Backends.Hyperlinker (ppHyperlinkedSource) where

import Haddock.Types
import Haddock.Backends.Hyperlinker.Renderer

import GHC
import Text.XHtml hiding ((</>))
import System.Directory
import System.FilePath

ppHyperlinkedSource :: FilePath -> FilePath -> Maybe FilePath -> [Interface]
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle ifaces = do
    createDirectoryIfMissing True (outdir </> "src")
    mapM_ (ppHyperlinkedModuleSource outdir mstyle) ifaces

ppHyperlinkedModuleSource :: FilePath -> Maybe FilePath -> Interface -> IO ()
ppHyperlinkedModuleSource outdir mstyle iface = case ifaceTokenizedSrc iface of
    Just tokens -> writeFile path $ showHtml . render mstyle $ tokens
    Nothing -> return ()
  where
    path = outdir </> "src" </> moduleSourceFile (ifaceMod iface)

moduleSourceFile :: Module -> FilePath
moduleSourceFile = (++ ".html") . moduleNameString . moduleName
