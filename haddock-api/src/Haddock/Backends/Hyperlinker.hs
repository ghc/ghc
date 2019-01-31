{-# LANGUAGE OverloadedStrings #-}
module Haddock.Backends.Hyperlinker
    ( ppHyperlinkedSource
    , module Haddock.Backends.Hyperlinker.Types
    , module Haddock.Backends.Hyperlinker.Utils
    ) where


import Haddock.Types
import Haddock.Utils (writeUtf8File)
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils
import Haddock.Backends.Xhtml.Utils ( renderToString )

import Data.Maybe
import System.Directory
import System.FilePath

import HieTypes       ( HieFile(..), HieASTs(..) )
import HieBin         ( readHieFile )
import Data.Map as M
import FastString     ( mkFastString )
import Module         ( Module, moduleName )
import NameCache      ( initNameCache )
import UniqSupply     ( mkSplitUniqSupply )
import SysTools.Info  ( getCompilerInfo' )


-- | Generate hyperlinked source for given interfaces.
--
-- Note that list of interfaces should also contain interfaces normally hidden
-- when generating documentation. Otherwise this could lead to dead links in
-- produced source.
ppHyperlinkedSource :: FilePath -- ^ Output directory
                    -> FilePath -- ^ Resource directory
                    -> Maybe FilePath -- ^ Custom CSS file path
                    -> Bool -- ^ Flag indicating whether to pretty-print HTML
                    -> M.Map Module SrcPath -- ^ Paths to sources
                    -> [Interface] -- ^ Interfaces for which we create source
                    -> IO ()
ppHyperlinkedSource outdir libdir mstyle pretty srcs' ifaces = do
    createDirectoryIfMissing True srcdir
    let cssFile = fromMaybe (defaultCssFile libdir) mstyle
    copyFile cssFile $ srcdir </> srcCssFile
    copyFile (libdir </> "html" </> highlightScript) $
        srcdir </> highlightScript
    mapM_ (ppHyperlinkedModuleSource srcdir pretty srcs) ifaces
  where
    srcdir = outdir </> hypSrcDir
    srcs = (srcs', M.mapKeys moduleName srcs')

-- | Generate hyperlinked source for particular interface.
ppHyperlinkedModuleSource :: FilePath -> Bool -> SrcMaps -> Interface -> IO ()
ppHyperlinkedModuleSource srcdir pretty srcs iface = case ifaceHieFile iface of
    Just hfp -> do
        -- Parse the GHC-produced HIE file
        u <- mkSplitUniqSupply 'a'
        HieFile { hie_hs_file = file
                , hie_asts = HieASTs asts
                , hie_types = types
                , hie_hs_src = rawSrc
                } <- fmap fst (readHieFile (initNameCache u []) hfp)
        comp <- getCompilerInfo' df

        -- Get the AST and tokens corresponding to the source file we want
        let mast | M.size asts == 1 = snd <$> M.lookupMin asts
                 | otherwise        = M.lookup (mkFastString file) asts
            tokens = parse comp df file rawSrc

        -- Produce and write out the hyperlinked sources
        case mast of
          Just ast ->
              let fullAst = recoverFullIfaceTypes df types ast
              in writeUtf8File path . renderToString pretty . render' fullAst $ tokens
          Nothing
            | M.size asts == 0 -> return ()
            | otherwise -> error $ unwords [ "couldn't find ast for"
                                           , file, show (M.keys asts) ]
    Nothing -> return ()
  where
    df = ifaceDynFlags iface
    render' = render (Just srcCssFile) (Just highlightScript) srcs
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

