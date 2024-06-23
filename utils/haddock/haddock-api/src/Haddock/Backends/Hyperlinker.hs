{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Haddock.Backends.Hyperlinker
  ( ppHyperlinkedSource
  , module Haddock.Backends.Hyperlinker.Types
  , module Haddock.Backends.Hyperlinker.Utils
  ) where

import Data.Map as M
import Data.Maybe
import GHC.Data.FastString (mkFastString)
import GHC.Platform
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Driver.Session (supportedLanguagesAndExtensions, safeImportsOn)
import GHC.Parser.Lexer as Lexer
import qualified GHC.Driver.DynFlags as DynFlags
import GHC.Iface.Ext.Binary (hie_file_result, readHieFile)
import GHC.Iface.Ext.Types (HieAST (..), HieASTs (..), HieFile (..), SourcedNodeInfo (..), pattern HiePath)
import GHC.Types.SrcLoc (mkRealSrcLoc, realSrcLocSpan, srcSpanFile)
import GHC.Unit.Module (Module, moduleName)
import qualified GHC.Utils.Outputable as Outputable
import System.Directory
import System.FilePath

import Haddock.Backends.Hyperlinker.Parser
import Haddock.Backends.Hyperlinker.Renderer
import Haddock.Backends.Hyperlinker.Types
import Haddock.Backends.Hyperlinker.Utils
import Haddock.Backends.Xhtml.Utils (renderToString)
import Haddock.InterfaceFile
import Haddock.Types
import Haddock.Utils (Verbosity, out, verbose, writeUtf8File)

-- | Generate hyperlinked source for given interfaces.
--
-- Note that list of interfaces should also contain interfaces normally hidden
-- when generating documentation. Otherwise this could lead to dead links in
-- produced source.
ppHyperlinkedSource
  :: Verbosity
  -> FilePath
  -- ^ Output directory
  -> FilePath
  -- ^ Resource directory
  -> Maybe FilePath
  -- ^ Custom CSS file path
  -> Bool
  -- ^ Flag indicating whether to pretty-print HTML
  -> M.Map Module SrcPath
  -- ^ Paths to sources
  -> [Interface]
  -- ^ Interfaces for which we create source
  -> IO ()
ppHyperlinkedSource verbosity outdir libdir mstyle pretty srcs' ifaces = do
  createDirectoryIfMissing True srcdir
  let cssFile = fromMaybe (defaultCssFile libdir) mstyle
  copyFile cssFile $ srcdir </> srcCssFile
  copyFile (libdir </> "html" </> highlightScript) $
    srcdir </> highlightScript
  mapM_ (ppHyperlinkedModuleSource verbosity srcdir pretty srcs) ifaces
  where
    srcdir = outdir </> hypSrcDir
    srcs = (srcs', M.mapKeys moduleName srcs')

-- | Generate hyperlinked source for particular interface.
ppHyperlinkedModuleSource :: Verbosity -> FilePath -> Bool -> SrcMaps -> Interface -> IO ()
ppHyperlinkedModuleSource verbosity srcdir pretty srcs iface = do
  -- Parse the GHC-produced HIE file
  nc <- freshNameCache
  HieFile
    { hie_hs_file = file
    , hie_asts = HieASTs asts
    , hie_types = types
    , hie_hs_src = rawSrc
    } <-
    hie_file_result
      <$> (readHieFile nc iface.ifaceHieFile)

  -- Get the AST and tokens corresponding to the source file we want
  let fileFs = mkFastString file
      mast
        | M.size asts == 1 = snd <$> M.lookupMin asts
        | otherwise = M.lookup (HiePath (mkFastString file)) asts
      tokens' = parse parserOpts sDocContext file rawSrc
      ast = fromMaybe (emptyHieAst fileFs) mast
      fullAst = recoverFullIfaceTypes sDocContext types ast

  -- Warn if we didn't find an AST, but there were still ASTs
  if M.null asts
    then pure ()
    else
      out verbosity verbose $
        unwords
          [ "couldn't find ast for"
          , file
          , show (M.keys asts)
          ]

  -- The C preprocessor can double the backslashes on tokens (see #19236),
  -- which means the source spans will not be comparable and we will not
  -- be able to associate the HieAST with the correct tokens.
  --
  -- We work around this by setting the source span of the tokens to the file
  -- name from the HieAST
  let tokens = fmap (\tk -> tk{tkSpan = (tkSpan tk){srcSpanFile = srcSpanFile $ nodeSpan fullAst}}) tokens'

  -- Produce and write out the hyperlinked sources
  writeUtf8File path . renderToString pretty . render' fullAst $ tokens
  where
    dflags = ifaceDynFlags iface
    arch_os = platformArchOS (dflags.targetPlatform)
    sDocContext = DynFlags.initSDocContext dflags Outputable.defaultUserStyle
    parserOpts =
      Lexer.mkParserOpts
        (dflags.extensionFlags)
        (initDiagOpts dflags)
        (supportedLanguagesAndExtensions arch_os)
        (safeImportsOn dflags)
        False -- lex Haddocks as comment tokens
        True -- produce comment tokens
        False -- produce position pragmas tokens
    render' = render (Just srcCssFile) (Just highlightScript) srcs
    path = srcdir </> hypSrcModuleFile (ifaceMod iface)

    emptyHieAst fileFs =
      Node
        { nodeSpan = realSrcLocSpan (mkRealSrcLoc fileFs 1 0)
        , nodeChildren = []
        , sourcedNodeInfo = SourcedNodeInfo mempty
        }

-- | Name of CSS file in output directory.
srcCssFile :: FilePath
srcCssFile = "style.css"

-- | Name of highlight script in output and resource directory.
highlightScript :: FilePath
highlightScript = "highlight.js"

-- | Path to default CSS file.
defaultCssFile :: FilePath -> FilePath
defaultCssFile libdir = libdir </> "html" </> "solarized.css"
