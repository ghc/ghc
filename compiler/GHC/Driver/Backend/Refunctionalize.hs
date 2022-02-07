{-# LANGUAGE FlexibleContexts #-}

module GHC.Driver.Backend.Refunctionalize
  ( applyCodeOutput
  , applyAssemblerInfoGetter
  , applyAssemblerProg
  , applyCDefs
  )
where

import GHC.Prelude

import GHC.Driver.Backend


import GHC.Utils.Error

import GHC.CmmToAsm     ( nativeCodeGen )

import GHC.Cmm              ( RawCmmGroup )
import GHC.CmmToC           ( cmmToC )

import GHC.Driver.Session
import GHC.Driver.Config.CmmToAsm  (initNCGConfig)

import GHC.Data.Stream           ( Stream )
import GHC.Utils.Outputable
import GHC.Utils.Ppr (Mode(LeftMode))
import GHC.Utils.Logger
import GHC.Utils.Exception (bracket)

import GHC.Unit

import GHC.Types.Unique.Supply ( mkSplitUniqSupply )

import System.IO

import GHC.SysTools.Info
import GHC.SysTools.Tasks
import GHC.CmmToLlvm.LlvmVersion
import GHC.Driver.Config.CmmToLlvm
import GHC.CmmToLlvm
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC.Data.Stream as Stream
import GHC.Platform


applyCDefs :: DefunctionalizedCDefs -> Logger -> DynFlags -> IO [String]
applyCDefs NoCDefs _ _ = return []
applyCDefs LlvmCDefs logger dflags = do
    llvmVer <- figureLlvmVersion logger dflags
    return $ case fmap llvmVersionList llvmVer of
               Just [m] -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,0) ]
               Just (m:n:_) -> [ "-D__GLASGOW_HASKELL_LLVM__=" ++ format (m,n) ]
               _ -> []
  where
    format (major, minor)
      | minor >= 100 = error "backendCDefs: Unsupported minor version"
      | otherwise = show (100 * major + minor :: Int) -- Contract is Int


applyCodeOutput
    :: DefunctionalizedCodeOutput
    -> Logger
    -> DynFlags
    -> Module
    -> ModLocation
    -> FilePath
    -> Set UnitId
    -> Stream IO RawCmmGroup a
    -> IO a
applyCodeOutput NcgCodeOutput = outputAsm
applyCodeOutput ViaCCodeOutput = outputC
applyCodeOutput LlvmCodeOutput = outputLlvm


outputAsm, outputLlvm, outputC
          :: Logger
          -> DynFlags
          -> Module
          -> ModLocation
          -> FilePath
          -> Set UnitId
          -> Stream IO RawCmmGroup a
          -> IO a

outputLlvm logger dflags _this_mod _location filenm _deps cmm_stream = do
  lcg_config <- initLlvmCgConfig logger dflags
  {-# SCC "llvm_output" #-} doOutput filenm $
    \f -> {-# SCC "llvm_CodeGen" #-}
      llvmCodeGen logger lcg_config f cmm_stream


outputAsm logger dflags this_mod location filenm _deps cmm_stream = do
  ncg_uniqs <- mkSplitUniqSupply 'n'
  debugTraceMsg logger 4 (text "Outputing asm to" <+> text filenm)
  let ncg_config = initNCGConfig dflags this_mod
  {-# SCC "OutputAsm" #-} doOutput filenm $
    \h -> {-# SCC "NativeCodeGen" #-}
      nativeCodeGen logger ncg_config location h ncg_uniqs cmm_stream

outputC logger dflags _module _location filenm unit_deps cmm_stream =
  withTiming logger (text "C codegen") (\a -> seq a () {- FIXME -}) $ do
    let pkg_names = map unitIdString (Set.toAscList unit_deps)
    doOutput filenm $ \ h -> do
      hPutStr h ("/* GHC_PACKAGES " ++ unwords pkg_names ++ "\n*/\n")
      hPutStr h "#include \"Stg.h\"\n"
      let platform = targetPlatform dflags
          writeC cmm = do
            let doc = cmmToC platform cmm
            putDumpFileMaybe logger Opt_D_dump_c_backend
                          "C backend output"
                          FormatC
                          doc
            let ctx = initSDocContext dflags (PprCode CStyle)
            printSDocLn ctx LeftMode h doc
      Stream.consume cmm_stream id writeC


doOutput :: String -> (Handle -> IO a) -> IO a
doOutput filenm io_action = bracket (openFile filenm WriteMode) hClose io_action


        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)


{-
        let (as_prog, get_asm_info) | backendWantsClangTools (backend dflags)
                    , platformOS platform == OSDarwin
                    = (GHC.SysTools.runClang, pure Clang)
                    | otherwise
                    = (GHC.SysTools.runAs, getAssemblerInfoGetter logger dflags)
-}

applyAssemblerInfoGetter
    :: DefunctionalizedAssemblerInfoGetter
    -> Logger -> DynFlags -> Platform -> IO CompilerInfo
applyAssemblerInfoGetter StandardAssemblerInfoGetter logger dflags _platform =
    getAssemblerInfo logger dflags
applyAssemblerInfoGetter DarwinClangAssemblerInfoGetter logger dflags platform =
    if platformOS platform == OSDarwin then
        pure Clang
    else
        getAssemblerInfo logger dflags

applyAssemblerProg
    :: DefunctionalizedAssemblerProg
    -> Logger -> DynFlags -> Platform -> [Option] -> IO ()
applyAssemblerProg StandardAssemblerProg logger dflags _platform =
    runAs logger dflags
applyAssemblerProg DarwinClangAssemblerProg logger dflags platform =
    if platformOS platform == OSDarwin then
        runClang logger dflags
    else
        runAs logger dflags



