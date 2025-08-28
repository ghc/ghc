{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Char (toUpper,isSpace)
import Data.Maybe (isNothing,fromMaybe)
import qualified Data.List as List
import System.Exit
import System.Console.GetOpt
import System.Environment
import System.FilePath ((</>))
import qualified System.IO (readFile, writeFile)

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Program
import GHC.Toolchain.Target
import GHC.Toolchain.PlatformDetails
import GHC.Toolchain.ParseTriple
import GHC.Toolchain.Utils

import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Cxx
import GHC.Toolchain.Tools.Cpp
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.Ar
import GHC.Toolchain.Tools.Ranlib
import GHC.Toolchain.Tools.Nm
import GHC.Toolchain.Tools.MergeObjs
import GHC.Toolchain.Tools.Readelf
import GHC.Toolchain.NormaliseTriple (normaliseTriple)
import Text.Read (readMaybe)

data Opts = Opts
    { optTriple    :: Maybe String
    , optTargetPrefix :: Maybe String
    , optLocallyExecutable :: Maybe Bool
    , optLlvmTriple :: Maybe String
    , optOutput    :: Maybe String
    , optCc        :: ProgOpt
    , optCxx       :: ProgOpt
    , optCpp       :: ProgOpt
    , optHsCpp     :: ProgOpt
    , optJsCpp     :: ProgOpt
    , optCmmCpp    :: ProgOpt
    , optCcLink    :: ProgOpt
    , optAr        :: ProgOpt
    , optRanlib    :: ProgOpt
    , optNm        :: ProgOpt
    , optReadelf   :: ProgOpt
    , optMergeObjs :: ProgOpt
    , optWindres   :: ProgOpt
    , optOtool     :: ProgOpt
    , optInstallNameTool :: ProgOpt
    -- Note we don't actually configure LD into anything but
    -- see #23857 and #22550 for the very unfortunate story.
    , optLd        :: ProgOpt
    , optUnregisterised :: Maybe Bool
    , optTablesNextToCode :: Maybe Bool
    , optUseLibFFIForAdjustors :: Maybe Bool
    , optLdOverride :: Maybe Bool
    , optVerbosity :: Int
    , optKeepTemp  :: Bool
    , optOutputSettings :: Bool -- ^ Output settings file, not Target
    }

data FormatOpts = FormatOpts
    { formatOptInput :: FilePath
    , formatOptOutput :: FilePath
    }

_formatOptOutput :: Lens FormatOpts String
_formatOptOutput = Lens formatOptOutput (\x o -> o {formatOptOutput=x})

_formatOptInput :: Lens FormatOpts String
_formatOptInput = Lens formatOptInput (\x o -> o {formatOptInput=x})

emptyFormatOpts :: FormatOpts
emptyFormatOpts = FormatOpts { formatOptInput = error "formatOpts: input"
                             , formatOptOutput = error "formatOpts: output"
                             }

emptyOpts :: Opts
emptyOpts = Opts
    { optTriple    = Nothing
    , optTargetPrefix = Nothing
    , optLocallyExecutable = Nothing
    , optLlvmTriple = Nothing
    , optOutput    = Nothing
    , optCc        = po0
    , optCxx       = po0
    , optCpp       = po0
    , optHsCpp     = po0
    , optJsCpp     = po0
    , optCmmCpp    = po0
    , optCcLink    = po0
    , optAr        = po0
    , optRanlib    = po0
    , optNm        = po0
    , optReadelf   = po0
    , optMergeObjs = po0
    , optWindres   = po0
    , optOtool     = po0
    , optInstallNameTool = po0
    , optLd        = po0
    , optUnregisterised = Nothing
    , optTablesNextToCode = Nothing
    , optUseLibFFIForAdjustors = Nothing
    , optLdOverride = Nothing
    , optVerbosity = 1
    , optKeepTemp  = False
    , optOutputSettings = False
    }
  where
    po0 = emptyProgOpt

_optCc, _optCxx, _optCpp, _optHsCpp, _optJsCpp, _optCmmCpp, _optCcLink, _optAr,
    _optRanlib, _optNm, _optReadelf, _optMergeObjs, _optWindres, _optLd
    :: Lens Opts ProgOpt
_optCc      = Lens optCc      (\x o -> o {optCc=x})
_optCxx     = Lens optCxx     (\x o -> o {optCxx=x})
_optCpp     = Lens optCpp     (\x o -> o {optCpp=x})
_optHsCpp   = Lens optHsCpp   (\x o -> o {optHsCpp=x})
_optJsCpp   = Lens optJsCpp   (\x o -> o {optJsCpp=x})
_optCmmCpp  = Lens optCmmCpp   (\x o -> o {optCmmCpp=x})
_optCcLink  = Lens optCcLink  (\x o -> o {optCcLink=x})
_optAr      = Lens optAr      (\x o -> o {optAr=x})
_optRanlib  = Lens optRanlib  (\x o -> o {optRanlib=x})
_optNm      = Lens optNm      (\x o -> o {optNm=x})
_optReadelf = Lens optReadelf (\x o -> o {optReadelf=x})
_optMergeObjs = Lens optMergeObjs (\x o -> o {optMergeObjs=x})
_optWindres = Lens optWindres (\x o -> o {optWindres=x})
_optOtool = Lens optOtool (\x o -> o {optOtool =x})
_optInstallNameTool = Lens optInstallNameTool (\x o -> o {optInstallNameTool=x})
_optLd = Lens optLd (\x o -> o {optLd= x})

_optTriple :: Lens Opts (Maybe String)
_optTriple = Lens optTriple (\x o -> o {optTriple=x})

_optLlvmTriple :: Lens Opts (Maybe String)
_optLlvmTriple = Lens optLlvmTriple (\x o -> o {optLlvmTriple=x})

_optOutput :: Lens Opts (Maybe String)
_optOutput = Lens optOutput (\x o -> o {optOutput=x})

_optTargetPrefix :: Lens Opts (Maybe String)
_optTargetPrefix = Lens optTargetPrefix (\x o -> o {optTargetPrefix=x})

_optLocallyExecutable, _optUnregisterised, _optTablesNextToCode, _optUseLibFFIForAdjustors, _optLdOvveride :: Lens Opts (Maybe Bool)
_optLocallyExecutable = Lens optLocallyExecutable (\x o -> o {optLocallyExecutable=x})
_optUnregisterised = Lens optUnregisterised (\x o -> o {optUnregisterised=x})
_optTablesNextToCode = Lens optTablesNextToCode (\x o -> o {optTablesNextToCode=x})
_optUseLibFFIForAdjustors = Lens optUseLibFFIForAdjustors (\x o -> o {optUseLibFFIForAdjustors=x})
_optLdOvveride = Lens optLdOverride (\x o -> o {optLdOverride=x})

_optOutputSettings :: Lens Opts Bool
_optOutputSettings = Lens optOutputSettings (\x o -> o {optOutputSettings=x})

_optVerbosity :: Lens Opts Int
_optVerbosity = Lens optVerbosity (\x o -> o {optVerbosity=x})

_optKeepTemp :: Lens Opts Bool
_optKeepTemp = Lens optKeepTemp (\x o -> o {optKeepTemp=x})

options :: [OptDescr (Opts -> Opts)]
options =
    [ tripleOpt
    , targetPrefixOpt
    , llvmTripleOpt
    , verbosityOpt
    , keepTempOpt
    , outputSettingsOpt
    , outputOpt
    ] ++
    concat
    [ enableDisable "unregisterised" "unregisterised backend" _optUnregisterised
    , enableDisable "tables-next-to-code" "info-tables-next-to-code optimisation" _optTablesNextToCode
    , enableDisable "libffi-adjustors" "the use of libffi for adjustors, even on platforms which have support for more efficient, native adjustor implementations." _optUseLibFFIForAdjustors
    , enableDisable "ld-override" "override gcc's default linker" _optLdOvveride
    , enableDisable "locally-executable" "the use of a target prefix which will be added to all tool names when searching for toolchain components" _optLocallyExecutable
    ] ++
    concat
    [ progOpts "cc" "C compiler" _optCc
    , progOpts "cpp" "C preprocessor" _optCpp
    , progOpts "hs-cpp" "Haskell C preprocessor" _optHsCpp
    , progOpts "js-cpp" "JavaScript C preprocessor" _optJsCpp
    , progOpts "cmm-cpp" "C-- C preprocessor" _optCmmCpp
    , progOpts "cxx" "C++ compiler" _optCxx
    , progOpts "cc-link" "C compiler for linking" _optCcLink
    , progOpts "ar" "ar archiver" _optAr
    , progOpts "ranlib" "ranlib utility" _optRanlib
    , progOpts "nm" "nm archiver" _optNm
    , progOpts "readelf" "readelf utility" _optReadelf
    , progOpts "merge-objs" "linker for merging objects" _optMergeObjs
    , progOpts "windres" "windres utility" _optWindres
    , progOpts "otool" "otool utility" _optOtool
    , progOpts "install-name-tool" "install_name_tool utility" _optInstallNameTool
    , progOpts "ld" "linker" _optLd
    ]
  where
    progOpts :: String -> String -> Lens Opts ProgOpt -> [OptDescr (Opts -> Opts)]
    progOpts progName description lens =
        [ Option [] [progName] (ReqArg (set (lens % _poPath) . progPath) metavar) ("Path of " ++ description)
        , Option [] [progName++"-opt"] (ReqArg (over (lens % _poFlags) . updatePoFlags) "OPTS") ("Flags to pass to " ++ progName)
        ]
      where
        metavar = map toUpper progName

        progPath "" = Nothing
        progPath p  = Just p

        -- Empty list of flags is as if it was unspecified
        updatePoFlags "" existingOpts      = existingOpts
        -- Otherwise prepend specified flags to existing flags or make new
        updatePoFlags newOpts Nothing      = Just [newOpts]
        updatePoFlags newOpts (Just eopts) = Just (newOpts:eopts)
        -- NB: By prepending, the resulting flags will match the left-to-right
        -- order they were passed in


    enableDisable :: String -> String -> Lens Opts (Maybe Bool) -> [OptDescr (Opts -> Opts)]
    enableDisable optName description lens =
        [ Option [] ["enable-" ++ optName] (NoArg (set lens (Just True))) ("Enable " ++ description)
        , Option [] ["disable-" ++ optName] (NoArg (set lens (Just False))) ("Disable " ++ description)
        ]

    tripleOpt = Option ['t'] ["triple"] (ReqArg (set _optTriple . Just) "TRIPLE") "Target triple"
    llvmTripleOpt = Option [] ["llvm-triple"] (ReqArg (set _optLlvmTriple . Just) "LLVM-TRIPLE") "LLVM Target triple"

    targetPrefixOpt = Option ['T'] ["target-prefix"] (ReqArg (set _optTargetPrefix . Just) "PREFIX")
        "A target prefix which will be added to all tool names when searching for toolchain components"


    verbosityOpt = Option ['v'] ["verbose"] (OptArg f "N") "set output verbosity"
      where
        f mb = set _optVerbosity (parseVerbosity mb)
        parseVerbosity :: Maybe String -> Int
        parseVerbosity mb
          | Nothing <- mb        = 1
          | Just s <- mb
          , (n, ""):_ <- reads s = n
          | otherwise            = error "unparseable verbosity level"

    keepTempOpt = Option [] ["keep-temp"] (NoArg (set _optKeepTemp True))
        "do not remove temporary files"

    outputSettingsOpt = Option [] ["output-settings"] (NoArg (set _optOutputSettings True))
        "output settings instead of Target"

    outputOpt = Option ['o'] ["output"] (ReqArg (set _optOutput . Just) "OUTPUT")
        "The output path for the generated target toolchain configuration"

formatOpts :: [OptDescr (FormatOpts -> FormatOpts)]
formatOpts = [
    (Option ['o'] ["output"] (ReqArg (set _formatOptOutput) "OUTPUT")
        "The output path for the formatted target toolchain configuration")
    , (Option ['i'] ["input"] (ReqArg (set _formatOptInput) "INPUT")
        "The target file to format")
    ]

validateOpts :: Opts -> [String]
validateOpts opts = mconcat
    [ assertJust _optTriple "missing --triple flag"
    , assertJust _optOutput "missing --output flag"
    ]
  where
    assertJust :: Lens Opts (Maybe a) -> String -> [String]
    assertJust lens msg =
      [ msg | Nothing <- pure $ view lens opts ]

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      ("format": args) -> doFormat args
      _ -> doConfigure argv

-- The format mode is very useful for normalising paths and newlines on windows.
doFormat :: [String] -> IO ()
doFormat args = do
  let (opts0, _nonopts, errs) = getOpt RequireOrder formatOpts args
  case errs of
    [] -> do
      let opts = foldr (.) id opts0 emptyFormatOpts
      tgtFile <- System.IO.readFile (view _formatOptInput opts)
      case readMaybe @Target tgtFile of
        Nothing -> error $ "Failed to read a valid Target value from " ++ view _formatOptInput opts ++ ":\n" ++ tgtFile
        Just tgt -> do
          let file = formatOptOutput opts
          System.IO.writeFile file (show tgt)
    _ -> do
      mapM_ putStrLn errs
      putStrLn $ usageInfo "ghc-toolchain" formatOpts
      exitWith (ExitFailure 1)



doConfigure :: [String] -> IO ()
doConfigure args = do
    let (opts0, _nonopts, parseErrs) = getOpt RequireOrder options args
    let opts = foldr (.) id opts0 emptyOpts
    case parseErrs ++ validateOpts opts of
      [] -> do
          let env = Env { verbosity = optVerbosity opts
                        , targetPrefix = case optTargetPrefix opts of
                                           Just prefix -> Just prefix
                                           Nothing -> Just $ fromMaybe (error "undefined triple") (optTriple opts) ++ "-"
                        , keepTemp = optKeepTemp opts
                        , canLocallyExecute = fromMaybe True (optLocallyExecutable opts)
                        , logContexts = []
                        }
          r <- runM env (run opts)
          case r of
            Left err -> print err >> exitWith (ExitFailure 2)
            Right () -> return ()
      errs -> do
        mapM_ putStrLn errs
        putStrLn $ usageInfo "ghc-toolchain" options
        exitWith (ExitFailure 1)

run :: Opts -> M ()
run opts = do
    tgt <- mkTarget opts
    logDebug $ "Final Target: " ++ show tgt
    let file = fromMaybe (error "undefined --output") (optOutput opts)
    let output = case optOutputSettings opts of
          False -> show tgt
          True  -> show (targetToSettings tgt)
    writeFile file output

optional :: M a -> M (Maybe a)
optional k = fmap Just k <|> pure Nothing

registerisedSupported :: ArchOS -> Bool
registerisedSupported archOs =
    case archOS_arch archOs of
      ArchX86       -> True
      ArchX86_64    -> True
      ArchPPC       -> True
      ArchPPC_64 _  -> True
      ArchS390X     -> True
      ArchARM _ _ _ -> True
      ArchAArch64   -> True
      ArchRISCV64   -> True
      ArchWasm32    -> True
      ArchJavaScript -> True
      ArchLoongArch64 -> True
      _             -> False

determineUnregisterised :: ArchOS -> Maybe Bool -> M Bool
determineUnregisterised archOs userReq =
    case userReq of
      Just False -- user requested registerised build
        | not regSupported -> throwE "GHC doesn't support registerised compilation on this architecture"
        | otherwise        -> return False
      Just True            -> return True
      Nothing -- user wasn't explicit, do registerised if we support it
        | regSupported     -> return False
        | otherwise        -> return True
  where
    regSupported = registerisedSupported archOs

tablesNextToCodeSupported :: ArchOS -> Bool
tablesNextToCodeSupported archOs =
    case archOS_arch archOs of
      ArchPPC      -> False
      ArchPPC_64 _ -> False
      ArchS390X    -> False
      ArchAArch64  -> archOS_OS archOs /= OSMinGW32
      _            -> True

determineTablesNextToCode
    :: ArchOS
    -> Bool       -- ^ unregisterised
    -> Maybe Bool -- ^ user flag
    -> M Bool
determineTablesNextToCode archOs unreg userReq =
    case userReq of
      Just True
        | not tntcSupported
                        -> throwE "Tables-next-to-code not supported by this platform"
        | unreg         -> throwE "Tables-next-to-code cannot be used with unregisterised code generator"
        | otherwise     -> return True
      Just False        -> return False
      Nothing           -> pure tntcSupported
  where
    tntcSupported = tablesNextToCodeSupported archOs

determineUseLibFFIForAdjustors :: ArchOS
                               -> Maybe Bool -- ^ Enable/disable option --libffi-adjustors
                               -> M Bool
determineUseLibFFIForAdjustors archOs mb = checking "whether to use libffi for adjustors" $
  case mb of
    Just True ->
      -- The user explicitly requested it
      pure True
    _    ->
      -- If don't have a native adjustor implementation we use libffi
      pure (not . archHasNativeAdjustors $ archOS_arch archOs)

-- | Do we implement a native adjustor implementation (i.e. found in @rts/adjustors@) for this 'Arch'?
archHasNativeAdjustors :: Arch -> Bool
archHasNativeAdjustors = \case
  ArchX86    -> True
  ArchX86_64 -> True
  ArchJavaScript -> True
  _          -> False


-- | The platforms which we attempt to override ld
ldOverrideWhitelist :: ArchOS -> Bool
ldOverrideWhitelist a =
  case archOS_OS a of
    OSLinux   -> True
    OSMinGW32 -> True
    _ -> False



mkTarget :: Opts -> M Target
mkTarget opts = do
    normalised_triple <- normaliseTriple (fromMaybe (error "missing --triple") (optTriple opts))
    -- Use Llvm target if specified, otherwise use triple as llvm target
    let tgtLlvmTarget = fromMaybe normalised_triple (optLlvmTriple opts)

    (archOs, tgtVendor) <- do
      cc0 <- findBasicCc (optCc opts)
      parseTriple cc0 normalised_triple

    cc0 <- findCc archOs tgtLlvmTarget (optCc opts)
    cxx <- findCxx archOs tgtLlvmTarget (optCxx opts)
    cpp <- findCpp (optCpp opts) cc0
    hsCpp <- findHsCpp (optHsCpp opts) cc0
    -- TODO: same case as ranlib below
    -- TODO: we need it really only for javascript target (maybe wasm target as well)
    jsCpp <- Just <$> findJsCpp (optJsCpp opts) cc0
    cmmCpp <- findCmmCpp (optCmmCpp opts) cc0
    cc <- addPlatformDepCcFlags archOs cc0
    readelf <- optional $ findReadelf (optReadelf opts)
    ccLink <- findCcLink tgtLlvmTarget (optLd opts) (optCcLink opts) (ldOverrideWhitelist archOs && fromMaybe True (optLdOverride opts)) archOs cc readelf

    ar <- findAr tgtVendor (optAr opts)
    -- TODO: We could have
    -- ranlib <- if arNeedsRanlib ar
    --              then Just <$> findRanlib (optRanlib opts)
    --              else return Nothing
    -- but in order to match the configure output, for now we do
    ranlib <- Just <$> findRanlib (optRanlib opts)

    nm <- findNm (optNm opts)
    mergeObjs <- optional $ findMergeObjs (optMergeObjs opts) cc ccLink nm

    when (isNothing mergeObjs && not (arSupportsDashL ar)) $
      throwE "Neither a object-merging tool (e.g. ld -r) nor an ar that supports -L is available"

    -- Windows-specific utilities
    windres <-
        case archOS_OS archOs of
          OSMinGW32 -> do
            windres <- findProgram "windres" (optWindres opts) ["windres"]
            return (Just windres)
          _ -> return Nothing

    otool <-
        case archOS_OS archOs of
          OSDarwin -> do
            otool <- findProgram "otool" (optOtool opts) ["otool"]
            return (Just otool)
          _ -> return Nothing

    install_name_tool <-
        case archOS_OS archOs of
          OSDarwin -> do
            install_name_tool <- findProgram "install_name_tool" (optInstallNameTool opts) ["install_name_tool"]
            return (Just install_name_tool)
          _ -> return Nothing

    -- various other properties of the platform
    tgtWordSize <- checkWordSize cc
    tgtEndianness <- checkEndianness cc
    tgtSymbolsHaveLeadingUnderscore <- checkLeadingUnderscore cc nm
    tgtSupportsSubsectionsViaSymbols <- checkSubsectionsViaSymbols archOs cc
    tgtSupportsIdentDirective <- checkIdentDirective cc
    tgtSupportsGnuNonexecStack <- checkGnuNonexecStack archOs cc

    -- code generator configuration
    tgtUnregisterised <- determineUnregisterised archOs (optUnregisterised opts)
    tgtTablesNextToCode <-
        determineTablesNextToCode archOs tgtUnregisterised (optTablesNextToCode opts)
    tgtUseLibffi <- determineUseLibFFIForAdjustors archOs (optUseLibFFIForAdjustors opts)
    when tgtUnregisterised $ do
        -- The via-C code generator requires these
        let prog = "int main(int argc, char** argv) { return 0; }"
            via_c_args = ["-fwrapv", "-fno-builtin"]
        forM_ via_c_args $ \arg -> checking ("support of "++arg) $ withTempDir $ \dir -> do
            let cc' = over (_ccProgram % _prgFlags) (++ [arg]) cc
            compileC cc' (dir </> "test.o") prog
            return ()

    let t = Target { tgtArchOs = archOs
                   , tgtVendor
                   , tgtLocallyExecutable = fromMaybe True (optLocallyExecutable opts)
                   , tgtCCompiler = cc
                   , tgtCxxCompiler = cxx
                   , tgtCPreprocessor = cpp
                   , tgtHsCPreprocessor = hsCpp
                   , tgtJsCPreprocessor = jsCpp
                   , tgtCmmCPreprocessor = cmmCpp
                   , tgtAr = ar
                   , tgtCCompilerLink = ccLink
                   , tgtRanlib = ranlib
                   , tgtNm = nm
                   , tgtMergeObjs = mergeObjs
                   , tgtWindres = windres
                   , tgtOtool = otool
                   , tgtInstallNameTool = install_name_tool
                   , tgtWordSize
                   , tgtEndianness
                   , tgtUnregisterised
                   , tgtTablesNextToCode
                   , tgtUseLibffiForAdjustors = tgtUseLibffi
                   , tgtSymbolsHaveLeadingUnderscore
                   , tgtSupportsSubsectionsViaSymbols
                   , tgtSupportsIdentDirective
                   , tgtSupportsGnuNonexecStack
                   , tgtLlvmTarget
                   }
    return t

--- ROMES:TODO: fp_settings.m4 in general which I don't think was ported completely (e.g. the basenames and windows llvm-XX and such)


targetToSettings :: Target -> [(String,String)]
targetToSettings tgt@Target{..} =
  [ ("C compiler command",   ccPath)
  , ("C compiler flags",     ccFlags)
  , ("C++ compiler command", cxxPath)
  , ("C++ compiler flags",   cxxFlags)
  , ("C compiler link flags",       clinkFlags)
  , ("C compiler supports -no-pie", linkSupportsNoPie)
  , ("CPP command",         cppPath)
  , ("CPP flags",           cppFlags)
  , ("Haskell CPP command", hsCppPath)
  , ("Haskell CPP flags",   hsCppFlags)
  , ("JavaScript CPP command", jsCppPath)
  , ("JavaScript CPP flags", jsCppFlags)
  , ("C-- CPP command", cmmCppPath)
  , ("C-- CPP flags",   cmmCppFlags)
  , ("C-- CPP supports -g0", cmmCppSupportsG0')
  , ("ld supports compact unwind", linkSupportsCompactUnwind)
  , ("ld supports filelist",       linkSupportsFilelist)
  , ("ld supports single module",  linkSupportsSingleModule)
  , ("ld is GNU ld",               linkIsGnu)
  , ("Merge objects command", mergeObjsPath)
  , ("Merge objects flags", mergeObjsFlags)
  , ("Merge objects supports response files", mergeObjsSupportsResponseFiles')
  , ("ar command",          arPath)
  , ("ar flags",            arFlags)
  , ("ar supports at file", arSupportsAtFile')
  , ("ar supports -L",      arSupportsDashL')
  , ("ranlib command", ranlibPath)
  , ("otool command", maybe "otool" prgPath tgtOtool)
  , ("install_name_tool command", maybe "install_name_tool" prgPath tgtInstallNameTool)
  , ("windres command", maybe "/bin/false" prgPath tgtWindres) -- TODO: /bin/false is not available on many distributions by default, but we keep it as it were before the ghc-toolchain patch. Fix-me.
  , ("unlit command", "$topdir/../bin/unlit") -- FIXME
  , ("cross compiling", yesNo False) -- FIXME: why do we need this settings at all?
  , ("target platform string", targetPlatformTriple tgt)
  , ("target os",        (show $ archOS_OS tgtArchOs))
  , ("target arch",      (show $ archOS_arch tgtArchOs))
  , ("target word size", wordSize)
  , ("target word big endian",       isBigEndian)
  , ("target has GNU nonexec stack", (yesNo tgtSupportsGnuNonexecStack))
  , ("target has .ident directive",  (yesNo tgtSupportsIdentDirective))
  , ("target has subsections via symbols", (yesNo tgtSupportsSubsectionsViaSymbols))
  , ("target has libm", has_libm)
  , ("Unregisterised", (yesNo tgtUnregisterised))
  , ("LLVM target", tgtLlvmTarget)
  , ("LLVM llc command", llc_cmd)
  , ("LLVM opt command", llvm_opt_cmd)
  , ("LLVM llvm-as command", llvm_as_cmd)
  , ("LLVM llvm-as flags", "") -- see ec826009b3a9d5f8e975ca2c8002832276043c18, #25793
  , ("Use inplace MinGW toolchain", use_inplace_mingw)
  , ("target RTS linker only supports shared libraries", yesNo (targetRTSLinkerOnlySupportsSharedLibs tgt))
  , ("Use interpreter", yesNo (targetSupportsInterpreter tgt))
  , ("Support SMP", yesNo (targetSupportsSMP tgt))
  , ("RTS ways", "v") -- FIXME: should be a property of the RTS, not of the target
  , ("Tables next to code", (yesNo tgtTablesNextToCode))
  , ("Leading underscore",  (yesNo tgtSymbolsHaveLeadingUnderscore))
  , ("Use LibFFI", yesNo tgtUseLibffiForAdjustors)
  , ("RTS expects libdw", yesNo False) -- FIXME
  , ("Relative Global Package DB", "package.conf.d") -- FIXME
  , ("base unit-id", "")
  ]
  where
    yesNo True  = "YES"
    yesNo False = "NO"

    wordSize = show (wordSize2Bytes tgtWordSize)
    isBigEndian = yesNo $ (\case BigEndian -> True; LittleEndian -> False) tgtEndianness

    has_libm = "NO" -- FIXME
    llc_cmd = "llc" -- FIXME
    llvm_opt_cmd = "opt" -- FIXME
    llvm_as_cmd = "llvm-as" -- FIXME
    use_inplace_mingw = "NO" -- FIXME

    ccPath  = prgPath $ ccProgram tgtCCompiler
    ccFlags = escapeArgs $ prgFlags $ ccProgram tgtCCompiler
    cxxPath  = prgPath $ cxxProgram tgtCxxCompiler
    cxxFlags = escapeArgs $ prgFlags $ cxxProgram tgtCxxCompiler
    clinkFlags = escapeArgs $ prgFlags $ ccLinkProgram tgtCCompilerLink
    linkSupportsNoPie = yesNo $ ccLinkSupportsNoPie tgtCCompilerLink
    cppPath  = prgPath $ cppProgram tgtCPreprocessor
    cppFlags = escapeArgs $ prgFlags $ cppProgram tgtCPreprocessor
    hsCppPath  = prgPath $ hsCppProgram tgtHsCPreprocessor
    hsCppFlags = escapeArgs $ prgFlags $ hsCppProgram tgtHsCPreprocessor
    jsCppPath  = maybe "" (prgPath . jsCppProgram) tgtJsCPreprocessor
    jsCppFlags = maybe "" (escapeArgs . prgFlags . jsCppProgram) tgtJsCPreprocessor
    cmmCppPath  = prgPath $ cmmCppProgram tgtCmmCPreprocessor
    cmmCppFlags = escapeArgs $ prgFlags $ cmmCppProgram tgtCmmCPreprocessor
    cmmCppSupportsG0' = yesNo $ cmmCppSupportsG0 tgtCmmCPreprocessor
    mergeObjsPath  = maybe "" (prgPath . mergeObjsProgram) tgtMergeObjs
    mergeObjsFlags = maybe "" (escapeArgs . prgFlags . mergeObjsProgram) tgtMergeObjs
    linkSupportsSingleModule    = yesNo $ ccLinkSupportsSingleModule tgtCCompilerLink
    linkSupportsFilelist        = yesNo $ ccLinkSupportsFilelist tgtCCompilerLink
    linkSupportsCompactUnwind   = yesNo $ ccLinkSupportsCompactUnwind tgtCCompilerLink
    linkIsGnu                   = yesNo $ ccLinkIsGnu tgtCCompilerLink
    arPath  = prgPath $ arMkArchive tgtAr
    arFlags = escapeArgs $ prgFlags (arMkArchive tgtAr)
    arSupportsAtFile' = yesNo (arSupportsAtFile tgtAr)
    arSupportsDashL' = yesNo (arSupportsDashL tgtAr)
    ranlibPath  = maybe "" (prgPath . ranlibProgram) tgtRanlib
    mergeObjsSupportsResponseFiles' = maybe "NO" (yesNo . mergeObjsSupportsResponseFiles) tgtMergeObjs

-- | Just like 'GHC.ResponseFile.escapeArgs', but use spaces instead of newlines
-- for splitting elements.
escapeArgs :: [String] -> String
escapeArgs = unwords . map escapeArg

escapeArg :: String -> String
escapeArg = reverse . List.foldl' escape []

escape :: String -> Char -> String
escape cs c
  |    isSpace c
    || '\\' == c
    || '\'' == c
    || '"'  == c = c:'\\':cs -- n.b., our caller must reverse the result
  | otherwise    = c:cs

-- | Does the target support the -N RTS flag?
--
-- Adapated from hadrian: Oracles.Flag.targetSupportsSMP
targetSupportsSMP :: Target -> Bool
targetSupportsSMP Target{..} = case archOS_arch tgtArchOs of
    -- The THREADED_RTS requires `BaseReg` to be in a register and the
    -- Unregisterised mode doesn't allow that.
  _ | tgtUnregisterised -> False
  ArchARM isa _ _
    -- We don't support load/store barriers pre-ARMv7. See #10433.
    | isa < ARMv7   -> False
    | otherwise     -> True
  ArchX86           -> True
  ArchX86_64        -> True
  ArchPPC           -> True
  ArchPPC_64 ELF_V1 -> True
  ArchPPC_64 ELF_V2 -> True
  ArchAArch64       -> True
  ArchS390X         -> True
  ArchRISCV64       -> True
  ArchLoongArch64   -> True
  ArchAArch64       -> True
  _                 -> False



-- | Check whether the target supports GHCi.
--
-- Adapted from hadrian:Oracles.Settings.ghcWithInterpreter
targetSupportsInterpreter :: Target -> Bool
targetSupportsInterpreter Target{..} = goodOs && goodArch
  where
    goodOs = case archOS_OS tgtArchOs of
      OSMinGW32 -> True
      OSLinux   -> True
      OSSolaris2 -> True
      OSFreeBSD -> True
      OSDragonFly -> True
      OSNetBSD -> True
      OSOpenBSD -> True
      OSDarwin -> True
      OSKFreeBSD -> True
      OSWasi -> True
      _ -> False
      -- TODO "cygwin32"?

    goodArch = case archOS_arch tgtArchOs of
      ArchX86 -> True
      ArchX86_64 -> True
      ArchPPC -> True
      ArchAArch64 -> True
      ArchS390X -> True
      ArchPPC_64 ELF_V1 -> True
      ArchPPC_64 ELF_V2 -> True
      ArchRISCV64 -> True
      ArchWasm32 -> True
      ArchAArch64 -> True
      ArchARM {} -> True
      _ -> False


targetRTSLinkerOnlySupportsSharedLibs :: Target -> Bool
targetRTSLinkerOnlySupportsSharedLibs tgt = case archOS_arch (tgtArchOs tgt) of
  ArchWasm32 -> True
  _ -> False
