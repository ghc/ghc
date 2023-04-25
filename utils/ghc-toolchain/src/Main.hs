{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad
import Data.Char (toUpper)
import System.Exit
import System.Console.GetOpt
import System.Environment
import System.FilePath ((</>))

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

data Opts = Opts
    { optTriple    :: String
    , optTargetPrefix :: Maybe String
    , optCc        :: ProgOpt
    , optCxx       :: ProgOpt
    , optCpp       :: ProgOpt
    , optCcLink    :: ProgOpt
    , optAr        :: ProgOpt
    , optRanlib    :: ProgOpt
    , optNm        :: ProgOpt
    , optReadelf   :: ProgOpt
    , optMergeObjs :: ProgOpt
    , optWindres   :: ProgOpt
    , optDllwrap   :: ProgOpt
    , optUnregisterised :: Maybe Bool
    , optTablesNextToCode :: Maybe Bool
    , optVerbosity :: Int
    , optKeepTemp  :: Bool
    }

emptyOpts :: Opts
emptyOpts = Opts
    { optTriple    = ""
    , optTargetPrefix = Nothing
    , optCc        = po0
    , optCxx       = po0
    , optCpp       = po0
    , optCcLink    = po0
    , optAr        = po0
    , optRanlib    = po0
    , optNm        = po0
    , optReadelf   = po0
    , optMergeObjs = po0
    , optDllwrap   = po0
    , optWindres   = po0
    , optUnregisterised = Nothing
    , optTablesNextToCode = Nothing
    , optVerbosity = 0
    , optKeepTemp  = False
    }
  where
    po0 = emptyProgOpt

_optCc, _optCxx, _optCpp, _optCcLink, _optAr, _optRanlib, _optNm,
    _optReadelf, _optMergeObjs, _optDllwrap, _optWindres
    :: Lens Opts ProgOpt
_optCc      = Lens optCc      (\x o -> o {optCc=x})
_optCxx     = Lens optCxx     (\x o -> o {optCxx=x})
_optCpp     = Lens optCpp     (\x o -> o {optCpp=x})
_optCcLink  = Lens optCcLink  (\x o -> o {optCcLink=x})
_optAr      = Lens optAr      (\x o -> o {optAr=x})
_optRanlib  = Lens optRanlib  (\x o -> o {optRanlib=x})
_optNm      = Lens optNm      (\x o -> o {optNm=x})
_optReadelf = Lens optReadelf (\x o -> o {optReadelf=x})
_optMergeObjs = Lens optMergeObjs (\x o -> o {optMergeObjs=x})
_optDllwrap = Lens optDllwrap (\x o -> o {optDllwrap=x})
_optWindres = Lens optWindres (\x o -> o {optWindres=x})

_optTriple :: Lens Opts String
_optTriple = Lens optTriple (\x o -> o {optTriple=x})

_optTargetPrefix :: Lens Opts (Maybe String)
_optTargetPrefix = Lens optTargetPrefix (\x o -> o {optTargetPrefix=x})

_optUnregisterised :: Lens Opts (Maybe Bool)
_optUnregisterised = Lens optUnregisterised (\x o -> o {optUnregisterised=x})

_optTablesNextToCode :: Lens Opts (Maybe Bool)
_optTablesNextToCode = Lens optTablesNextToCode (\x o -> o {optTablesNextToCode=x})

_optVerbosity :: Lens Opts Int
_optVerbosity = Lens optVerbosity (\x o -> o {optVerbosity=x})

_optKeepTemp :: Lens Opts Bool
_optKeepTemp = Lens optKeepTemp (\x o -> o {optKeepTemp=x})

options :: [OptDescr (Opts -> Opts)]
options = 
    [ tripleOpt
    , targetPrefixOpt
    , verbosityOpt
    , keepTempOpt
    ] ++
    concat
    [ enableDisable "unregisterised" "unregisterised backend" _optUnregisterised
    , enableDisable "tables-next-to-code" "Tables-next-to-code optimisation" _optTablesNextToCode
    ] ++
    concat
    [ progOpts "cc" "C compiler" _optCc
    , progOpts "cpp" "C preprocessor" _optCpp
    , progOpts "cxx" "C++ compiler" _optCxx
    , progOpts "cc-link" "C compiler for linking" _optCcLink
    , progOpts "ar" "ar archiver" _optAr
    , progOpts "ranlib" "ranlib utility" _optAr
    , progOpts "nm" "nm archiver" _optNm
    , progOpts "readelf" "readelf utility" _optReadelf
    , progOpts "merge-objs" "linker for merging objects" _optMergeObjs
    , progOpts "dllwrap" "dllwrap utility" _optDllwrap
    , progOpts "windres" "windres utility" _optWindres
    ]
  where
    progOpts :: String -> String -> Lens Opts ProgOpt -> [OptDescr (Opts -> Opts)]
    progOpts progName description lens =
        [ Option [] [progName] (ReqArg (set (lens % _poPath) . Just) metavar) ("Path of " ++ description)
        , Option [] [progName++"-opt"] (ReqArg (\x -> over (lens % _poFlags) (++[x])) "OPTS") ("Flags to pass to " ++ progName)
        ]
      where
        metavar = map toUpper progName

    enableDisable :: String -> String -> Lens Opts (Maybe Bool) -> [OptDescr (Opts -> Opts)]
    enableDisable optName description lens =
        [ Option [] ["enable-" ++ optName] (NoArg (set lens (Just True))) ("Enable " ++ description)
        , Option [] ["disable-" ++ optName] (NoArg (set lens (Just False))) ("Disable " ++ description)
        ]

    tripleOpt = Option ['t'] ["triple"] (ReqArg (set _optTriple) "TRIPLE") "Target triple"

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

main :: IO ()
main = do
    argv <- getArgs
    let (opts0, _nonopts, errs) = getOpt RequireOrder options argv
    let opts = foldr (.) id opts0 emptyOpts
    case errs of
      [] -> do
          let env = Env { verbosity = optVerbosity opts
                        , targetPrefix = case optTargetPrefix opts of
                                           Just prefix -> Just $ prefix
                                           Nothing -> Just $ optTriple opts ++ "-"
                        , keepTemp = optKeepTemp opts
                        , logContexts = []
                        }
          r <- runM env (run opts)
          case r of
            Left err -> print err >> exitWith (ExitFailure 2)
            Right () -> return ()
      _  -> do
        mapM_ putStrLn errs
        putStrLn $ usageInfo "ghc-toolchain" options
        exitWith (ExitFailure 1)

run :: Opts -> M ()
run opts = do
    tgt <- mkTarget opts
    logDebug $ "Final Target: " ++ show tgt
    writeFile "default.target" (show tgt)

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
      _             -> False

determineUnregisterised :: ArchOS -> Maybe Bool -> M Bool
determineUnregisterised archOs userReq =
    case userReq of
      Just False
        | not regSupported -> throwE "GHC doesn't support registerised compilation on this architecture"
        | otherwise        -> return False
      Just True            -> return True
      Nothing
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
      _            -> True

determineTablesNextToCode
    :: ArchOS
    -> Bool       -- ^ unregisterised
    -> Maybe Bool -- ^ user flag
    -> M Bool
determineTablesNextToCode archOs unreg userReq =
    case userReq of
      Just True
        | unreg         -> throwE "Tables-next-to-code cannot be used with unregisterised code generator"
        | tntcSupported -> throwE "Tables-next-to-code not supported by this platform"
        | otherwise     -> return True
      Just False        -> return False
      Nothing
        | tntcSupported -> return True
        | otherwise     -> return False
  where
    tntcSupported = tablesNextToCodeSupported archOs

mkTarget :: Opts -> M Target
mkTarget opts = do
    cc0 <- findCc (optCc opts)
    cxx <- findCxx (optCxx opts)
    cpp <- findCpp (optCpp opts) cc0
    archOs <- parseTriple cc0 (optTriple opts)
    cc <- addPlatformDepCcFlags archOs cc0
    readelf <- optional $ findReadelf (optReadelf opts)
    ccLink <- findCcLink (optCcLink opts) archOs cc readelf

    ar <- findAr (optAr opts)
    ranlib <- if arNeedsRanlib ar
                 then Just <$> findRanlib (optRanlib opts)
                 else return Nothing

    nm <- findNm (optNm opts)
    mergeObjs <- optional $ findMergeObjs (optMergeObjs opts) cc ccLink nm

    -- Windows-specific utilities
    (windres, dllwrap) <-
        case archOS_OS archOs of
          OSMinGW32 -> do
            windres <- findProgram "windres" (optWindres opts) ["windres"]
            dllwrap <- findProgram "dllwrap" (optDllwrap opts) ["dllwrap"]
            return (Just windres, Just dllwrap)
          _ -> return (Nothing, Nothing)

    -- various other properties of the platform
    tgtWordSize <- checkWordSize cc
    tgtEndianness <- checkEndianness cc
    tgtSymbolsHaveLeadingUnderscore <- checkLeadingUnderscore cc nm
    tgtSupportsSubsectionsViaSymbols <- checkSubsectionsViaSymbols cc
    tgtSupportsIdentDirective <- checkIdentDirective cc
    tgtSupportsGnuNonexecStack <- checkGnuNonexecStack archOs cc
    tgtLlvmTarget <- pure $ optTriple opts

    -- code generator configuration
    tgtUnregisterised <- determineUnregisterised archOs (optUnregisterised opts)
    tgtTablesNextToCode <-
        determineTablesNextToCode archOs tgtUnregisterised (optTablesNextToCode opts)
    when tgtUnregisterised $ do
        -- The via-C code generator requires these
        let prog = "int main(int argc, char** argv) { return 0; }I"
            via_c_args = ["-fwrapv", "-fno-builtin"]
        forM_ via_c_args $ \arg -> checking ("support of "++arg) $ withTempDir $ \dir -> do
            let cc' = over (_ccProgram % _prgFlags) (++ [arg]) cc
            compileC cc' (dir </> "test.o") prog
            return ()

    let t = Target { tgtArchOs = archOs
                   , tgtCCompiler = cc
                   , tgtCxxCompiler = cxx
                   , tgtCPreprocessor = cpp
                   , tgtAr = ar
                   , tgtCCompilerLink = ccLink
                   , tgtRanlib = ranlib
                   , tgtNm = nm
                   , tgtMergeObjs = mergeObjs
                   , tgtWindres = windres
                   , tgtDllwrap = dllwrap
                   , tgtWordSize
                   , tgtEndianness
                   , tgtUnregisterised
                   , tgtTablesNextToCode
                   , tgtSymbolsHaveLeadingUnderscore
                   , tgtSupportsSubsectionsViaSymbols
                   , tgtSupportsIdentDirective
                   , tgtSupportsGnuNonexecStack
                   , tgtLlvmTarget
                   }
    return t
