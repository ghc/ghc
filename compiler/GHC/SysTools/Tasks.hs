{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
--
-- Tasks running external programs for SysTools
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module GHC.SysTools.Tasks where

import GHC.Prelude
import GHC.ForeignSrcLang

import GHC.CmmToLlvm.Version (LlvmVersion, llvmVersionStr, supportedLlvmVersionUpperBound, parseLlvmVersion, supportedLlvmVersionLowerBound)

import GHC.Settings

import GHC.SysTools.Process

import GHC.Driver.Session
import GHC.Utils.Exception as Exception
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.Panic

import Control.Monad
import Data.List (tails, isPrefixOf)
import Data.Maybe (fromMaybe)
import System.IO
import System.Process
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types (GhcMessage(..), DriverMessage (DriverNoConfiguredLLVMToolchain))
import GHC.Driver.CmdLine (warnsToMessages)
import GHC.Types.SrcLoc (noLoc)

{-
************************************************************************
*                                                                      *
\subsection{Running an external program}
*                                                                      *
************************************************************************
-}

runUnlit :: Logger -> DynFlags -> [Option] -> IO ()
runUnlit logger dflags args = traceSystoolCommand logger "unlit" $ do
  let prog = pgm_L dflags
      opts = getOpts dflags opt_L
  runSomething logger "Literate pre-processor" prog
               (map Option opts ++ args)

-- | Prepend the working directory to the search path.
-- Note [Filepaths and Multiple Home Units]
augmentImports :: DynFlags  -> [FilePath] -> [FilePath]
augmentImports dflags fps | Nothing <- workingDirectory dflags  = fps
augmentImports _ [] = []
augmentImports _ [x] = [x]
augmentImports dflags ("-include":fp:fps) = "-include" : augmentByWorkingDirectory dflags fp  : augmentImports dflags fps
augmentImports dflags (fp1: fp2: fps) = fp1 : augmentImports dflags (fp2:fps)

-- | Discard some harmless warnings from gcc that we can't turn off
cc_filter :: [String] -> [String]
cc_filter = doFilter where
  {-
  gcc gives warnings in chunks like so:
      In file included from /foo/bar/baz.h:11,
                       from /foo/bar/baz2.h:22,
                       from wibble.c:33:
      /foo/flibble:14: global register variable ...
      /foo/flibble:15: warning: call-clobbered r...
  We break it up into its chunks, remove any call-clobbered register
  warnings from each chunk, and then delete any chunks that we have
  emptied of warnings.
  -}
  doFilter = unChunkWarnings . filterWarnings . chunkWarnings []
  -- We can't assume that the output will start with an "In file inc..."
  -- line, so we start off expecting a list of warnings rather than a
  -- location stack.
  chunkWarnings :: [String] -- The location stack to use for the next
                            -- list of warnings
                -> [String] -- The remaining lines to look at
                -> [([String], [String])]
  chunkWarnings loc_stack [] = [(loc_stack, [])]
  chunkWarnings loc_stack xs
      = case break loc_stack_start xs of
        (warnings, lss:xs') ->
            case span loc_start_continuation xs' of
            (lsc, xs'') ->
                (loc_stack, warnings) : chunkWarnings (lss : lsc) xs''
        _ -> [(loc_stack, xs)]

  filterWarnings :: [([String], [String])] -> [([String], [String])]
  filterWarnings [] = []
  -- If the warnings are already empty then we are probably doing
  -- something wrong, so don't delete anything
  filterWarnings ((xs, []) : zs) = (xs, []) : filterWarnings zs
  filterWarnings ((xs, ys) : zs) = case filter wantedWarning ys of
                                       [] -> filterWarnings zs
                                       ys' -> (xs, ys') : filterWarnings zs

  unChunkWarnings :: [([String], [String])] -> [String]
  unChunkWarnings [] = []
  unChunkWarnings ((xs, ys) : zs) = xs ++ ys ++ unChunkWarnings zs

  loc_stack_start        s = "In file included from " `isPrefixOf` s
  loc_start_continuation s = "                 from " `isPrefixOf` s
  wantedWarning w
   | "warning: call-clobbered register used" `isContainedIn` w = False
   | otherwise = True

-- | See the Note [Preprocessing invocations]
data SourceCodePreprocessor
  = SCPCpp
    -- ^ Use the ordinary C preprocessor
  | SCPHsCpp
    -- ^ Use the Haskell C preprocessor (don't remove C comments, don't break on names including single quotes)
  | SCPJsCpp
    -- ^ Use the JavaScript preprocessor (don't remove jsdoc and multiline comments)
  | SCPCmmCpp
    -- ^ Use the C-- preprocessor (don't emit debug information)
  deriving (Eq)

-- | Run source code preprocessor.
-- See also Note [Preprocessing invocations] in GHC.SysTools.Cpp
runSourceCodePreprocessor
  :: Logger
  -> TmpFs
  -> DynFlags
  -> SourceCodePreprocessor
  -> [Option]
  -> IO ()
runSourceCodePreprocessor logger tmpfs dflags preprocessor args =
  traceSystoolCommand logger logger_name $ do
    let
      (program, configured_args) = pgm_getter dflags
      runtime_args = Option <$> (augmentImports dflags $ getOpts dflags opt_getter)
      extra_warns = [Option "-Werror" | gopt Opt_WarnIsError dflags]
                    ++ [Option "-Wundef" | wopt Opt_WarnCPPUndef dflags]
      all_args = configured_args ++ runtime_args ++ extra_warns ++ args

    mb_env <- getGccEnv (configured_args ++ runtime_args)

    runSomething readable_name program all_args mb_env

  where
    toolSettings' = toolSettings dflags
    cmmG0 = ["-g0" | toolSettings_cmmCppSupportsG0 toolSettings']
    -- GCC <=10 (pre commit r11-5596-g934a54180541d2) implied -dD for debug
    -- flags by the spec snippet %{g3|ggdb3|gstabs3|gxcoff3|gvms3:-dD}.  This
    -- means that a g0 will not override a previously-specified -g3 causing
    -- debug info emission (see https://gcc.gnu.org/PR97989).  We're filtering
    -- -optc here, rather than the combined command line, in order to avoid an
    -- issue where a user has to, for some reason, override our decision.  If
    -- they see the need to do that, they can pass -optCmmP.
    g3Flags = ["-g3", "-ggdb3", "-gstabs3", "-gxcoff3", "-gvms3"]
    optCFiltered = filter (`notElem` g3Flags) . opt_c
    -- In the wild (and GHC), there is lots of code assuming that -optc gets
    -- passed to the C-- preprocessor too.  Note that the arguments are
    -- reversed by getOpts. That is, in the invocation, first come the runtime
    -- C opts, then -g0, then the runtime CmmP opts.
    cAndCmmOpt dflags =  opt_CmmP dflags ++ cmmG0 ++ optCFiltered dflags
    (logger_name, pgm_getter, opt_getter, readable_name)
      = case preprocessor of
        SCPCpp -> ("cpp", pgm_cpp, opt_c, "C pre-processor")
        SCPHsCpp -> ("hs-cpp", pgm_P, opt_P, "Haskell C pre-processor")
        SCPJsCpp -> ("js-cpp", pgm_JSP, opt_JSP, "JavaScript C pre-processor")
        SCPCmmCpp -> ("cmm-cpp", pgm_CmmP, cAndCmmOpt, "C-- C pre-processor")

    runSomethingResponseFileCpp
      = runSomethingResponseFile logger tmpfs (tmpDir dflags) cc_filter
    runSomethingFilteredOther phase_name pgm args mb_env
      = runSomethingFiltered logger id phase_name pgm args Nothing mb_env

    runSomething
      = case preprocessor of
        SCPCpp -> runSomethingResponseFileCpp
        SCPHsCpp -> runSomethingFilteredOther
        SCPJsCpp -> runSomethingFilteredOther
        SCPCmmCpp -> runSomethingResponseFileCpp

runPp :: Logger -> DynFlags -> [Option] -> IO ()
runPp logger dflags args = traceSystoolCommand logger "pp" $ do
  let prog = pgm_F dflags
      opts = map Option (getOpts dflags opt_F)
  runSomething logger "Haskell pre-processor" prog (args ++ opts)

-- | Run compiler of C-like languages and raw objects (such as gcc or clang).
runCc :: Maybe ForeignSrcLang -> Logger -> TmpFs -> DynFlags -> [Option] -> IO ()
runCc mLanguage logger tmpfs dflags args = traceSystoolCommand logger "cc" $ do
  let args1 = map Option userOpts
      args2 = languageOptions ++ args ++ args1
      -- We take care to pass -optc flags in args1 last to ensure that the
      -- user can override flags passed by GHC. See #14452.
  mb_env <- getGccEnv args2
  runSomethingResponseFile logger tmpfs (tmpDir dflags) cc_filter dbgstring prog args2
                           mb_env
 where
  -- force the C compiler to interpret this file as C when
  -- compiling .hc files, by adding the -x c option.
  -- Also useful for plain .c files, just in case GHC saw a
  -- -x c option.
  (languageOptions, userOpts, prog, dbgstring) = case mLanguage of
    Nothing -> ([], userOpts_c, pgm_c dflags, "C Compiler")
    Just language -> ([Option "-x", Option languageName], opts, prog, dbgstr)
      where
        (languageName, opts, prog, dbgstr) = case language of
          LangC      -> ("c",             userOpts_c
                        ,pgm_c dflags,    "C Compiler")
          LangCxx    -> ("c++",           userOpts_cxx
                        ,pgm_cxx dflags , "C++ Compiler")
          LangObjc   -> ("objective-c",   userOpts_c
                        ,pgm_c dflags   , "Objective C Compiler")
          LangObjcxx -> ("objective-c++", userOpts_cxx
                        ,pgm_cxx dflags,  "Objective C++ Compiler")
          LangAsm    -> ("assembler",     []
                        ,pgm_c dflags,    "Asm Compiler")
          RawObject  -> ("c",             []
                        ,pgm_c dflags,    "C Compiler") -- claim C for lack of a better idea
          --JS backend shouldn't reach here, so we just pass
          -- strings to satisfy the totality checker
          LangJs     -> ("js",            []
                        ,pgm_c dflags,    "JS Backend Compiler")
  userOpts_c   = getOpts dflags opt_c
  userOpts_cxx = getOpts dflags opt_cxx

isContainedIn :: String -> String -> Bool
xs `isContainedIn` ys = any (xs `isPrefixOf`) (tails ys)

-- | Run the linker with some arguments and return the output
askLd :: Logger -> DynFlags -> [Option] -> IO String
askLd logger dflags args = traceSystoolCommand logger "linker" $ do
  let (p,args0) = pgm_l dflags
      args1     = map Option (getOpts dflags opt_l)
      args2     = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingWith logger "gcc" p args2 $ \real_args ->
    readCreateProcessWithExitCode' (proc p real_args){ env = mb_env }

runAs :: Logger -> DynFlags -> [Option] -> IO ()
runAs logger dflags args = traceSystoolCommand logger "as" $ do
  let (p,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingFiltered logger id "Assembler" p args2 Nothing mb_env

-- | Run the LLVM Optimiser
runLlvmOpt :: Logger -> DynFlags -> [Option] -> IO ()
runLlvmOpt logger dflags args = traceSystoolCommand logger "opt" $ do
  let (p,args0) = pgm_lo dflags
      args1 = map Option (getOpts dflags opt_lo)
      -- We take care to pass -optlo flags (e.g. args0) last to ensure that the
      -- user can override flags passed by GHC. See #14821.
  runSomething logger "LLVM Optimiser" p (args1 ++ args ++ args0)

-- | Run the LLVM Compiler
runLlvmLlc :: Logger -> DynFlags -> [Option] -> IO ()
runLlvmLlc logger dflags args = traceSystoolCommand logger "llc" $ do
  let (p,args0) = pgm_lc dflags
      args1 = map Option (getOpts dflags opt_lc)
  runSomething logger "LLVM Compiler" p (args0 ++ args1 ++ args)

-- | Run the LLVM Assembler
runLlvmAs :: Logger -> DynFlags -> [Option] -> IO ()
runLlvmAs logger dflags args = traceSystoolCommand logger "llvm-as" $ do
  let (p,args0) = pgm_las dflags
      args1 = map Option (getOpts dflags opt_las)
  runSomething logger "LLVM assembler" p (args0 ++ args1 ++ args)


runEmscripten :: Logger -> DynFlags -> [Option] -> IO ()
runEmscripten logger dflags args = traceSystoolCommand logger "emcc" $ do
  let (p,args0) = pgm_a dflags
      args1     = args0 ++ args
  runSomething logger "Emscripten" p args1

-- | Figure out which version of LLVM we are running this session
figureLlvmVersion :: Logger -> DynFlags -> IO (Maybe LlvmVersion)
figureLlvmVersion logger dflags = traceSystoolCommand logger "llc" $ do
  let (pgm,opts) = pgm_lc dflags
      diag_opts = initDiagOpts dflags
      args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  -- Since !12001, when GHC is not configured with llc/opt with
  -- supported version range, configure script will leave llc/opt
  -- commands as blank in settings. In this case, we should bail out
  -- with a proper error, see #25011.
  --
  -- Note that this does not make the -Wunsupported-llvm-version
  -- warning logic redundant! Power users might want to use
  -- -pgmlc/-pgmlo to override llc/opt locations to test LLVM outside
  -- officially supported version range, and the driver will produce
  -- the warning and carry on code generation.
  when (null pgm) $
    printOrThrowDiagnostics logger (initPrintConfig dflags) diag_opts
      (GhcDriverMessage <$> warnsToMessages diag_opts [noLoc DriverNoConfiguredLLVMToolchain])
  catchIO (do
              (pin, pout, perr, p) <- runInteractiveProcess pgm args'
                                              Nothing Nothing
              {- > llc -version
                  LLVM (http://llvm.org/):
                    LLVM version 3.5.2
                    ...
              -}
              hSetBinaryMode pout False
              _     <- hGetLine pout
              vline <- hGetLine pout
              let mb_ver = parseLlvmVersion vline
              hClose pin
              hClose pout
              hClose perr
              _ <- waitForProcess p
              return mb_ver
            )
            (\err -> do
                debugTraceMsg logger 2
                    (text "Error (figuring out LLVM version):" <+>
                      text (show err))
                errorMsg logger $ vcat
                    [ text "Warning:", nest 9 $
                          text "Couldn't figure out LLVM version!" $$
                          text ("Make sure you have installed LLVM between ["
                                ++ llvmVersionStr supportedLlvmVersionLowerBound
                                ++ " and "
                                ++ llvmVersionStr supportedLlvmVersionUpperBound
                                ++ ")") ]
                return Nothing)

-- See Note [Merging object files for GHCi] in GHC.Driver.Pipeline.
runMergeObjects :: Logger -> TmpFs -> DynFlags -> [Option] -> IO ()
runMergeObjects logger tmpfs dflags args =
  traceSystoolCommand logger "merge-objects" $ do
    let (p,args0) = fromMaybe err (pgm_lm dflags)
        err = throwGhcException $ UsageError $ unwords
            [ "Attempted to merge object files but the configured linker"
            , "does not support object merging." ]
        optl_args = map Option (getOpts dflags opt_lm)
        args2     = args0 ++ args ++ optl_args
    if toolSettings_mergeObjsSupportsResponseFiles (toolSettings dflags)
      then do
        mb_env <- getGccEnv args2
        runSomethingResponseFile logger tmpfs (tmpDir dflags) id "Merge objects" p args2 mb_env
      else do
        runSomething logger "Merge objects" p args2

runAr :: Logger -> DynFlags -> Maybe FilePath -> [Option] -> IO ()
runAr logger dflags cwd args = traceSystoolCommand logger "ar" $ do
  let ar = pgm_ar dflags
  runSomethingFiltered logger id "Ar" ar args cwd Nothing

askOtool :: Logger -> ToolSettings -> Maybe FilePath -> [Option] -> IO String
askOtool logger toolSettings mb_cwd args = do
  let otool = toolSettings_pgm_otool toolSettings
  runSomethingWith logger "otool" otool args $ \real_args ->
    readCreateProcessWithExitCode' (proc otool real_args){ cwd = mb_cwd }

runInstallNameTool :: Logger -> ToolSettings -> [Option] -> IO ()
runInstallNameTool logger toolSettings args = do
  let tool = toolSettings_pgm_install_name_tool toolSettings
  runSomethingFiltered logger id "Install Name Tool" tool args Nothing Nothing

runRanlib :: Logger -> DynFlags -> [Option] -> IO ()
runRanlib logger dflags args = traceSystoolCommand logger "ranlib" $ do
  let ranlib = pgm_ranlib dflags
  runSomethingFiltered logger id "Ranlib" ranlib args Nothing Nothing

runWindres :: Logger -> DynFlags -> [Option] -> IO ()
runWindres logger dflags args = traceSystoolCommand logger "windres" $ do
  let cc_args = map Option (sOpt_c (settings dflags))
      windres = pgm_windres dflags
      opts = map Option (getOpts dflags opt_windres)
  mb_env <- getGccEnv cc_args
  runSomethingFiltered logger id "Windres" windres (opts ++ args) Nothing mb_env
