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
import GHC.Platform
import GHC.ForeignSrcLang
import GHC.IO (catchException)

import GHC.CmmToLlvm.Config (LlvmVersion, llvmVersionStr, supportedLlvmVersionUpperBound, parseLlvmVersion, supportedLlvmVersionLowerBound)

import GHC.Settings

import GHC.SysTools.Process
import GHC.SysTools.Info

import GHC.Driver.Session

import GHC.Utils.Exception as Exception
import GHC.Utils.Error
import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Logger
import GHC.Utils.TmpFs
import GHC.Utils.Constants (isWindowsHost)
import GHC.Utils.Panic

import Data.List (tails, isPrefixOf)
import Data.Maybe (fromMaybe)
import System.IO
import System.Process

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

runCpp :: Logger -> DynFlags -> [Option] -> IO ()
runCpp logger dflags args = traceSystoolCommand logger "cpp" $ do
  let opts = getOpts dflags opt_P
      modified_imports = augmentImports dflags opts
  let (p,args0) = pgm_P dflags
      args1 = map Option modified_imports
      args2 = [Option "-Werror" | gopt Opt_WarnIsError dflags]
                ++ [Option "-Wundef" | wopt Opt_WarnCPPUndef dflags]
  mb_env <- getGccEnv args2
  runSomethingFiltered logger id  "C pre-processor" p
                       (args0 ++ args1 ++ args2 ++ args) Nothing mb_env

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
  runSomethingResponseFile logger tmpfs dflags cc_filter dbgstring prog args2
                           mb_env
 where
  -- discard some harmless warnings from gcc that we can't turn off
  cc_filter = unlines . doFilter . lines

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

-- | Run the clang compiler (used as an assembler for the LLVM
-- backend on OS X as LLVM doesn't support the OS X system
-- assembler)
runClang :: Logger -> DynFlags -> [Option] -> IO ()
runClang logger dflags args = traceSystoolCommand logger "clang" $ do
  let (clang,_) = pgm_lcc dflags
      -- be careful what options we call clang with
      -- see #5903 and #7617 for bugs caused by this.
      (_,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  catchException
    (runSomethingFiltered logger id "Clang (Assembler)" clang args2 Nothing mb_env)
    (\(err :: SomeException) -> do
        errorMsg logger $
            text ("Error running clang! you need clang installed to use the" ++
                  " LLVM backend") $+$
            text "(or GHC tried to execute clang incorrectly)"
        throwIO err
    )

runEmscripten :: Logger -> DynFlags -> [Option] -> IO ()
runEmscripten logger dflags args = traceSystoolCommand logger "emcc" $ do
  let (p,args0) = pgm_a dflags
      args1     = args0 ++ args
  runSomething logger "Emscripten" p args1

-- | Figure out which version of LLVM we are running this session
figureLlvmVersion :: Logger -> DynFlags -> IO (Maybe LlvmVersion)
figureLlvmVersion logger dflags = traceSystoolCommand logger "llc" $ do
  let (pgm,opts) = pgm_lc dflags
      args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  catchIO (do
              (pin, pout, perr, p) <- runInteractiveProcess pgm args'
                                              Nothing Nothing
              {- > llc -version
                  <vendor> LLVM version 15.0.7
                    ...
                  OR
                  LLVM (http://llvm.org/):
                    LLVM version 14.0.6
              -}
              hSetBinaryMode pout False
              line1 <- hGetLine pout
              mb_ver <- case parseLlvmVersion line1 of
                mb_ver@(Just _) -> return mb_ver
                Nothing -> parseLlvmVersion <$> hGetLine pout -- Try the second line
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



runLink :: Logger -> TmpFs -> DynFlags -> [Option] -> IO ()
runLink logger tmpfs dflags args = traceSystoolCommand logger "linker" $ do
  -- See Note [Run-time linker info]
  --
  -- `-optl` args come at the end, so that later `-l` options
  -- given there manually can fill in symbols needed by
  -- Haskell libraries coming in via `args`.
  linkargs <- neededLinkArgs `fmap` getLinkerInfo logger dflags
  let (p,args0) = pgm_l dflags
      optl_args = map Option (getOpts dflags opt_l)
      args2     = args0 ++ linkargs ++ args ++ optl_args
  mb_env <- getGccEnv args2
  runSomethingResponseFile logger tmpfs dflags ld_filter "Linker" p args2 mb_env
  where
    ld_filter = case (platformOS (targetPlatform dflags)) of
                  OSSolaris2 -> sunos_ld_filter
                  _ -> id
{-
  SunOS/Solaris ld emits harmless warning messages about unresolved
  symbols in case of compiling into shared library when we do not
  link against all the required libs. That is the case of GHC which
  does not link against RTS library explicitly in order to be able to
  choose the library later based on binary application linking
  parameters. The warnings look like:

Undefined                       first referenced
  symbol                             in file
stg_ap_n_fast                       ./T2386_Lib.o
stg_upd_frame_info                  ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_litE_closure ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_appE_closure ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_conE_closure ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziSyntax_mkNameGzud_closure ./T2386_Lib.o
newCAF                              ./T2386_Lib.o
stg_bh_upd_frame_info               ./T2386_Lib.o
stg_ap_ppp_fast                     ./T2386_Lib.o
templatezmhaskell_LanguageziHaskellziTHziLib_stringL_closure ./T2386_Lib.o
stg_ap_p_fast                       ./T2386_Lib.o
stg_ap_pp_fast                      ./T2386_Lib.o
ld: warning: symbol referencing errors

  this is actually coming from T2386 testcase. The emitting of those
  warnings is also a reason why so many TH testcases fail on Solaris.

  Following filter code is SunOS/Solaris linker specific and should
  filter out only linker warnings. Please note that the logic is a
  little bit more complex due to the simple reason that we need to preserve
  any other linker emitted messages. If there are any. Simply speaking
  if we see "Undefined" and later "ld: warning:..." then we omit all
  text between (including) the marks. Otherwise we copy the whole output.
-}
    sunos_ld_filter :: String -> String
    sunos_ld_filter = unlines . sunos_ld_filter' . lines
    sunos_ld_filter' x = if (undefined_found x && ld_warning_found x)
                          then (ld_prefix x) ++ (ld_postfix x)
                          else x
    breakStartsWith x y = break (isPrefixOf x) y
    ld_prefix = fst . breakStartsWith "Undefined"
    undefined_found = not . null . snd . breakStartsWith "Undefined"
    ld_warn_break = breakStartsWith "ld: warning: symbol referencing errors"
    ld_postfix = tail . snd . ld_warn_break
    ld_warning_found = not . null . snd . ld_warn_break

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
    -- N.B. Darwin's ld64 doesn't support response files. Consequently we only
    -- use them on Windows where they are truly necessary.
    if isWindowsHost
      then do
        mb_env <- getGccEnv args2
        runSomethingResponseFile logger tmpfs dflags id "Merge objects" p args2 mb_env
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

touch :: Logger -> DynFlags -> String -> String -> IO ()
touch logger dflags purpose arg = traceSystoolCommand logger "touch" $
  runSomething logger purpose (pgm_T dflags) [FileOption "" arg]
