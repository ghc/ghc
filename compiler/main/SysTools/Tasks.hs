{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Tasks running external programs for SysTools
--
-- (c) The GHC Team 2017
--
-----------------------------------------------------------------------------
module SysTools.Tasks where

import Exception
import ErrUtils
import HscTypes
import DynFlags
import Outputable
import GHC.Platform
import Util

import Data.List
import Data.Char
import Data.Maybe

import System.IO
import System.Process
import GhcPrelude

import LlvmCodeGen.Base (LlvmVersion, llvmVersionStr, supportedLlvmVersionLowerBound, supportedLlvmVersionUpperBound, llvmVersionStr, parseLlvmVersion)

import SysTools.Process
import SysTools.Info

import Control.Monad (join, forM, filterM, void)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.ParserCombinators.ReadP as Parser

{-
************************************************************************
*                                                                      *
\subsection{Running an external program}
*                                                                      *
************************************************************************
-}

runUnlit :: DynFlags -> [Option] -> IO ()
runUnlit dflags args = traceToolCommand dflags "unlit" $ do
  let prog = pgm_L dflags
      opts = getOpts dflags opt_L
  runSomething dflags "Literate pre-processor" prog
               (map Option opts ++ args)

runCpp :: DynFlags -> [Option] -> IO ()
runCpp dflags args = traceToolCommand dflags "cpp" $ do
  let (p,args0) = pgm_P dflags
      args1 = map Option (getOpts dflags opt_P)
      args2 = [Option "-Werror" | gopt Opt_WarnIsError dflags]
                ++ [Option "-Wundef" | wopt Opt_WarnCPPUndef dflags]
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id  "C pre-processor" p
                       (args0 ++ args1 ++ args2 ++ args) Nothing mb_env

runPp :: DynFlags -> [Option] -> IO ()
runPp dflags args = traceToolCommand dflags "pp" $ do
  let prog = pgm_F dflags
      opts = map Option (getOpts dflags opt_F)
  runSomething dflags "Haskell pre-processor" prog (args ++ opts)

-- | Run compiler of C-like languages and raw objects (such as gcc or clang).
runCc :: Maybe ForeignSrcLang -> DynFlags -> [Option] -> IO ()
runCc mLanguage dflags args = traceToolCommand dflags "cc" $ do
  let p = pgm_c dflags
      args1 = map Option userOpts
      args2 = languageOptions ++ args ++ args1
      -- We take care to pass -optc flags in args1 last to ensure that the
      -- user can override flags passed by GHC. See #14452.
  mb_env <- getGccEnv args2
  runSomethingResponseFile dflags cc_filter "C Compiler" p args2 mb_env
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
  (languageOptions, userOpts) = case mLanguage of
    Nothing -> ([], userOpts_c)
    Just language -> ([Option "-x", Option languageName], opts)
      where
        (languageName, opts) = case language of
          LangC      -> ("c",             userOpts_c)
          LangCxx    -> ("c++",           userOpts_cxx)
          LangObjc   -> ("objective-c",   userOpts_c)
          LangObjcxx -> ("objective-c++", userOpts_cxx)
          LangAsm    -> ("assembler",     [])
          RawObject  -> ("c",             []) -- claim C for lack of a better idea
  userOpts_c   = getOpts dflags opt_c
  userOpts_cxx = getOpts dflags opt_cxx

isContainedIn :: String -> String -> Bool
xs `isContainedIn` ys = any (xs `isPrefixOf`) (tails ys)

-- | Run the linker with some arguments and return the output
askLd :: DynFlags -> [Option] -> IO String
askLd dflags args = traceToolCommand dflags "linker" $ do
  let (p,args0) = pgm_l dflags
      args1     = map Option (getOpts dflags opt_l)
      args2     = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingWith dflags "gcc" p args2 $ \real_args ->
    readCreateProcessWithExitCode' (proc p real_args){ env = mb_env }

runAs :: DynFlags -> [Option] -> IO ()
runAs dflags args = traceToolCommand dflags "as" $ do
  let (p,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Assembler" p args2 Nothing mb_env

-- | Run the LLVM Optimiser
runLlvmOpt :: DynFlags -> [Option] -> IO ()
runLlvmOpt dflags args = traceToolCommand dflags "opt" $ do
  let (p,args0) = pgm_lo dflags
      args1 = map Option (getOpts dflags opt_lo)
      -- We take care to pass -optlo flags (e.g. args0) last to ensure that the
      -- user can override flags passed by GHC. See #14821.
  runSomething dflags "LLVM Optimiser" p (args1 ++ args ++ args0)

-- | Run the LLVM Compiler
runLlvmLlc :: DynFlags -> [Option] -> IO ()
runLlvmLlc dflags args = traceToolCommand dflags "llc" $ do
  let (p,args0) = pgm_lc dflags
      args1 = map Option (getOpts dflags opt_lc)
  runSomething dflags "LLVM Compiler" p (args0 ++ args1 ++ args)

-- | Run the clang compiler (used as an assembler for the LLVM
-- backend on OS X as LLVM doesn't support the OS X system
-- assembler)
runClang :: DynFlags -> [Option] -> IO ()
runClang dflags args = traceToolCommand dflags "clang" $ do
  let (clang,_) = pgm_lcc dflags
      -- be careful what options we call clang with
      -- see #5903 and #7617 for bugs caused by this.
      (_,args0) = pgm_a dflags
      args1 = map Option (getOpts dflags opt_a)
      args2 = args0 ++ args1 ++ args
  mb_env <- getGccEnv args2
  Exception.catch (do
        runSomethingFiltered dflags id "Clang (Assembler)" clang args2 Nothing mb_env
    )
    (\(err :: SomeException) -> do
        errorMsg dflags $
            text ("Error running clang! you need clang installed to use the" ++
                  " LLVM backend") $+$
            text "(or GHC tried to execute clang incorrectly)"
        throwIO err
    )

-- | Figure out which version of LLVM we are running this session
figureLlvmVersion :: DynFlags -> IO (Maybe LlvmVersion)
figureLlvmVersion dflags = traceToolCommand dflags "llc" $ do
  let (pgm,opts) = pgm_lc dflags
      args = filter notNull (map showOpt opts)
      -- we grab the args even though they should be useless just in
      -- case the user is using a customised 'llc' that requires some
      -- of the options they've specified. llc doesn't care what other
      -- options are specified when '-version' is used.
      args' = args ++ ["-version"]
  catchIO (do
              (pin, pout, perr, _) <- runInteractiveProcess pgm args'
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
              return mb_ver
            )
            (\err -> do
                debugTraceMsg dflags 2
                    (text "Error (figuring out LLVM version):" <+>
                      text (show err))
                errorMsg dflags $ vcat
                    [ text "Warning:", nest 9 $
                          text "Couldn't figure out LLVM version!" $$
                          text ("Make sure you have installed LLVM between ["
                                ++ llvmVersionStr supportedLlvmVersionLowerBound
                                ++ " and "
                                ++ llvmVersionStr supportedLlvmVersionUpperBound
                                ++ ")") ]
                return Nothing)


-- | On macOS we rely on the linkers @-dead_strip_dylibs@ flag to remove unused
-- libraries from the dynamic library.  We do this to reduce the number of load
-- commands that end up in the dylib, and has been limited to 32K (32768) since
-- macOS Sierra (10.14).
--
-- @-dead_strip_dylibs@ does not dead strip @-rpath@ entries, as such passing
-- @-l@ and @-rpath@ to the linker will result in the unnecesasry libraries not
-- being included in the load commands, however the @-rpath@ entries are all
-- forced to be included.  This can lead to 100s of @-rpath@ entries being
-- included when only a handful of libraries end up being truely linked.
--
-- Thus after building the library, we run a fixup phase where we inject the
-- @-rpath@ for each found library (in the given library search paths) into the
-- dynamic library through @-add_rpath@.
--
-- See Note [Dynamic linking on macOS]
runInjectRPaths :: DynFlags -> [FilePath] -> FilePath -> IO ()
runInjectRPaths dflags _ _ | not (gopt Opt_RPath dflags) = return ()
runInjectRPaths dflags lib_paths dylib = do
  info <- lines <$> askOtool dflags Nothing [Option "-L", Option dylib]
  -- filter the output for only the libraries. And then drop the @rpath prefix.
  let libs = fmap (drop 7) $ filter (isPrefixOf "@rpath") $ fmap (head.words) $ info
  -- find any pre-existing LC_PATH items
  info <- lines <$> askOtool dflags Nothing [Option "-l", Option dylib]

  let paths = mapMaybe get_rpath info
      lib_paths' = [ p | p <- lib_paths, not (p `elem` paths) ]
  -- only find those rpaths, that aren't already in the library.
  rpaths <- nub.sort.join <$> forM libs (\f -> filterM (\l -> doesFileExist (l </> f)) lib_paths')
  -- inject the rpaths
  case rpaths of
    [] -> return ()
    _  -> runInstallNameTool dflags $ map Option $ "-add_rpath":(intersperse "-add_rpath" rpaths) ++ [dylib]

get_rpath :: String -> Maybe FilePath
get_rpath l = case readP_to_S rpath_parser l of
                [(rpath, "")] -> Just rpath
                _ -> Nothing


rpath_parser :: ReadP FilePath
rpath_parser = do
  skipSpaces
  void $ string "path"
  void $ many1 (satisfy isSpace)
  rpath <- many get
  void $ many1 (satisfy isSpace)
  void $ string "(offset "
  void $ munch1 isDigit
  void $ Parser.char ')'
  skipSpaces
  return rpath

runLink :: DynFlags -> [Option] -> IO ()
runLink dflags args = traceToolCommand dflags "linker" $ do
  -- See Note [Run-time linker info]
  --
  -- `-optl` args come at the end, so that later `-l` options
  -- given there manually can fill in symbols needed by
  -- Haskell libaries coming in via `args`.
  linkargs <- neededLinkArgs `fmap` getLinkerInfo dflags
  let (p,args0) = pgm_l dflags
      optl_args = map Option (getOpts dflags opt_l)
      args2     = args0 ++ linkargs ++ args ++ optl_args
  mb_env <- getGccEnv args2
  runSomethingResponseFile dflags ld_filter "Linker" p args2 mb_env
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
runMergeObjects :: DynFlags -> [Option] -> IO ()
runMergeObjects dflags args = traceToolCommand dflags "merge-objects" $ do
  let (p,args0) = pgm_lm dflags
      optl_args = map Option (getOpts dflags opt_lm)
      args2     = args0 ++ args ++ optl_args
  -- N.B. Darwin's ld64 doesn't support response files. Consequently we only
  -- use them on Windows where they are truly necessary.
#if defined(mingw32_HOST_OS)
  mb_env <- getGccEnv args2
  runSomethingResponseFile dflags id "Merge objects" p args2 mb_env
#else
  runSomething dflags "Merge objects" p args2
#endif

runLibtool :: DynFlags -> [Option] -> IO ()
runLibtool dflags args = traceToolCommand dflags "libtool" $ do
  linkargs <- neededLinkArgs `fmap` getLinkerInfo dflags
  let args1      = map Option (getOpts dflags opt_l)
      args2      = [Option "-static"] ++ args1 ++ args ++ linkargs
      libtool    = pgm_libtool dflags
  mb_env <- getGccEnv args2
  runSomethingFiltered dflags id "Linker" libtool args2 Nothing mb_env

runAr :: DynFlags -> Maybe FilePath -> [Option] -> IO ()
runAr dflags cwd args = traceToolCommand dflags "ar" $ do
  let ar = pgm_ar dflags
  runSomethingFiltered dflags id "Ar" ar args cwd Nothing

askAr :: DynFlags -> Maybe FilePath -> [Option] -> IO String
askAr dflags mb_cwd args = traceToolCommand dflags "ar" $ do
  let ar = pgm_ar dflags
  runSomethingWith dflags "Ar" ar args $ \real_args ->
    readCreateProcessWithExitCode' (proc ar real_args){ cwd = mb_cwd }

askOtool :: DynFlags -> Maybe FilePath -> [Option] -> IO String
askOtool dflags mb_cwd args = do
  let otool = pgm_otool dflags
  runSomethingWith dflags "otool" otool args $ \real_args ->
    readCreateProcessWithExitCode' (proc otool real_args){ cwd = mb_cwd }

runInstallNameTool :: DynFlags -> [Option] -> IO ()
runInstallNameTool dflags args = do
  let tool = pgm_install_name_tool dflags
  runSomethingFiltered dflags id "Install Name Tool" tool args Nothing Nothing

runRanlib :: DynFlags -> [Option] -> IO ()
runRanlib dflags args = traceToolCommand dflags "ranlib" $ do
  let ranlib = pgm_ranlib dflags
  runSomethingFiltered dflags id "Ranlib" ranlib args Nothing Nothing

runMkDLL :: DynFlags -> [Option] -> IO ()
runMkDLL dflags args = traceToolCommand dflags "mkdll" $ do
  let (p,args0) = pgm_dll dflags
      args1 = args0 ++ args
  mb_env <- getGccEnv (args0++args)
  runSomethingFiltered dflags id "Make DLL" p args1 Nothing mb_env

runWindres :: DynFlags -> [Option] -> IO ()
runWindres dflags args = traceToolCommand dflags "windres" $ do
  let cc = pgm_c dflags
      cc_args = map Option (sOpt_c (settings dflags))
      windres = pgm_windres dflags
      opts = map Option (getOpts dflags opt_windres)
      quote x = "\"" ++ x ++ "\""
      args' = -- If windres.exe and gcc.exe are in a directory containing
              -- spaces then windres fails to run gcc. We therefore need
              -- to tell it what command to use...
              Option ("--preprocessor=" ++
                      unwords (map quote (cc :
                                          map showOpt opts ++
                                          ["-E", "-xc", "-DRC_INVOKED"])))
              -- ...but if we do that then if windres calls popen then
              -- it can't understand the quoting, so we have to use
              -- --use-temp-file so that it interprets it correctly.
              -- See #1828.
            : Option "--use-temp-file"
            : args
  mb_env <- getGccEnv cc_args
  runSomethingFiltered dflags id "Windres" windres args' Nothing mb_env

touch :: DynFlags -> String -> String -> IO ()
touch dflags purpose arg = traceToolCommand dflags "touch" $
  runSomething dflags purpose (pgm_T dflags) [FileOption "" arg]

-- * Tracing utility

-- | Record in the eventlog when the given tool command starts
--   and finishes, prepending the given 'String' with
--   \"systool:\", to easily be able to collect and process
--   all the systool events.
--
--   For those events to show up in the eventlog, you need
--   to run GHC with @-v2@ or @-ddump-timings@.
traceToolCommand :: DynFlags -> String -> IO a -> IO a
traceToolCommand dflags tool = withTiming
  dflags (text $ "systool:" ++ tool) (const ())
