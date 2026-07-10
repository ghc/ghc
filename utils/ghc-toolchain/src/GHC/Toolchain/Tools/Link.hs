{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}

module GHC.Toolchain.Tools.Link ( CcLink(..), findCcLink ) where

import Control.Monad (when)
import Data.List (isInfixOf)
import System.FilePath

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Ar
import GHC.Toolchain.Tools.Ranlib
import GHC.Toolchain.Tools.Readelf
import System.Exit (ExitCode(..))
import Control.Applicative

-- | Configuration on how the C compiler can be used to link
data CcLink = CcLink { ccLinkProgram :: Program
                     , ccLinkSupportsNoPie :: Bool -- See Note [No PIE when linking] in GHC.Driver.Session
                     , ccLinkSupportsCompactUnwind :: Bool
                     , ccLinkSupportsFilelist :: Bool
                     , ccLinkSupportsSingleModule :: Bool
                     , ccLinkIsGnu :: Bool
                     , ccLinkSupportsVerbatimNamespace :: Bool
                     }
    deriving (Read, Eq, Ord)

-- These instances are more suitable for diffing
instance Show CcLink where
  show CcLink{..} = unlines
    [ "CcLink"
    , "{ ccLinkProgram = " ++ show ccLinkProgram
    , ", ccLinkSupportsNoPie = " ++ show ccLinkSupportsNoPie
    , ", ccLinkSupportsCompactUnwind = " ++ show ccLinkSupportsCompactUnwind
    , ", ccLinkSupportsFilelist = " ++ show ccLinkSupportsFilelist
    , ", ccLinkSupportsSingleModule = " ++ show ccLinkSupportsSingleModule
    , ", ccLinkIsGnu = " ++ show ccLinkIsGnu
    , ", ccLinkSupportsVerbatimNamespace = " ++ show ccLinkSupportsVerbatimNamespace
    , "}"
    ]

_ccLinkProgram :: Lens CcLink Program
_ccLinkProgram = Lens ccLinkProgram (\x o -> o{ccLinkProgram=x})

-- | tries to add flags to the c compiler to run as a linker
findCcLink :: String  -- ^ The llvm target to use if CcLink supports --target
           -> ProgOpt -- ^ The contents of $LD
           -> ProgOpt -- ^ The c compiler intended for invoking the linker
           -> Bool    -- ^ Whether GHC should disregard ld and search for a linker it considers better
           -> ArchOS
           -> Cc      -- ^ the c compiler used for compiling c programs, used as a fallback
           -> Maybe Readelf -> Ar -> Ranlib -> M CcLink
findCcLink target ld progOpt userLdOverride archOs cc readelf ar ranlib = checking "for C compiler for linking command" $ do
  -- Use the specified linker or try using the C compiler
  rawCcLink <- findProgram "C compiler for linking" progOpt [] <|> pure (programFromOpt progOpt (prgPath $ ccProgram cc) [])
  ccLinkProgram <- if
    -- A cc with linker was already specified, don't doubt the user's proficiency,
    -- autonomy and right to segmentation faults
    | Just _ <- progOpt.poFlags -> pure rawCcLink
    -- we get a $LD; now figure out how to pass it to the $CC
    | Just ldPathOrFlavour <- ld.poPath
    -- even though $LD=ld doesn't really mean anything to C compilers,
    -- we are lenient and just act as if $LD wasn't set and enable-ld-override
    -- is off
    , ldPathOrFlavour /= "ld" -> do
        flip oneOf'

          [ do
            -- first check if the c compiler supports --ld-path, which goes best with $LD
            ldPath <- findProgram (ldPathOrFlavour <> " linker") emptyProgOpt $  tryLdPrefix [ldPathOrFlavour]
            -- findProgram takes a userSpec but in our case this is emptyProgOpt, so we just
            -- extract the path that it found and ignore the (empty) arguments
            checkLink rawCcLink [ fLdPath ldPath.prgPath ]
          , -- second, check if the c compiler instead understands -fuse-ld (linker "flavour");
            -- do not try to expand the path first since absolute paths are not valid linker flavours
            checkLink rawCcLink [ fUseLd ldPathOrFlavour] ]

          -- $LD was set but we couldn't figure out how to pass it to $CC. This is a hard failure,
          -- we report it.
          [ ldPathOrFlavour <> " is an invalid linker."
          , "$LD can be either of an executable name that is in $PATH *or* a"
            <> " path to an executable."
          , "If your C compiler only supports the '-fuse-ld' flag, $LD can"
            <> " only be one of the linker flavours supported by it."
          , "Mind that if your C compiler supports '--ld-path', 'configure'"
            <> " will always prefer using an absolute path to your linker as"
            <> " that is less error-prone." ]

    -- $LD is not set or $LD=ld (see above)
    -- Try to convince @cc@ to use a more efficient linker than @bfd.ld@
    | let ldOverride
            | Just "ld" <- ld.poPath = False
            | otherwise = userLdOverride
    , ldOverride ->
       asum
         [ -- Annoyingly, gcc silently falls back to vanilla ld
           -- if @-fuse-ld@ is given passed a non-existent linker.
           -- Consequently, we must first check that the desired ld
           -- executable exists before trying cc.
           do linkerPath <- findProgram (linker ++ " linker") emptyProgOpt [ linker ]
              checkLink rawCcLink [ fLdPath linkerPath.prgPath]
                <|> checkLink rawCcLink [ fUseLd linker]
         | linker <- tryLdPrefix ["lld", "bfd"]
         ]
       -- fall back to raw ld
       <|> checkLink rawCcLink []

    -- we can't help the user
    | otherwise -> checkLink rawCcLink []

  targetedCcLink                  <- linkSupportsTarget archOs cc target ccLinkProgram
  ccLinkSupportsNoPie             <- checkSupportsNoPie  cc targetedCcLink
  ccLinkSupportsCompactUnwind     <- checkSupportsCompactUnwind archOs cc targetedCcLink
  ccLinkSupportsFilelist          <- checkSupportsFilelist cc targetedCcLink
  ccLinkSupportsSingleModule      <- checkSupportsSingleModule archOs cc targetedCcLink
  ccLinkIsGnu                     <- checkLinkIsGnu archOs targetedCcLink
  checkedCcLink                   <- addPlatformDepLinkFlags archOs cc targetedCcLink
  ccLinkSupportsVerbatimNamespace <- linkSupportsVerbatimNamespace cc ar ranlib checkedCcLink

  checkBfdCopyBug archOs cc readelf targetedCcLink

  let finalCcLink = CcLink
                    { ccLinkProgram = checkedCcLink, ccLinkSupportsNoPie
                    , ccLinkSupportsCompactUnwind, ccLinkSupportsFilelist
                    , ccLinkSupportsSingleModule, ccLinkIsGnu, ccLinkSupportsVerbatimNamespace }

  linkRequiresNoFixupChains archOs cc finalCcLink
    >>= linkRequiresNoWarnDuplicateLibraries archOs cc
  where
    checkLink ccLink extraFlags =
      let prog = over _prgFlags (extraFlags <>) ccLink
       in prog <$ checkLinkWorks cc prog
    tryLdPrefix progs =  [id, ("ld." <>)] <*> progs
    fUseLd flavour = "-fuse-ld=" <> flavour
    fLdPath path = "--ld-path=" <> path

-- | Test whether the linker supports the verbatim '-l:libfoo.a' syntax, allowing
-- us better control over partial static linking.
linkSupportsVerbatimNamespace :: Cc -> Ar -> Ranlib -> Program -> M Bool
linkSupportsVerbatimNamespace cc ar ranlib ccLink = (<|> pure False) $ checking "whether cc linker supports -l:libfoo.a" $ withTempDir $ \tmpDir -> do
    let test_c  = tmpDir </> "test.c"
    writeFile test_c testLibrary
    let test_o  = tmpDir </> "test.o"
    let test_a  = tmpDir </> "libtest.a"

    let main_c = tmpDir </> "main.c"
    writeFile main_c testMain
    let main' = tmpDir </> "main"
    let err = "linker didn't produce any output"

    callProgram (ccProgram cc) ["-c", test_c, "-o", test_o]
    callProgram (arMkArchive ar) [test_a, test_o]
    when (arNeedsRanlib ar) $ callProgram (ranlibProgram ranlib) [test_a]

    callProgram ccLink [main_c, "-o", main', "-L" ++ tmpDir, "-l:libtest.a"]
    expectFileExists main' err
      -- Linking in windows might produce an executable with an ".exe" extension
      <|> expectFileExists (main' <.> "exe") err

    return True

  where
   testLibrary = mconcat
     ["void my_func(void) {"
     ,"/* A simple function to be archived */"
     , "}"
     ]
   testMain = mconcat
     ["void my_func(void);"
     ,"int main(void) {"
     , "  my_func();"
     , "  return 0;"
     , "}"
     ]


linkSupportsTarget :: ArchOS -> Cc -> String -> Program -> M Program
-- Javascript toolchain provided by emsdk just ignores --target flag so
-- we have this special case to match with ./configure (#23744)
linkSupportsTarget archOs cc target link =
    checking "whether cc linker supports --target" $
    supportsTarget archOs (Lens id const) (checkLinkWorks cc) target link

-- | See Note [No PIE when linking] in GHC.Driver.Session
checkSupportsNoPie :: Cc -> Program -> M Bool
checkSupportsNoPie cc ccLink = checking "whether the cc linker supports -no-pie" $
  withTempDir $ \dir -> do
    let test_o  = dir </> "test.o"
    let test = dir </> "test"
    compileC cc test_o "int main() { return 0; }"
    -- Check output as some GCC versions only warn and don't respect -Werror
    -- when passed an unrecognized flag.
    (code, out, err) <- readProgram ccLink ["-no-pie", "-Werror", test_o, "-o", test]
    return if
      | ExitSuccess <- code
      , not ("unrecognized" `isInfixOf` out)
      , not ("unrecognized" `isInfixOf` err) -> True
      | otherwise -> False

-- ROMES:TODO: This check is wrong here and in configure because with ld.gold parses "-n" "o_compact_unwind"
-- TODO:
-- * Check if compiling for darwin
-- * Then do the check
-- * Otherwise say its just not supported
checkSupportsCompactUnwind :: ArchOS -> Cc -> Program -> M Bool
checkSupportsCompactUnwind archOs cc ccLink
  | OSDarwin <- archOS_OS archOs = checking "whether the cc linker understands -no_compact_unwind" $
      withTempDir $ \dir -> do
        let test_o  = dir </> "test.o"
            test2_o = dir </> "test2.o"

        compileC cc test_o "int foo() { return 0; }"

        exitCode <- runProgram ccLink ["-r", "-Wl,-no_compact_unwind", "-o", test2_o, test_o]
        return if | ExitSuccess <- exitCode -> True
                  | otherwise -> False
  | otherwise = return False

checkSupportsFilelist :: Cc -> Program -> M Bool
checkSupportsFilelist cc ccLink = checking "whether the cc linker understands -filelist" $
  withTempDir $ \dir -> do
    let test_o   = dir </> "test.o"
        test1_o  = dir </> "test1.o"
        test2_o  = dir </> "test2.o"
        test_ofiles = dir </> "test.o-files"

    compileC cc test1_o "int foo() { return 0; }"
    compileC cc test2_o "int bar() { return 0; }"

    --  write the filenames test1_o and test2_o to the test_ofiles file
    writeFile  test_ofiles (unlines [test1_o,test2_o])

    exitCode <- runProgram ccLink ["-r", "-Wl,-filelist", test_ofiles, "-o", test_o]

    return if | ExitSuccess <- exitCode -> True
              | otherwise -> False

-- | Check that the (darwin) linker supports @-single_module@.
--
-- In XCode 15, the linker warns when @-single_module@ is passed as the flag
-- became the default and is now obsolete to pass.
--
-- We assume non-darwin linkers don't support this flag.
checkSupportsSingleModule :: ArchOS -> Cc -> Program -> M Bool
checkSupportsSingleModule archOs cc link
  | ArchOS _ OSDarwin <- archOs
  = checking "whether the darwin linker supports -single_module" $ do
      withTempDir $ \dir -> do
        let test_dylib = dir </> "test.dylib"
            test_c     = dir </> "test.c"
            testmain_o = dir </> "testmain.o"
            testmain   = dir </> "testmain"

        -- Main
        compileC cc testmain_o "extern int foo(int); int main() { return foo(5); }"

        -- Dynamic library
        writeFile test_c "int foo(int x) { return x*x; }"
        _ <- runProgram (ccProgram cc) ["-shared", "-o", test_dylib, test_c]

        (_, out, err) <- readProgram link ["-Wl,-single_module", "-o", testmain, test_dylib, testmain_o]

        return $ not $ "obsolete" `isInfixOf` err || "obsolete" `isInfixOf` out
  | otherwise
  = return False

-- | Check whether linking works.
checkLinkWorks :: Cc -> Program -> M ()
checkLinkWorks cc ccLink = withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
        main_o = dir </> "main.o"
    compileC cc test_o "int f(int a) { return 2*a; }"
    compileC cc main_o "int f(int a); int main(int argc, char **argv) { return f(0); }"

    let out = dir </> "test"
        err = "linker didn't produce any output"
    callProgram ccLink ["-Werror", "-o", out, test_o, main_o]
    expectFileExists out err
      -- Linking in windows might produce an executable with an ".exe" extension
      <|> expectFileExists (out <.> "exe") err

checkLinkIsGnu :: ArchOS -> Program -> M Bool
checkLinkIsGnu archOs _
  -- emsdk is never going to provide gnu ld (See #23744)
  | ArchJavaScript <- archOS_arch archOs = return False
checkLinkIsGnu _ ccLink = do
  out <- readProgramStdout ccLink ["-Wl,--version"]
  return ("GNU" `isInfixOf` out)

-- | Check for binutils bug #16177 present in some versions of the bfd ld
-- implementation affecting ARM relocations.
-- https://sourceware.org/bugzilla/show_bug.cgi?id=16177
checkBfdCopyBug :: ArchOS -> Cc -> Maybe Readelf -> Program -> M ()
checkBfdCopyBug archOs cc mb_readelf ccLink
  | ArchARM{} <- archOS_arch archOs =
    checking "whether linker is affected by binutils #16177" $ withTempDir $ \dir -> do
    readelf <- case mb_readelf of
      Just x -> return x
      Nothing -> throwE "readelf needed to check for binutils #16177 but not found. Please set --readelf (and --readelf-opts as necessary)."

    let test_o = dir </> "test.o"
        lib_o = dir </> "lib.o"
        lib_so = dir </> "lib.so"
        main_o = dir </> "main.o"
        exe = dir </> "exe"

    compileAsm cc lib_o progLib
    callProgram ccLink ["-shared", lib_o, "-o", lib_so]

    compileC cc main_o progMain
    compileAsm cc test_o progTest

    callProgram ccLink ["-o", exe, test_o, main_o, lib_so]

    out <- readProgramStdout (readelfProgram readelf) ["-r", exe]
    when ("R_ARM_COPY" `isInfixOf` out) $
        throwE "Your linker is affected by binutils #16177. Please choose a different linker."

  | otherwise = return ()

  where
    progTest = unlines
        [ ".data"
        , "  .globl data_object"
        , "object_reference:"
        , "  .long data_object"
        , "  .size object_reference, 4"
        ]

    progLib = unlines
        [ "  .data"
        , "  .globl data_object"
        , "  .type data_object, %object"
        , "  .size data_object, 4"
        , "data_object:"
        , "    .long 123"
        ]

    progMain =
        "int main(int argc, char **argv) { return 0; }"

{- Note [ELF needed shared libs]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Some distributions change the link editor's default handling of
ELF DT_NEEDED tags to include only those shared objects that are
needed to resolve undefined symbols. For Template Haskell we need
the last temporary shared library also if it is not needed for the
currently linked temporary shared library. We specify --no-as-needed
to override the default. This flag exists in GNU ld and GNU gold.
See #10110.

The flag is only needed on ELF systems. On Windows (PE) and Mac OS X
(Mach-O) the flag is not needed.
-}

-- | Add various platform-dependent flags needed for reliable linking.
addPlatformDepLinkFlags :: ArchOS -> Cc -> Program -> M Program
addPlatformDepLinkFlags archOs cc ccLink0 = do
  ccLink1 <- addNoAsNeeded archOs cc ccLink0
  ccLink2 <- addOSMinGW32CcFlags archOs cc ccLink1
  -- As per FPTOOLS_SET_C_LD_FLAGS
  case archOs of
    -- ROMES:TODO: Consider dropping this alongside other configuration for solaris that was dropped
    ArchOS ArchX86_64 OSSolaris2 ->
      -- Solaris is a multi-lib platform, providing both 32- and 64-bit
      -- user-land. It appears to default to 32-bit builds but we of course want to
      -- compile for 64-bits on x86-64.
      --
      -- On OpenSolaris uses gnu ld whereas SmartOS appears to use the Solaris
      -- implementation, which rather uses the -64 flag.
      return $ ccLink2 & _prgFlags %++ "-m64"
    ArchOS ArchAlpha _ ->
      -- For now, to suppress the gcc warning "call-clobbered
      -- register used for global register variable", we simply
      -- disable all warnings altogether using the -w flag. Oh well.
      return $ ccLink2 & over _prgFlags (++["-w","-mieee","-D_REENTRANT"])
    -- ArchOS ArchHPPA? _ ->
    ArchOS ArchARM{} OSFreeBSD ->
      -- On arm/freebsd, tell gcc to generate Arm
      -- instructions (ie not Thumb).
      return $ ccLink2 & _prgFlags %++ "-Wl,-z,noexecstack"
    ArchOS ArchARM{} OSLinux ->
      -- On arm/linux and arm/android, tell gcc to generate Arm
      -- instructions (ie not Thumb).
      return $ ccLink2 & _prgFlags %++ "-Wl,-z,noexecstack"
    ArchOS ArchAArch64 OSFreeBSD ->
      return $ ccLink2 & _prgFlags %++ "-Wl,-z,noexecstack"
    ArchOS ArchAArch64 OSLinux ->
      return $ ccLink2 & _prgFlags %++ "-Wl,-z,noexecstack"
    ArchOS ArchAArch64 OSNetBSD ->
      return $ ccLink2 & _prgFlags %++ "-Wl,-z,noexecstack"
    ArchOS ArchPPC OSAIX ->
      -- We need `-D_THREAD_SAFE` to unlock the thread-local `errno`.
      return $ ccLink2 & over _prgFlags (++["-D_THREAD_SAFE","-Wl,-bnotextro"])
    _ ->
      return ccLink2

-- | Adds flags specific to mingw32
addOSMinGW32CcFlags :: ArchOS -> Cc -> Program -> M Program
addOSMinGW32CcFlags archOs cc link
  | ArchOS _ OSMinGW32 <- archOs = do
      checkFStackCheck cc link <|> throwE "Windows requires -fstack-check support yet the C compiler linker appears not to support it"
  | otherwise = return link

-- | Check that @cc@ supports @-fstack-check@.
-- See Note [Windows stack allocations].
checkFStackCheck :: Cc -> Program -> M Program
checkFStackCheck cc link = checking "that -fstack-check works" $ do
      let link' = link & _prgFlags %++ "-fstack-check"
      checkLinkWorks cc link'
      return link'

-- | See Note [ELF needed shared libs]
addNoAsNeeded :: ArchOS -> Cc -> Program -> M Program
addNoAsNeeded archOs cc ccLink
  | os <- archOS_OS archOs
  , osElfTarget os
  = checking "that --no-as-needed works" $ do
      let ccLink' = over _prgFlags (++["-Wl,--no-as-needed"]) ccLink
      checkLinkWorks cc ccLink'
      return ccLink'
  | otherwise = return ccLink

-- | See if whether we are using a version of ld64 on darwin platforms which
-- requires us to pass -no_fixup_chains
linkRequiresNoFixupChains :: ArchOS -> Cc -> CcLink -> M CcLink
linkRequiresNoFixupChains archOs cc ccLink
  | OSDarwin <- archOS_OS archOs = checking "whether CC linker requires -no_fixup_chains" $
      let ccLink' = over (_ccLinkProgram % _prgFlags) (++["-Wl,-no_fixup_chains"]) ccLink
       in (ccLink' <$ checkLinkWorks cc (ccLinkProgram ccLink')) <|> return ccLink
  | otherwise = return ccLink

-- | XCode 15 introduced a new linker which warns on duplicate libraries being
-- linked. To disable this warning, we pass -Wl,-no_warn_duplicate_libraries as
-- suggested by Brad King in CMake issue #25297.
--
-- This flag isn't necessarily available to other linkers on darwin, so we must
-- only configure it into the CC linker arguments if valid.
linkRequiresNoWarnDuplicateLibraries :: ArchOS -> Cc -> CcLink -> M CcLink
linkRequiresNoWarnDuplicateLibraries archOs cc ccLink
  | OSDarwin <- archOS_OS archOs = checking "whether CC linker requires -no_warn_duplicate_libraries" $
      let ccLink' = over (_ccLinkProgram % _prgFlags) (++["-Wl,-no_warn_duplicate_libraries"]) ccLink
       in (ccLink' <$ checkLinkWorks cc (ccLinkProgram ccLink')) <|> return ccLink
  | otherwise = return ccLink

