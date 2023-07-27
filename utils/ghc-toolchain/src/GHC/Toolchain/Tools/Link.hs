{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module GHC.Toolchain.Tools.Link ( CcLink(..), findCcLink ) where

import Control.Monad (when)
import Data.List (isInfixOf)
import System.FilePath

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Readelf

-- | Configuration on how the C compiler can be used to link
data CcLink = CcLink { ccLinkProgram :: Program
                     , ccLinkSupportsNoPie :: Bool -- See Note [No PIE when linking] in GHC.Driver.Session
                     , ccLinkSupportsCompactUnwind :: Bool
                     , ccLinkSupportsFilelist :: Bool
                     , ccLinkIsGnu :: Bool
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
    , ", ccLinkIsGnu = " ++ show ccLinkIsGnu
    , "}"
    ]

_ccLinkProgram :: Lens CcLink Program
_ccLinkProgram = Lens ccLinkProgram (\x o -> o{ccLinkProgram=x})

findCcLink :: String -- ^ The llvm target to use if CcLink supports --target
           -> ProgOpt
           -> Bool   -- ^ Whether we should search for a more efficient linker
           -> ArchOS -> Cc -> Maybe Readelf -> M CcLink
findCcLink target progOpt ldOverride archOs cc readelf = checking "for C compiler for linking command" $ do
  -- Use the specified linker or try to find one
  rawCcLink <- findProgram "C compiler for linking" progOpt [takeFileName $ prgPath $ ccProgram cc]
  ccLinkProgram <- case poFlags progOpt of
                     Just _ ->
                         -- If the user specified linker flags don't second-guess them
                         pure rawCcLink
                     Nothing -> do
                         -- If not then try to find decent linker flags
                         findLinkFlags ldOverride cc rawCcLink <|> pure rawCcLink
  ccLinkProgram <- linkSupportsTarget cc target ccLinkProgram
  ccLinkSupportsNoPie         <- checkSupportsNoPie            ccLinkProgram
  ccLinkSupportsCompactUnwind <- checkSupportsCompactUnwind archOs cc ccLinkProgram
  ccLinkSupportsFilelist      <- checkSupportsFilelist      cc ccLinkProgram
  ccLinkIsGnu                 <- checkLinkIsGnu                ccLinkProgram
  checkBfdCopyBug archOs cc readelf ccLinkProgram
  ccLinkProgram <- addPlatformDepLinkFlags archOs cc ccLinkProgram
  let ccLink = CcLink {ccLinkProgram, ccLinkSupportsNoPie,
                       ccLinkSupportsCompactUnwind, ccLinkSupportsFilelist,
                       ccLinkIsGnu}
  ccLink <- linkRequiresNoFixupChains archOs cc ccLink
  return ccLink


-- | Try to convince @cc@ to use a more efficient linker than @bfd.ld@
findLinkFlags :: Bool -> Cc -> Program -> M Program
findLinkFlags enableOverride cc ccLink
  | enableOverride && doLinkerSearch =
    oneOf "this can't happen"
        [ -- Annoyingly, gcc silently falls back to vanilla ld (typically bfd
          -- ld) if @-fuse-ld@ is given with a non-existent linker.
          -- Consequently, we must first check that the desired ld
          -- executable exists before trying cc.
          do _ <- findProgram (linker ++ " linker") emptyProgOpt ["ld."++linker]
             prog <$ checkLinkWorks cc prog
        | linker <- ["lld", "gold"]
        , let prog = over _prgFlags (++["-fuse-ld="++linker]) ccLink
        ]
        <|> (ccLink <$ checkLinkWorks cc ccLink)
  | otherwise =
    return ccLink

linkSupportsTarget :: Cc -> String -> Program -> M Program
linkSupportsTarget cc target link
  = checking "whether cc linker supports --target" $
    supportsTarget (Lens id const) (checkLinkWorks cc) target link

-- | Should we attempt to find a more efficient linker on this platform?
--
-- N.B. On Darwin it is quite important that we use the system linker
-- unchanged as it is very easy to run into broken setups (e.g. unholy mixtures
-- of Homebrew and the Apple toolchain).
--
-- See #21712.
doLinkerSearch :: Bool
#if defined(linux_HOST_OS)
doLinkerSearch = True
#else
doLinkerSearch = False
#endif

-- | See Note [No PIE when linking] in GHC.Driver.Session
checkSupportsNoPie :: Program -> M Bool
checkSupportsNoPie ccLink = checking "whether the cc linker supports -no-pie" $
  withTempDir $ \dir -> do
    let test_c = dir </> "test.c"
    writeFile test_c "int main() { return 0; }"

    let test = dir </> "test"
    -- Check output as some GCC versions only warn and don't respect -Werror
    -- when passed an unrecognized flag.
    (code, out, err) <- readProgram ccLink ["-no-pie", "-Werror", "-x", "c", test_c, "-o", test]
    return (isSuccess code && not ("unrecognized" `isInfixOf` out) && not ("unrecognized" `isInfixOf` err))

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
        return $ isSuccess exitCode
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

    return (isSuccess exitCode)

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

checkLinkIsGnu :: Program -> M Bool
checkLinkIsGnu ccLink = do
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

-- See if whether we are using a version of ld64 on darwin platforms which
-- requires us to pass -no_fixup_chains
linkRequiresNoFixupChains :: ArchOS -> Cc -> CcLink -> M CcLink
linkRequiresNoFixupChains archOs cc ccLink
  | OSDarwin <- archOS_OS archOs = checking "whether CC linker requires -no_fixup_chains" $
      let ccLink' = over (_ccLinkProgram % _prgFlags) (++["-Wl,-no_fixup_chains"]) ccLink
       in (ccLink' <$ checkLinkWorks cc (ccLinkProgram ccLink')) <|> return ccLink
  | otherwise = return ccLink


