{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module GHC.Toolchain.Tools.Link ( CcLink(..), findCcLink ) where

import Control.Monad (when)
import Data.List
import System.FilePath

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Readelf

-- | Configuration on how the C compiler can be used to link
data CcLink = CcLink { ccLinkProgram :: Program
                     , ccLinkSupportsNoPie :: Bool
                     }
    deriving (Show, Read)

findCcLink :: ProgOpt -> ArchOS -> Cc -> Maybe Readelf -> M CcLink
findCcLink progOpt archOs cc readelf = checking "for C compiler for linking command" $ do
    ccLinkProgram <- case poPath progOpt of
        Just _ ->
            -- If the user specified a linker don't second-guess them
            findProgram "C compiler for linking" progOpt []
        Nothing -> do
            -- If not then try to find a decent linker on our own
            rawCcLink <- findProgram "C compiler for linking" progOpt [prgPath $ ccProgram cc]
            findLinkFlags cc rawCcLink <|> pure rawCcLink
    ccLinkSupportsNoPie <- checkSupportsNoPie ccLinkProgram
    checkBfdCopyBug archOs cc readelf ccLinkProgram
    ccLinkProgram <- addPlatformDepLinkFlags archOs cc ccLinkProgram
    return $ CcLink {ccLinkProgram, ccLinkSupportsNoPie}

-- | Try to convince @cc@ to use a more efficient linker than @bfd.ld@
findLinkFlags :: Cc -> Program -> M Program
findLinkFlags cc ccLink
  | doLinkerSearch =
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

checkSupportsNoPie :: Program -> M Bool
checkSupportsNoPie ccLink = withTempDir $ \dir -> do
    let test_c = dir </> "test.o"
    writeFile test_c "int main() { return 0; }"

    let test = dir </> "test"
    -- Check output as some GCC versions only warn and don't respect -Werror
    -- when passed an unrecognized flag.
    out <- readProgram ccLink ["-no-pie", "-Werror", "-x", "c", test_c, "-o", test]
    if "unrecognized" `isInfixOf` out
      then return False
      else return True

-- | Check whether linking works.
checkLinkWorks :: Cc -> Program -> M ()
checkLinkWorks cc ccLink = withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
        main_o = dir </> "main.o"
    compileC cc test_o "int f(int a) { return 2*a; }"
    compileC cc main_o "int f(int a); int main(int argc, char **argv) { return f(0); }"

    let out = dir </> "test"
    callProgram ccLink ["-o", out, test_o, main_o]
    expectFileExists out "linker didn't produce any output"

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

    out <- readProgram (readelfProgram readelf) ["-r", exe]
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
addPlatformDepLinkFlags archOs cc ccLink
  | OSLinux <- archOS_OS archOs = checking "that --no-as-needed works" $ do
      -- | See Note [ELF needed shared libs]
      let ccLink' = over _prgFlags (++["-Wl,--no-as-needed"]) ccLink
      checkLinkWorks cc ccLink'
      return ccLink'

  | otherwise = return ccLink
