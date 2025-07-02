{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Toolchain.Tools.Cc
    ( Cc(..)
    , _ccProgram
    , findBasicCc
    , findCc
      -- * Helpful utilities
    , preprocess
    , compileC
    , compileAsm
    , addPlatformDepCcFlags
    , checkC99Support
    ) where

import Control.Monad
import Data.List (isInfixOf) -- Wouldn't it be better to use bytestring?
import System.FilePath

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program

newtype Cc = Cc { ccProgram :: Program
                }
    deriving (Show, Read, Eq, Ord)

_ccProgram :: Lens Cc Program
_ccProgram = Lens ccProgram (\x o -> o{ccProgram=x})

_ccFlags :: Lens Cc [String]
_ccFlags = _ccProgram % _prgFlags

-- We use this to find a minimally-functional compiler needed to call
-- parseTriple.
findBasicCc :: ProgOpt -> M Cc
findBasicCc progOpt = checking "for C compiler" $ do
    -- TODO: We keep the candidate order we had in configure, but perhaps
    -- there's a more optimal one
    ccProgram <- findProgram "C compiler" progOpt ["gcc", "clang", "cc"]
    return $ Cc{ccProgram}

findCc :: ArchOS
       -> String -- ^ The llvm target to use if Cc supports --target
       -> ProgOpt -> M Cc
findCc archOs llvmTarget progOpt = do
    cc0 <- findBasicCc progOpt
    cc1 <- ignoreUnusedArgs cc0
    cc2 <- ccSupportsTarget archOs llvmTarget cc1
    checking "whether Cc works" $ checkCcWorks cc2
    cc3 <- oneOf "cc doesn't support C99" $ map checkC99Support
        [ cc2
        , cc2 & _ccFlags %++ "-std=gnu99"
        ]
    checkCcSupportsExtraViaCFlags cc3
    return cc3

checkCcWorks :: Cc -> M ()
checkCcWorks cc = withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    compileC cc test_o $ unlines
        [ "#include <stdio.h>"
        , "int main(int argc, char **argv) {"
        , "  printf(\"hello world!\");"
        , "  return 0;"
        , "}"
        ]

-- | GHC tends to produce command-lines with unused arguments that elicit
-- warnings from Clang. Clang offers the @-Qunused-arguments@ flag to silence
-- these. See #11684.
ignoreUnusedArgs :: Cc -> M Cc
ignoreUnusedArgs cc
  | "-Qunused-arguments" `elem` (view _ccFlags cc) = return cc
  | otherwise
  = checking "for -Qunused-arguments support" $ do
      let cc' = cc & _ccFlags %++ "-Qunused-arguments"
      (cc' <$ checkCcWorks cc') <|> return cc

-- Does Cc support the --target=<triple> option? If so, we should pass it
-- whenever possible to avoid ambiguity and potential compile-time errors (e.g.
-- see #20162).
ccSupportsTarget :: ArchOS -> String -> Cc -> M Cc
ccSupportsTarget archOs target cc =
    checking "whether Cc supports --target" $
    supportsTarget archOs _ccProgram checkCcWorks target cc

checkC99Support :: Cc -> M Cc
checkC99Support cc = checking "for C99 support" $ withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    compileC cc test_o $ unlines
        [ "#include <stdio.h>"
        , "#if !defined __STDC_VERSION__ || __STDC_VERSION__ < 199901L"
        , "# error \"Compiler does not advertise C99 conformance\""
        , "#endif"
        ]
    return cc

checkCcSupportsExtraViaCFlags :: Cc -> M ()
checkCcSupportsExtraViaCFlags cc = checking "whether cc supports extra via-c flags" $ withTempDir $ \dir -> do
  let test_o = dir </> "test.o"
      test_c = test_o -<.> "c"
  writeFile test_c "int main() { return 0; }"
  (code, out, err) <- readProgram (ccProgram cc)
                                  [ "-c"
                                  , "-fwrapv", "-fno-builtin"
                                  , "-Werror", "-x", "c"
                                  , "-o", test_o, test_c]
  when (not (isSuccess code)
        || "unrecognized" `isInfixOf` out
        || "unrecognized" `isInfixOf` err
        ) $
    throwE "Your C compiler must support the -fwrapv and -fno-builtin flags"

-- | Preprocess the given program.
preprocess
    :: Cc
    -> String   -- ^ program
    -> M String -- ^ preprocessed output
preprocess cc prog = withTempDir $ \dir -> do
    let out = dir </> "test.c"
    compile "c" ["-E"] _ccProgram cc out prog
    readFile out

-- | Compile a C source file to object code.
compileC
    :: Cc       -- ^ cc
    -> FilePath -- ^ output path
    -> String   -- ^ C source
    -> M ()
compileC = compile "c" ["-c"] _ccProgram

-- | Compile an assembler source file to object code.
compileAsm
    :: Cc       -- ^ cc
    -> FilePath -- ^ output path
    -> String   -- ^ Assembler source
    -> M ()
compileAsm = compile "S" ["-c"] _ccProgram

-- | Add various platform-dependent compiler flags needed by GHC. We can't do
-- this in `findCc` since we need a 'Cc` to determine the 'ArchOS'.
addPlatformDepCcFlags :: ArchOS -> Cc -> M Cc
addPlatformDepCcFlags archOs cc0 = do
  let cc1 = addWorkaroundFor7799 archOs cc0
  -- As per FPTOOLS_SET_C_LD_FLAGS
  case archOs of
    ArchOS ArchX86 OSFreeBSD ->
      return $ cc1 & _ccFlags %++ "-march=i686"
    ArchOS ArchX86_64 OSSolaris2 ->
      -- Solaris is a multi-lib platform, providing both 32- and 64-bit
      -- user-land. It appears to default to 32-bit builds but we of course want to
      -- compile for 64-bits on x86-64.
      return $ cc1 & _ccFlags %++ "-m64"
    ArchOS ArchAlpha _ ->
      -- For now, to suppress the gcc warning "call-clobbered
      -- register used for global register variable", we simply
      -- disable all warnings altogether using the -w flag. Oh well.
      return $ cc1 & over _ccFlags (++["-w","-mieee","-D_REENTRANT"])
    -- ArchOS ArchHPPA? _ ->
    ArchOS ArchARM{} OSFreeBSD ->
      -- On arm/freebsd, tell gcc to generate Arm
      -- instructions (ie not Thumb).
      return $ cc1 & _ccFlags %++ "-marm"
    ArchOS ArchARM{} OSLinux ->
      -- On arm/linux and arm/android, tell gcc to generate Arm
      -- instructions (ie not Thumb).
      return $ cc1 & _ccFlags %++ "-marm"
    ArchOS ArchPPC OSAIX ->
      -- We need `-D_THREAD_SAFE` to unlock the thread-local `errno`.
      return $ cc1 & _ccFlags %++ "-D_THREAD_SAFE"
    ArchOS ArchLoongArch64 OSLinux ->
      -- On LoongArch64, we need `-mcmodel=medium` to tell gcc to generate big
      -- enough jump instruction.
      return $ cc1 & _ccFlags %++ "-mcmodel=medium"
    ArchOS ArchRISCV64 _ ->
      return $ cc1 & _ccFlags %++ "-march=rv64gcv"
    _ ->
      return cc1


-- | Workaround for #7799
addWorkaroundFor7799 :: ArchOS -> Cc -> Cc
addWorkaroundFor7799 archOs cc
  | ArchX86 <- archOS_arch archOs = cc & _ccFlags %++ "-U__i686"
  | otherwise = cc
