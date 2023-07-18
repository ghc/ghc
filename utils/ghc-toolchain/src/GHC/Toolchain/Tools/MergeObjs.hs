{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module GHC.Toolchain.Tools.MergeObjs ( MergeObjs(..), findMergeObjs ) where

import Control.Monad
import Data.List (isInfixOf)
import System.FilePath

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.Nm

-- | Configuration on how the C compiler can be used to link
data MergeObjs = MergeObjs { mergeObjsProgram :: Program
                           , mergeObjsSupportsResponseFiles :: Bool
                           }
    deriving (Show, Read, Eq, Ord)

findMergeObjs :: ProgOpt -> Cc -> CcLink -> Nm -> M MergeObjs
findMergeObjs progOpt cc ccLink nm = checking "for linker for merging objects" $ do
    prog <- findProgram "linker for merging objects" progOpt ["ld.gold", "ld"]
    let mo = prog & _prgFlags %++ "-r"
    checkMergingWorks cc nm mo
    checkForGoldT22266 cc ccLink mo
    supportsResponseFiles <- checkSupportsResponseFiles cc nm mo
    return (MergeObjs mo supportsResponseFiles)

checkMergingWorks :: Cc -> Nm -> Program -> M ()
checkMergingWorks cc nm mergeObjs =
    checking "whether object merging works" $ withTempDir $ \dir -> do
        let fo s = dir </> s <.> "o"
        compileC cc (fo "a") "int funA(int x) { return x; }"
        compileC cc (fo "b") "int funB(int x) { return x; }"
        callProgram mergeObjs [fo "a", fo "b", "-o", fo "out"]
        out <- readProgramStdout (nmProgram nm) [fo "out"]
        let ok = all (`isInfixOf` out) ["funA", "funB"]
        unless ok $ throwE "merged objects is missing symbols"

checkSupportsResponseFiles :: Cc -> Nm -> Program -> M Bool
checkSupportsResponseFiles cc nm mergeObjs = checking "whether the merge objects tool supports response files" $
  withTempDir $ \dir -> do
    -- Like 'checkMergingWorks', but pass the arguments in a response file
    let fo s     = dir </> s <.> "o"
        args_txt = dir </> "args.txt"
    compileC cc (fo "a") "int funA(int x) { return x; }"
    compileC cc (fo "b") "int funB(int x) { return x; }"
    writeFile args_txt (unlines [fo "a", fo "b", "-o", fo "out"])
    callProgram mergeObjs ["@" <> args_txt]
    out <- readProgramStdout (nmProgram nm) [fo "out"]
    return $ all (`isInfixOf` out) ["funA", "funB"]

-- Test for binutils #22266. This bug manifested as GHC bug #14328 (see also:
-- #14675, #14291).
-- Uses test from
-- https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;h=033bfb739b525703bfe23f151d09e9beee3a2afe
checkForGoldT22266 :: Cc -> CcLink -> Program -> M ()
checkForGoldT22266 cc ccLink mergeObjs = do
    version <- checking "for ld.gold object merging bug (binutils #22266)" $
        readProgramStdout mergeObjs ["--version"]
    when ("gold" `isInfixOf` version) check_it
  where
    check_it =
        checking "for ld.gold object merging bug (binutils #22266)" $
        ifCrossCompiling (logInfo "Cross-compiling, assuming linker is unaffected") $
        withTempDir $ \dir -> do
            let f s = dir </> s
                link_script = f "link.t"
                a_o = f "a.o"
                merged_o = f "merged.o"
                main_o = f "main.o"
                exe = f "main"
            compileC cc a_o progA
            writeFile link_script ldScript
            callProgram mergeObjs
                ["-T", link_script, a_o, "-o", merged_o]
            compileC cc main_o progMain
            callProgram (ccLinkProgram ccLink)
                ["-o", exe, merged_o, main_o]
            callProgram (Program exe []) []

    progA = unlines
        [ "__attribute__((section(\".data.a\")))"
        , "static int int_from_a_1 = 0x11223344;"
        , ""
        , "__attribute__((section(\".data.rel.ro.a\")))"
        , "int *p_int_from_a_2 = &int_from_a_1;"
        , ""
        , "const char *hello (void);"
        , ""
        , "const char * hello (void)"
        , "{ return \"XXXHello, world!\" + 3; }"
        ]

    progMain = unlines
        [ "#include <stdlib.h>"
        , "#include <string.h>"
        , ""
        , "extern int *p_int_from_a_2;"
        , "extern const char *hello (void);"
        , ""
        , "int main (void) {"
        , "  if (*p_int_from_a_2 != 0x11223344)"
        , "    abort ();"
        , "  if (strcmp(hello(), \"Hello, world!\") != 0)"
        , "    abort ();"
        , "  return 0;"
        , "}"
        ]

    ldScript = unlines
        [ "SECTIONS {"
        , "  .text : { *(.text*) }"
        , "  .rodata : { *(.rodata .rodata.* .gnu.linkonce.r.*) }"
        , "  .data.rel.ro : { *(.data.rel.ro*) }"
        , "  .data : { *(.data*) }"
        , "  .bss : { *(.bss*) }"
        , "}"
        ]
