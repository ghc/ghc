{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE CPP #-}

module GHC.Toolchain.Tools.MergeObjs ( MergeObjs(..), findMergeObjs ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.FilePath
import System.Process

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Program
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Link
import GHC.Toolchain.Tools.Nm

-- | Configuration on how the C compiler can be used to link
data MergeObjs = MergeObjs { mergeObjsProgram :: Program
                           }
    deriving (Show, Read)

findMergeObjs :: ProgOpt -> Cc -> CcLink -> Nm -> M MergeObjs
findMergeObjs progOpt cc ccLink nm = checking "for linker for merging objects" $ do
    prog <- findProgram "linker for merging objects" progOpt ["ld"]
    let mo = MergeObjs $ over _prgFlags (++["-r"]) prog
    checkMergingWorks cc nm mo
    checkForGoldT22266 cc ccLink mo
    return mo

checkMergingWorks :: Cc -> Nm -> MergeObjs -> M ()
checkMergingWorks cc nm mergeObjs =
    checking "whether object merging works" $ withTempDir $ \dir -> do
        let fo s = dir </> s <.> "o"
        compileC cc (fo "a") "void funA(int x) { return x; }"
        compileC cc (fo "b") "void funB(int x) { return x; }"
        callProgram (mergeObjsProgram mergeObjs) [fo "a", fo "b", "-o", fo "out"]
        out <- readProgram (nmProgram nm) [fo "out"]
        let ok = all (`isInfixOf` out) ["funA", "funB"]
        unless ok $ throwE "merged objects is missing symbols"

checkForGoldT22266 :: Cc -> CcLink -> MergeObjs -> M ()
checkForGoldT22266 cc ccLink mergeObjs = do
    version <- checking "for ld.gold object merging bug (binutils #22266)" $
        readProgram (mergeObjsProgram mergeObjs) ["--version"]
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
            callProgram (mergeObjsProgram mergeObjs)
                ["-T", link_script, "-o", merged_o]
            compileC cc main_o progMain
            callProgram (ccLinkProgram ccLink)
                ["-o", exe, merged_o, main_o]
            liftIO $ callProcess exe []

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
