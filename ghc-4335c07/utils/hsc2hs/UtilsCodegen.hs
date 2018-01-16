module UtilsCodegen where

{-
Generate the utility code for hsc2hs.

We don't want to include C headers in template-hsc.h
See GHC trac #2897
-}

import Control.Monad

import C
import Common
import Flags

withUtilsObject :: Config -> FilePath -> FilePath
                -> (FilePath -> IO a)
                -> IO a
withUtilsObject config outDir outBase f = do

    let beVerbose = cVerbose config
        flags     = cFlags config
        possiblyRemove = if cKeepFiles config
                         then flip const
                         else finallyRemove
        cUtilsName = outDir ++ outBase ++ "_hsc_utils.c"
        oUtilsName = outDir ++ outBase ++ "_hsc_utils.o"

    possiblyRemove cUtilsName $ do
        writeBinaryFile cUtilsName $ unlines $
            ["#include <stddef.h>",
             "#include <string.h>",
             "#include <stdio.h>",
             "#include <stdarg.h>",
             "#include <ctype.h>",
             "",
             outTemplateHeaderCProg (cTemplate config),
             "",
             "int hsc_printf(const char *format, ...) {",
             "    int r;",
             "    va_list argp;",
             "    va_start(argp, format);",
             "    r = vprintf(format, argp);",
             "    va_end(argp);",
             "    return r;",
             "}",
             "",
             "int hsc_toupper(int c) {",
             "    return toupper(c);",
             "}",
             "",
             "int hsc_tolower(int c) {",
             "    return tolower(c);",
             "}",
             "",
             "int hsc_putchar(int c) {",
             "    return putchar(c);",
             "}",
             "",
             -- "void" should really be "FILE", but we aren't able to
             -- refer to "FILE" in template-hsc.h as we don't want to
             -- include <stdio.h> there. We cast to FILE * so as to
             -- allow compiling with g++.
             "int hsc_fputs(const char *s, void *stream) {",
             "    return fputs(s, (FILE *)stream);",
             "}",
             "",
             -- "void" should really be "FILE", but we aren't able to
             -- refer to "FILE" in template-hsc.h as we don't want to
             -- include <stdio.h> there. We explicitly cast to void *
             -- to allow compiling with g++.
             "void *hsc_stdout(void) {",
             "    return (void *)stdout;",
             "}"
            ]

        possiblyRemove oUtilsName $ do
           unless (cNoCompile config) $
               rawSystemL ("compiling " ++ cUtilsName)
                          beVerbose
                          (cCompiler config)
                          (["-c", cUtilsName, "-o", oUtilsName] ++
                           [cFlag | CompFlag cFlag <- flags])

           f oUtilsName

