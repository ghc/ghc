{-# LANGUAGE CPP #-}
module DirectCodegen where

{-
The standard mode for hsc2hs: generates a C file which is
compiled and run; the output of that program is the .hs file.
-}

import Data.Char                ( isAlphaNum, toUpper )
import Data.Foldable            ( foldl' )
import Control.Monad            ( when, forM_ )

import System.Exit              ( ExitCode(..), exitWith )
import System.FilePath          ( normalise )

import C
import Common
import Flags
import HSCParser
import UtilsCodegen

outputDirect :: Config -> FilePath -> FilePath -> FilePath -> String -> [Token] -> IO ()
outputDirect config outName outDir outBase name toks = do

    let beVerbose    = cVerbose config
        flags        = cFlags config
        enableCol    = cColumn config
        cProgName    = outDir++outBase++"_hsc_make.c"
        oProgName    = outDir++outBase++"_hsc_make.o"
        progName     = outDir++outBase++"_hsc_make"
#if defined(mingw32_HOST_OS) || defined(__CYGWIN32__)
-- This is a real hack, but the quoting mechanism used for calling the C preprocesseor
-- via GHC has changed a few times, so this seems to be the only way...  :-P * * *
                          ++ ".exe"
#endif
        outHFile     = outBase++"_hsc.h"
        outHName     = outDir++outHFile
        outCName     = outDir++outBase++"_hsc.c"

    let execProgName
            | null outDir = normalise ("./" ++ progName)
            | otherwise   = progName

    let specials = [(pos, key, arg) | Special pos key arg <- toks]

    let needsC = any (\(_, key, _) -> key == "def") specials
        needsH = needsC
        possiblyRemove = if cKeepFiles config
                         then flip const
                         else finallyRemove

    let includeGuard = map fixChar outHName
            where
            fixChar c | isAlphaNum c = toUpper c
                      | otherwise    = '_'

    when (cCrossSafe config) $
        forM_ specials (\ (SourcePos file line _,key,_) ->
            when (not $ key `elem` ["const","offset","size","alignment","peek","poke","ptr",
                                    "type","enum","error","warning","include","define","undef",
                                    "if","ifdef","ifndef", "elif","else","endif"]) $
             die (file ++ ":" ++ show line ++ " directive \"" ++ key ++ "\" is not safe for cross-compilation"))

    writeBinaryFile cProgName $
        outTemplateHeaderCProg (cTemplate config)++
        concatMap outFlagHeaderCProg flags++
        concatMap outHeaderCProg specials++
        "\nint main (void)\n{\n"++
        outHeaderHs flags (if needsH then Just outHName else Nothing) specials++
        outHsLine (SourcePos name 0 1)++
        fst (foldl' (outTokenHs enableCol) (id, (True, True)) toks) ""++
        "    return 0;\n}\n"

    when (cNoCompile config) $ exitWith ExitSuccess

    rawSystemL ("compiling " ++ cProgName) beVerbose (cCompiler config)
        (  ["-c"]
        ++ [cProgName]
        ++ ["-o", oProgName]
        ++ [f | CompFlag f <- flags]
        )
    possiblyRemove cProgName $
        withUtilsObject config outDir outBase $ \oUtilsName -> do

      rawSystemL ("linking " ++ oProgName) beVerbose (cLinker config)
        (  [oProgName, oUtilsName]
        ++ ["-o", progName]
        ++ [f | LinkFlag f <- flags]
        )
      possiblyRemove oProgName $ do

        rawSystemWithStdOutL ("running " ++ execProgName) beVerbose execProgName [] outName
        possiblyRemove progName $ do

          when needsH $ writeBinaryFile outHName $
            "#ifndef "++includeGuard++"\n" ++
            "#define "++includeGuard++"\n" ++
            "#include <HsFFI.h>\n" ++
            "#if __NHC__\n" ++
            "#undef HsChar\n" ++
            "#define HsChar int\n" ++
            "#endif\n" ++
            concatMap outFlagH flags++
            concatMap outTokenH specials++
            "#endif\n"

          when needsC $ writeBinaryFile outCName $
            "#include \""++outHFile++"\"\n"++
            concatMap outTokenC specials
            -- NB. outHFile not outHName; works better when processed
            -- by gcc or mkdependC.
