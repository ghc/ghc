module GHC.Toolchain.PlatformDetails
    ( checkWordSize
    , checkEndianness
    , checkLeadingUnderscore
    , checkSubsectionsViaSymbols
    , checkIdentDirective
    , checkGnuNonexecStack
    , checkTargetHasLibm
    , checkTargetHasLibdw
    ) where

import Data.List (isInfixOf)
import System.FilePath

import GHC.Platform.ArchOS

import GHC.Toolchain.Prelude
import GHC.Toolchain.Utils
import GHC.Toolchain.Target
import GHC.Toolchain.Program
import GHC.Toolchain.Library
import GHC.Toolchain.Tools.Cc
import GHC.Toolchain.Tools.Nm

checkWordSize :: Cc -> M WordSize
checkWordSize cc = checking "word size" $ do
    -- N.B. this is a surprisingly hard thing to check when cross-compiling.
    -- See https://stackoverflow.com/questions/4374379.
    -- To side-step this, we assume that the __SIZEOF_POINTER__ macro is
    -- available. It's technically not standard although should be available in
    -- any sane C implementation.
    output <- preprocess cc program
    case reverse $ lines output of
      []            -> throwE "test program produced no output"
      "undefined":_ -> throwE "__SIZEOF_POINTER__ is undefined"
      "8":_         -> return WS8
      "4":_         -> return WS4
      _             -> throwE $ "unexpected output:\n" ++ output
  where
    program = unlines
        [ "#include <stddef.h>"
        , "#include <inttypes.h>"
        , "#if !defined(__SIZEOF_POINTER__)"
        , "undefined"
        , "#else"
        , "__SIZEOF_POINTER__"
        , "#endif"
        ]

checkEndianness :: Cc -> M ByteOrder
checkEndianness cc = do
    checkEndiannessParamH cc <|> checkEndiannessLimitsH cc <|> checkEndianness__BYTE_ORDER__ cc

checkEndiannessParamH :: Cc -> M ByteOrder
checkEndiannessParamH cc = checking "endianness (param.h)" $ do
    output <- preprocess cc prog
    case reverse $ lines output of
      "big":_ -> return BigEndian
      "little":_ -> return LittleEndian
      "unknown":_ -> throwE "unknown endianness"
      _ -> throwE "unrecognized output"
  where
    prog = unlines
        [ "#include <sys/param.h>"
        , "#if ! (defined BYTE_ORDER && defined BIG_ENDIAN \\"
        , "   && defined LITTLE_ENDIAN && BYTE_ORDER && BIG_ENDIAN \\"
        , "   && LITTLE_ENDIAN)"
        , "bogus"
        , "#elif BYTE_ORDER == BIG_ENDIAN"
        , "big"
        , "#elif BYTE_ORDER == LITTLE_ENDIAN"
        , "little"
        , "#else"
        , "unknown"
        , "#endif"
        ]

checkEndiannessLimitsH :: Cc -> M ByteOrder
checkEndiannessLimitsH cc = checking "endianness (limits.h)" $ do
    out <- preprocess cc prog
    case reverse $ lines out of
      "big":_ -> return BigEndian
      "little":_ -> return LittleEndian
      "unknown":_ -> throwE "unknown endianness"
      _ -> throwE "unrecognized output"
  where
    prog = unlines
        [ "#include <limits.h>"
        , "#if defined(_LITTLE_ENDIAN)"
        , "little"
        , "#elif defined(_BIG_ENDIAN)"
        , "big"
        , "#else"
        , "unknown"
        , "#endif"
        ]

checkEndianness__BYTE_ORDER__ :: Cc -> M ByteOrder
checkEndianness__BYTE_ORDER__ cc = checking "endianness (__BYTE_ORDER__)" $ do
    out <- preprocess cc prog
    case reverse $ lines out of
      "big":_ -> return BigEndian
      "little":_ -> return LittleEndian
      "unknown":_ -> throwE "unknown endianness"
      _ -> throwE "unrecognized output"
  where
    prog = unlines
        [ "#include <sys/param.h>"
        , "#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__"
        , "little"
        , "#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__"
        , "big"
        , "#else"
        , "unknown"
        , "#endif"
        ]

checkLeadingUnderscore :: Cc -> Nm -> M Bool
checkLeadingUnderscore cc nm = checking ctxt $ withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    compileC cc test_o prog
    out <- readProgramStdout (nmProgram nm) [test_o]
    return $ "_func" `isInfixOf` out
  where
    prog = "int func(void) { return 0; }"
    ctxt = "whether symbols have leading underscores"

checkSubsectionsViaSymbols :: ArchOS -> Cc -> M Bool
checkSubsectionsViaSymbols archos cc =
  case archOS_arch archos of
    ArchAArch64 ->
      -- subsections via symbols is busted on arm64
      -- TODO: ^ is this comment up to date?
      return False
    _ ->
      testCompile
        "whether .subsections-via-symbols directive is supported"
        (asmStmt ".subsections_via_symbols") cc

checkIdentDirective :: Cc -> M Bool
checkIdentDirective =
    testCompile
      "whether the .ident directive is supported"
      (asmStmt ".ident \"GHC x.y.z\"")

checkGnuNonexecStack :: ArchOS -> Cc -> M Bool
checkGnuNonexecStack archOs =
    testCompile
      "whether GNU non-executable stack directives are supported"
      prog
  where
    progbits = case archOS_arch archOs of
                 ArchARM{} -> "%progbits" -- See #13937
                 _         -> "@progbits"

    prog = unlines [ asmStmt (".section .note.GNU-stack,\"\","++progbits)
                   , asmStmt ".section .text"
                   ]

checkTargetHasLibm :: Cc -> M Bool
checkTargetHasLibm cc = testLib cc "m" "atan" Nothing

checkTargetHasLibdw :: Cc -> Maybe FilePath -> Maybe FilePath -> M (Maybe Library)
checkTargetHasLibdw cc mincludeDir mlibDir = do
  b1 <- testHeader cc "elfutils/libdwfl.h" mincludeDir
  b2 <- testLib cc "dw" "dwfl_attach_state" mlibDir
  return $
    if b1 && b2
    then Just
      Library{ libName = "dw"
             , includePath = mincludeDir, libraryPath = mlibDir}
    else Nothing


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

asmStmt :: String -> String
asmStmt s = "__asm__(\"" ++ foldMap escape s ++ "\");"
  where
    escape '"' = "\\\""
    escape c   = [c]

-- | Check whether a lib is found and can be linked against.
-- Like @AC_CHECK_LIB@.
testLib :: Cc
        -> String         -- ^ Lib name
        -> String         -- ^ Lib symbol
        -> Maybe FilePath -- ^ Library dir (-L)
        -> M Bool
testLib cc0 libname symbol mlibDir = testCompile ("whether target has lib" ++ libname) prog cc2
  where
    cc1 = cc0 & _ccProgram % _prgFlags %++ ("-l" ++ libname)
    cc2 | Just libDir <- mlibDir
        = cc1 & _ccProgram % _prgFlags %++ ("-L" ++ libDir)
        | otherwise = cc1
    prog = unlines
        [ "char " ++ symbol ++ " (void);"
        , "int"
        , "main (void)"
        , "{"
        , "return " ++ symbol ++ " ();"
        , "  ;"
        , "  return 0;"
        , "}"
        ]

-- | Like @AC_CHECK_HEADER@
testHeader :: Cc
           -> String         -- ^ Header to check for
           -> Maybe FilePath -- ^ Extra path
           -> M Bool
testHeader cc0 header mincludeDir = testCompile ("whether target has <" ++ header ++ ">") prog cc1
  where
    cc1 | Just includeDir <- mincludeDir
        = cc0 & _ccProgram % _prgFlags %++ ("-I" ++ includeDir)
        | otherwise = cc0
    prog = unlines
        [ "#include <" ++ header ++ ">" ]

-- | Try compiling a program, returning 'True' if successful.
testCompile :: String -> String -> Cc -> M Bool
testCompile what program cc = checking what $ withTempDir $ \dir -> do
    let test_o = dir </> "test.o"
    (True <$ compileC cc test_o program) <|> return False
