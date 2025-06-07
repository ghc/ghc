-- Note: this file formatted with fourmolu
{-# LANGUAGE BangPatterns #-}

import Control.Monad.IO.Class
import Data.Data hiding (Fixity)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Maybe
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Config.Parser hiding (predefinedMacros)
import GHC.Driver.Env.Types
import GHC.Driver.Errors.Types
import GHC.Driver.Session hiding (initDynFlags)
import qualified GHC.Driver.Errors.Types as GHC
import qualified GHC.Driver.Session as GHC
import GHC.Hs.Dump
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Header
import GHC.Parser.Lexer (P (..), ParseResult (..))
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Parser.Lexer as Lexer
import GHC.SysTools.Cpp
import GHC.Types.SrcLoc
import GHC.Unit.Env
import GHC.Unit.State
import GHC.Utils.Outputable

-- Local simulation -----------------
import ParsePP
import qualified ParserM as PM
import PreProcess as PP
import State

-- The real thing -------------------
-- import GHC.Parser
-- import GHC.Parser.PreProcess
-- import GHC.Parser.PreProcess as PP
-- import GHC.Parser.PreProcess.ParsePP
-- import GHC.Parser.PreProcess.State

-- ---------------------------------------------------------------------

showAst :: (Data a) => a -> String
showAst ast =
    showSDocUnsafe $
        showAstData BlankSrcSpanFile NoBlankEpAnnotations ast

-- =====================================================================

-- =====================================================================
-- Emulate the parser

type LibDir = FilePath
type Includes = [(String, [String])]

parseString :: LibDir -> Includes -> String -> IO [Located Token]
parseString libdir includes str = ghcWrapper libdir $ do
    dflags0 <- initDynFlags
    let dflags = dflags0{extensionFlags = EnumSet.insert LangExt.GhcCpp (extensionFlags dflags0)}
    let pflags = initParserOpts dflags
    hsc <- getSession
    liftIO $ putStrLn "-- hsc ----------"
    liftIO $ putStrLn (show (length (unitPackages (hsc_unit_env hsc))))
    liftIO $ putStrLn (cppMacroDefines (hsc_unit_env hsc))
    liftIO $ putStrLn "-- parsing ----------"
    liftIO $ putStrLn str
    liftIO $ putStrLn "---------------------"
    -- return $ strParserWrapper str dflags "fake_test_file.hs"
    return $ snd $ strGetToks dflags includes pflags "fake_test_file.hs" str

doDump :: LibDir -> String -> IO ()
doDump libdir str = ghcWrapper libdir $ do
    dflags0 <- initDynFlags
    let dflags = dflags0{extensionFlags = EnumSet.insert LangExt.GhcCpp (extensionFlags dflags0)}
    -- let pflags = initParserOpts dflags
    let pflags = myInitParserOpts dflags
    -- hsc <- getSession
    liftIO $ putStrLn "-- parsing ----------"
    liftIO $ putStrLn str
    liftIO $ putStrLn "-- dumpGhcCpp test ----------"
    -- let (!pst,!toks) = strGetToks dflags [] pflags "fake_test_file.hs" str
    -- hsc <- getSession
    -- let pst0 =
    --         initParserStateWithMacros
    --             dflags
    --             (Just (hsc_unit_env hsc))
    --             pflags
    --             (stringToStringBuffer str)
    --             (mkRealSrcLoc (mkFastString "fake_test_file.hs") 1 1)
    -- liftIO $ putStrLn $ showPprUnsafe $ GHCPP.dumpGhcCpp pst0
    liftIO $ putStrLn "-- dumpGhcCpp test done ----------"
    let !pst = getPState dflags [] pflags "fake_test_file.hs" str
    liftIO $ putStrLn "-- dumpGhcCpp ----------"
    liftIO $ putStrLn $ showPprUnsafe $ dumpGhcCpp dflags pst
    liftIO $ putStrLn "---------------------"

-- return $ strGetToks dflags includes pflags "fake_test_file.hs" str

myInitParserOpts :: DynFlags -> Lexer.ParserOpts
myInitParserOpts =
  Lexer.mkParserOpts
    <$> extensionFlags
    <*> initDiagOpts
    <*> safeImportsOn
    <*> gopt Opt_Haddock
    <*> gopt Opt_KeepRawTokenStream
    <*> const False -- do not use LINE/COLUMN to update the internal location

unitPackages :: UnitEnv -> [UnitInfo]
unitPackages unit_env = pkgs
  where
    unit_state = ue_homeUnitState unit_env
    uids = explicitUnits unit_state
    pkgs = mapMaybe (lookupUnit unit_state . fst) uids

strGetToks ::
    DynFlags ->
    Includes ->
    Lexer.ParserOpts ->
    FilePath ->
    String ->
    (Lexer.PState PpState, [Located Token])
strGetToks dflags includes popts filename str = (final, reverse toks)
  where
    pstate = getPState dflags includes popts filename str
    (final, toks) = PP.lexAll pstate

getPState ::
    DynFlags ->
    Includes ->
    Lexer.ParserOpts ->
    FilePath ->
    String ->
    Lexer.PState PpState
getPState dflags includes popts filename str = pstate
  where
    includeMap = Map.fromList $ map (\(k, v) -> (k, stringToStringBuffer (intercalate "\n" v))) includes
    initState =
        initPpState
            { pp_includes = includeMap
            , pp_defines = predefinedMacros dflags
            , pp_scope = (PpScope True PpNoGroup) :| []
            }
    pstate = Lexer.initParserState initState popts buf loc
    -- pstate = Lexer.initPragState initState popts buf loc
    loc = mkRealSrcLoc (mkFastString filename) 1 1
    buf = stringToStringBuffer str

-- ---------------------------------------------------------------------

parseWith ::
    DynFlags ->
    FilePath ->
    P PpState w ->
    String ->
    Either GHC.ErrorMessages (Lexer.PState PpState, w)
parseWith dflags fileName aParser s =
    case runParser aParser dflags fileName s of
        PFailed pst ->
            Left (GhcPsMessage <$> GHC.getPsErrorMessages pst)
        POk pst pmod ->
            Right (pst, pmod)

runParser :: P PpState a -> DynFlags -> FilePath -> String -> ParseResult PpState a
runParser aParser flags filename str = unP aParser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState (initParserOpts flags) buffer location

-- ---------------------------------------------------------------------

initDynFlags :: (GHC.GhcMonad m) => m GHC.DynFlags
initDynFlags = do
    -- Based on GHC backpack driver doBackPack
    dflags0 <- GHC.getSessionDynFlags
    logger <- getLogger
    -- let parser_opts0 = initParserOpts dflags0
    -- (_, src_opts)   <- GHC.liftIO $ GHC.getOptionsFromFile parser_opts0 file
    -- (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
    -- Turn this on last to avoid T10942
    let dflags2 = dflags0 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
    -- Prevent parsing of .ghc.environment.* "package environment files"
    (dflags3, _, _) <-
        GHC.parseDynamicFlagsCmdLine
            logger
            dflags2
            [GHC.noLoc "-hide-all-packages"]
    _ <- GHC.setSessionDynFlags dflags3
    return dflags3

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: LibDir -> GHC.Ghc a -> IO a
ghcWrapper libdir a =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
        GHC.runGhc (Just libdir) a

-- ---------------------------------------------------------------------

predefinedMacros :: DynFlags -> MacroDefines
predefinedMacros _df =
    Map.fromList
        [
            ( "__GLASGOW_HASKELL__"
            , Map.singleton Nothing (Nothing, [PM.TInteger projectVersionInt])
            )
        ,
            ( "__GLASGOW_HASKELL_FULL_VERSION__"
            , Map.singleton Nothing (Nothing, [PM.TOther projectVersion])
            )
        ,
            ( "__GLASGOW_HASKELL_PATCHLEVEL1__"
            , Map.singleton Nothing (Nothing, [PM.TOther projectPatchLevel1])
            )
        ,
            ( "__GLASGOW_HASKELL_PATCHLEVEL2__"
            , Map.singleton Nothing (Nothing, [PM.TOther projectPatchLevel2])
            )
        ]
  where
    projectVersionInt = "913"
    projectVersion = "projectVersion"
    projectPatchLevel1 = "pl1"
    projectPatchLevel2 = "pl2"

-- =====================================================================
-- ---------------------------------------------------------------------

printToks :: [Located Token] -> IO ()
printToks toks = mapM_ go toks
  where
    go (L _ tk) = putStrLn (show tk)

-- Testing

libdirNow :: LibDir
libdirNow = "/home/alanz/mysrc/git.haskell.org/worktree/bisect/_build/stage1/lib"

dump :: [String] -> IO ()
dump strings = do
    let test = intercalate "\n" strings
    doDump libdirNow test

doTest :: [String] -> IO ()
doTest strings = doTestWithIncludes [] strings

doTestWithIncludes :: Includes -> [String] -> IO ()
doTestWithIncludes includes strings = do
    let test = intercalate "\n" strings
    !tks <- parseString libdirNow includes test
    putStrLn "-----------------------------------------"
    printToks (reverse tks)

t00 :: IO ()
t00 = do
    doTest
        [ "x + 1"
        ]

t0 :: IO ()
t0 = do
    doTest
        [ "# define FOO"
        , "#  ifdef FOO"
        , "x = 1"
        , "#endif"
        , ""
        ]

-- x = 1

t1 :: IO ()
t1 = do
    doTest
        [ "data X = X"
        , ""
        ]

-- data X = X

t2 :: IO ()
t2 = do
    doTest
        [ "#define FOO"
        , "#ifndef FOO"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

-- x = 5

t3 :: IO ()
t3 = do
    doTest
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example1 where"
        , ""
        , "y = 1"
        , ""
        , "#define FOO"
        , ""
        , "x ="
        , "#ifdef FOO"
        , "  \" hello \""
        , "#else"
        , "  \" bye now \""
        , "#endif"
        , ""
        , "foo = putStrLn x"
        ]

-- y = 1
-- x = "hello"
-- foo = putStrLn x

t3a :: IO ()
t3a = do
    doTest
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example1 where"
        , ""
        , "#define FOO"
        , ""
        , "x ="
        , "#ifdef FOO"
        , "  \" hello \""
        , "#else"
        , "  \" bye now \""
        , "#endif"
        , ""
        , "foo = putStrLn x"
        ]

-- x = "hello"
-- foo = putStrLn x

t4 :: IO ()
t4 = do
    doTest
        [ "/* package ghc-exactprint-1.7.0.1 */"
        , "#ifndef VERSION_ghc_exactprint"
        , "#define VERSION_ghc_exactprint \"1.7.0.1\""
        , "#endif /* VERSION_ghc_exactprint */"
        , "#ifndef MIN_VERSION_ghc_exactprint"
        , -- , "#define MIN_VERSION_ghc_exactprint(major1,major2,minor) (\\"
          -- , "  (major1) <  1 || \\"
          -- , "  (major1) == 1 && (major2) <  7 || \\"
          -- , "  (major1) == 1 && (major2) == 7 && (minor) <= 0)"
          "#endif /* MIN_VERSION_ghc_exactprint */"
        , ""
        , "#ifdef VERSION_ghc_exactprint"
        , "x = \"got version\""
        , "#else"
        , "x = \"no version\""
        , "#endif"
        , ""
        ]

-- x = "got version"

t5 :: IO ()
t5 = do
    doTest
        [ "#define MIN_VERSION_ghc_exactprint(major1,major2,minor) (\\"
        , "  (major1) <  1 || \\"
        , "  (major1) == 1 && (major2) <  7 || \\"
        , "  (major1) == 1 && (major2) == 7 && (minor) <= 0)"
        , "x = x"
        ]

-- x = x

t6 :: IO ()
t6 = do
    doTest
        [ "#define VERSION_ghc_exactprint \"1.7.0.1\""
        , ""
        , "#ifdef VERSION_ghc_exactprint"
        , "x = \"got version\""
        , "#else"
        , "x = \"no version\""
        , "#endif"
        , ""
        ]

-- x = "got version"

t7 :: Either String CppDirective
t7 = parseDirective "#define VERSION_ghc_exactprint \"1.7.0.1\""

t8 :: Either String CppDirective
t8 = parseDirective "#define MIN_VERSION_ghc_exactprint(major1,major2,minor) (  (major1) <  1 ||   (major1) == 1 && (major2) <  7 ||   (major1) == 1 && (major2) == 7 && (minor) <= 0)"

t9 :: Either String CppDirective
t9 = parseDirective "#define VERSION_ghc_exactprint \"1.7.0.1\""

t10 :: IO ()
t10 = do
    doTestWithIncludes
        testIncludes
        [ "#include \"bar.h\""
        , ""
        , "#ifdef FOO"
        , "x = 1"
        , "#else"
        , "x = 2"
        , "#endif"
        , ""
        ]

-- x = 1

testIncludes :: Includes
testIncludes =
    [
        ( "bar.h"
        , ["#include \"sub.h\""]
        )
    ,
        ( "sub.h"
        , ["#define FOO"]
        )
    ]

t11 :: IO ()
t11 = do
    doTest
        [ "#define FOO 4"
        , "#if FOO > 3"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

-- x = 1

t12 :: IO ()
t12 = do
    doTest
        [ "#define FOO 4"
        , "#if FOO > 3"
        , "#dumpghccpp"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

-- x = 1

t13 :: IO ()
t13 = do
    doTest
        [ "#if __GLASGOW_HASKELL__ == 913"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

-- x = 1

t14 :: IO ()
t14 = do
    doTest
        [ "#define FOO 5"
        , "#ifdef FOO"
        , "#define BAR 6"
        , "#else"
        , "#define BAZ 6"
        , "#endif"
        , "#ifdef BAR"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , "#ifdef BAZ"
        , "z = 1"
        , "#else"
        , "z = 5"
        , "#endif"
        , ""
        ]

-- x = 1
-- z = 5

t15 :: IO ()
t15 = do
    doTest
        [ "#define FOO() 5"
        , "#define FOO1(a) a+5"
        , "#define FOO2(a,b) a+b+5"
        , "#define FOO3(a,b,c) a+b+c+5"
        ]

t16 :: IO ()
t16 = do
    doTest
        [ "#define FOO(A,B) A + B"
        , "#if FOO(1,2) == 3"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

-- x = 1

t17 :: IO ()
t17 = do
    doTest
        [ "#define FOO(A,B) A + B"
        , "#if FOO(1,FOO(3,4)) == 8"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

-- x = 1

t18 :: IO ()
t18 = do
    dump
        [ "#define FOO(A,B) A + B"
        , "#define FOO(A,B,C) A + B + C"
        , "#if FOO(1,FOO(3,4)) == 8"
        , ""
        , "-- a comment"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        , ""
        ]

t19 :: IO ()
t19 = do
    dump
        [ "#define MIN_VERSION_ghc_exactprint(major1,major2,minor) (\\"
        , "  (major1) <  1 || \\"
        , "  (major1) == 1 && (major2) <  7 || \\"
        , "  (major1) == 1 && (major2) == 7 && (minor) <= 0)"
        , "x = x"
        ]

-- x = x

initDynFlags2 :: (GHC.GhcMonad m) => FilePath -> m GHC.DynFlags
initDynFlags2 file = do
    -- Based on GHC backpack driver doBackPack
    dflags0 <- GHC.getSessionDynFlags
    let parser_opts0 = initParserOpts dflags0
    logger <- getLogger
    hsc_env <- getSession
    let unit_env = hsc_unit_env hsc_env
    (_, src_opts) <- liftIO $ getOptionsFromFile dflags0 unit_env parser_opts0 (supportedLanguagePragmas dflags0) file
    liftIO $ putStrLn $ "src_opts:" ++ show  (map unLoc src_opts)
    (dflags1, _, _) <- GHC.parseDynamicFilePragma logger dflags0 src_opts
    -- Turn this on last to avoid T10942
    let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
    -- Prevent parsing of .ghc.environment.* "package environment files"
    (dflags3, _, _) <-
        GHC.parseDynamicFlagsCmdLine
            logger
            dflags2
            [GHC.noLoc "-hide-all-packages"]
    _ <- GHC.setSessionDynFlags dflags3
    return dflags3

getPragmas1 :: IO ()
getPragmas1 = ghcWrapper libdirNow $ do
    _dflags <- initDynFlags2 "Example4.hs"
    return ()

getPragmas2 :: IO ()
getPragmas2 = ghcWrapper libdirNow $ do
    _dflags <- initDynFlags2 "Example18.hs"
    return ()

t20 :: IO ()
t20 = do
    dump
        [ "{-# LANGUAGE CPP #-}"
        , "#if __GLASGOW_HASKELL__ > 913"
        , "{-# LANGUAGE GHC_CPP #-}"
        , "#endif"
        , ""
        , "module Example4 where"
        ]

t21 :: IO ()
t21 = do
    dump
        ["x = 1"]

t22 :: IO ()
t22 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "#if 0"
        , "ignored"
        , "#if 1"
        , "also ignored"
        , "#endif"
        , "#endif"
        , ""
        ]

t23 :: IO ()
t23 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "#define DEBUG 4"
        , "#if defined(DEBUG)"
        , "x = 1"
        , "#else"
        , "x = 2"
        , "#endif"
        , ""
        ]

t24 :: IO ()
t24 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "#define DEBUG 4"
        , "#if defined DEBUG "
        , "x = 1"
        , "#else"
        , "x = 2"
        , "#endif"
        , ""
        ]

t25 :: IO ()
t25 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "#define DEBUG 4"
        , "#if defined DEBUG || defined (FOO) "
        , "x = 1"
        , "#else"
        , "x = 2"
        , "#endif"
        , ""
        ]

t26 :: IO ()
t26 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example5 where"
        , ""
        , "sendMessage fd msg = do"
        , "  fromIntegral"
        , ""
        , "#if defined(HAVE_EVENTFD)"
        , "foreign import ccall unsafe \"sys/eventfd.h eventfd_write\""
        , "   c_eventfd_write :: CInt -> CULLong -> IO CInt"
        , "#endif"
        , ""
        , "foreign import ccall unsafe \"setIOManagerWakeupFd\""
        , "   c_setIOManagerWakeupFd :: CInt -> IO ()"
        ]

t27 :: IO ()
t27 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example5 where"
        , ""
        , "import MagicHaskeller.Analytical"
        , "#ifdef DEBUG"
        , "                                 hiding (rev)"
        , "#endif"
        , ""
        ]

t28 :: IO ()
t28 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "-- (Typed)IOPairs¾å¤Ç¥Ç¡¼¥¿¤ò¤È¤ë¡¥ghci¾å¤Ç :cmd ¤ò»È¤¤¤Þ¤¯¤ë´¶¤¸¡¥"
        , "module Example5 where"
        , ""
        , "import MagicHaskeller.Analytical"
        , "#ifdef DEBUG"
        , "                                 hiding (rev)"
        , "#endif"
        , "import MagicHaskeller.Classification(Filtrable)"
        , ""
        ]

t29 :: IO ()
t29 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "requestQuery = do"
        , "    if st < 400"
        , "#ifdef DEBUG"
        , "        do  e"
        , "#else"
        , "        then return body"
        , "        else fail \"not reached\""
        , "#endif"
        , ""
        ]

t30 :: IO ()
t30 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "#if __GLASGOW_HASKELL__ >= 503"
        , "import GHC.Word"
        , "import GHC.Exts (  Int(..), shiftRL# )"
        , "#elif __GLASGOW_HASKELL__"
        , "import Word"
        , "import GlaExts ( Word(..), Int(..), shiftRL# )"
        , "#else"
        , "import Data.Word"
        , "#endif"
        , ""
        ]

t31 :: IO ()
t31 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , "#if 0"
        , "import GHC.Word"
        , "import GHC.Exts (  Int(..), shiftRL# )"
        , "#elif __GLASGOW_HASKELL__"
        , "import Word"
        , "import GlaExts ( Word(..), Int(..), shiftRL# )"
        , "#else"
        , "import Data.Word"
        , "#endif"
        , ""
        ]

t32 :: IO ()
t32 = do
    dump
        [ "{-# LANGUAGE"
        , "    GHC_CPP"
        , "  , DeriveGeneric"
        , "#-}"
        , ""
        , "module Example12 where"
        ]

t33 :: IO ()
t33 = do
    dump
        [ "{-# LANGUAGE"
        , "    GHC_CPP"
        , "  , DeriveGeneric"
        , "#-}"
        , ""
        , "{-# LINE 4 \"hypsrc-test/src/PositionPragmas.hs\" #-}"
        , "module Example12 where"
        ]

t34 :: IO ()
t34 = do
    dump

        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example4 where"
        , ""
        , "#ifndef USE_POLYPARSE"
        , "import                  Control.Arrow((|||))"
        , "import qualified        Text.ParserCombinators.Parsec"
        , "#elif USE_POLYPARSE == 'L'"
        , "import qualified        Text.ParserCombinators.Poly.Lazy        as Poly"
        , "#elif USE_POLYPARSE == 'P'"
        , "import                  Control.Arrow((|||))"
        , "import qualified        Text.ParserCombinators.Poly.Plain       as Poly"
        , "#else"
        , "#       error \"USE_POLYPARSE invalid\""
        , "#endif"
        , ""
        ]

t35 :: IO ()
t35 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example14 where"

        , "#define FOO"
        , "#define FOO(X) X"
        , ""
        , "#undef FOO"
        , ""
        , "foo ="
        , "#ifdef FOO"
        , "  'a'"
        , "#else"
        , "  'b'"
        , "#endif"
        , ""
        ]

t36 :: IO ()
t36 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example15 where"
        , "#define MIN_VERSION_Cabal(a,b,c) 1"
        , ""
        , "#ifdef MIN_VERSION_Cabal"
        , "#undef CH_MIN_VERSION_Cabal"
        , "#define CH_MIN_VERSION_Cabal MIN_VERSION_Cabal"
        , "#endif"
        , ""
        , "#if CH_MIN_VERSION_Cabal(1,22,0)"
        , "x = 1"
        , "#endif"
        , ""
        ]

t37 :: IO ()
t37 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example14 where"
        , ""
        , "foo ="
        , "#if 1 /* and a comment */"
        , "  'a'"
        , "#else"
        , "  'b'"
        , "#endif"
        , ""
        ]

t38 :: IO ()
t38 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module Example16 where"
        , ""
        , "{-# RULES"
        , "\"foldg/Empty\"   forall e v o c. foldg e v o c Empty = e"
        , "#-}"
        , ""
        , "buildg :: Int"
        , "buildg = 1"
        ]

t39 :: IO ()
t39 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "{- WARNING! Do not edit!!!"
        , "   This code is autogenerated from src/data/*.txt! -}"
        , "module Example16 where"
        , "x='a'"
        , ""
        ]

t40 :: IO ()
t40 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "/* multi-line"
        , "   cpp-style comment */"
        , "module Example16 where"
        , "{- Haskell comment"
        , "   /* ignores cpp comments, so unclosed is fine -}"
        , "x='a'"
        , ""
        ]

t41 :: IO ()
t41 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module CFG where"
        , ""
        , "{-# LINE 6 \"src/CFG.ag\" #-}"
        , ""
        , "import Data.Word"
        , "import ByteCode"
        ]

t42 :: IO ()
t42 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "module T23465 where"
        , ""
        , "{-# WARNInG"
        , "   in \"x-f\" f \"fw\" ;"
        , "   in \"x-f\" g \"gw\""
        , "#-}"
        , "f = f"
        , "g = g"
        ]

t43 :: IO ()
t43 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "{-# LANGUAGE Trustworthy #-}"
        -- , "#if MIN_VERSION_base(4,10,0)"
        , "#if 1"
        , "{-# LANGUAGE GADTs #-}"
        -- , "# if !(MIN_VERSION_base(4,11,0))"
        , "# if !(1)"
        , "{-# LANGUAGE TypeInType #-}"
        , "# endif"
        , "#endif"
        , "module T23465 where"
        , ""
        ]

t44 :: IO ()
t44 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , "{-# RULES"
        , "\"Lazy Bitstream streamChunks/unstreamChunks fusion\""
        , "    ∀s. streamChunks (unId (unstreamChunks s)) = s"
        , "#if 1"
        , "\"Lazy Bitstream unstreamChunks/streamChunks fusion\""
        , "    ∀v. unId (unstreamChunks (streamChunks v)) = v"
        , "#endif"
        , "  #-}"
        , ""
        ]

t45 :: IO ()
t45 = do
    dump
        [ "{-# LANGUAGE GHC_CPP #-}"
        , ""
        , "foo ="
        , "-- deliberate missing #if"
        , "#else"
        , "           toRow $(return $ ConP cname $ map VarP cvars) = $(toFields cvars)|]"
        , "#endif"
        , ""
        ]
