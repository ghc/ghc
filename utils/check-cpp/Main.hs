-- Note: this file formatted with fourmolu
{-# LANGUAGE BangPatterns #-}

import Control.Monad.IO.Class
import Data.Data hiding (Fixity)
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser
import GHC.Driver.Errors.Types
import qualified GHC.Driver.Errors.Types as GHC
import qualified GHC.Driver.Session as GHC
import GHC.Hs.Dump
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer (P (..), ParseResult (..), Token (..))
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Outputable
import qualified Text.Parsec as Parsec

import Parse
import ParseSimulate
import PreProcess

-- ---------------------------------------------------------------------

showAst :: (Data a) => a -> String
showAst ast =
    showSDocUnsafe
        $ showAstData BlankSrcSpanFile NoBlankEpAnnotations ast

-- =====================================================================

-- =====================================================================
-- Emulate the parser

type LibDir = FilePath
type Includes = [(String, [String])]

-- parseString :: LibDir -> String -> IO (WarningMessages, Either ErrorMessages [Located Token])
parseString :: LibDir -> Includes -> String -> IO [Located Token]
parseString libdir includes str = ghcWrapper libdir $ do
    dflags0 <- initDynFlags
    let dflags = dflags0{extensionFlags = EnumSet.insert LangExt.GhcCpp (extensionFlags dflags0)}
    let pflags = initParserOpts dflags
    -- return $ strParser str dflags "fake_test_file.hs"
    liftIO $ putStrLn "-- parsing ----------"
    liftIO $ putStrLn str
    liftIO $ putStrLn "---------------------"
    return $ strGetToks includes pflags "fake_test_file.hs" str

strGetToks :: Includes -> Lexer.ParserOpts -> FilePath -> String -> [Located Token]
-- strGetToks includes popts filename str = reverse $ lexAll pstate
strGetToks includes popts filename str = reverse $ lexAll (trace ("pstate=" ++ show initState) pstate)
  where
    includeMap = Map.fromList $ map (\(k, v) -> (k, stringToStringBuffer (intercalate "\n" v))) includes
    initState = initPpState{pp_includes = includeMap}
    pstate = Lexer.initParserState initState popts buf loc
    loc = mkRealSrcLoc (mkFastString filename) 1 1
    buf = stringToStringBuffer str
    -- cpp_enabled = Lexer.GhcCppBit `Lexer.xtest` Lexer.pExtsBitmap popts

    lexAll state = case unP (ppLexerDbg True return) state of
        POk _ t@(L _ ITeof) -> [t]
        POk state' t -> t : lexAll state'
        -- (trace ("lexAll: " ++ show (unLoc t)) state')
        PFailed pst -> error $ "failed" ++ showErrorMessages (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)

-- _ -> [L (mkSrcSpanPs (last_loc state)) ITeof]

showErrorMessages :: Messages GhcMessage -> String
showErrorMessages msgs =
    renderWithContext defaultSDocContext
        $ vcat
        $ pprMsgEnvelopeBagWithLocDefault
        $ getMessages
        $ msgs

{- | Parse a file, using the emulated haskell parser, returning the
resulting tokens only
-}
strParser ::
    -- | Haskell module source text (full Unicode is supported)
    String ->
    -- | the flags
    DynFlags ->
    -- | the filename (for source locations)
    FilePath ->
    (WarningMessages, Either ErrorMessages [Located Token])
strParser str dflags filename =
    let
        loc = mkRealSrcLoc (mkFastString filename) 1 1
        buf = stringToStringBuffer str
     in
        case unP parseModuleNoHaddock (Lexer.initParserState initPpState (initParserOpts dflags) buf loc) of
            PFailed pst ->
                let (warns, errs) = Lexer.getPsMessages pst
                 in (GhcPsMessage <$> warns, Left $ GhcPsMessage <$> errs)
            POk pst rdr_module ->
                let (warns, _) = Lexer.getPsMessages pst
                 in (GhcPsMessage <$> warns, Right rdr_module)

initDynFlags :: (GHC.GhcMonad m) => m GHC.DynFlags
initDynFlags = do
    -- Based on GHC backpack driver doBackPack
    dflags0 <- GHC.getSessionDynFlags
    -- let parser_opts0 = initParserOpts dflags0
    -- (_, src_opts)   <- GHC.liftIO $ GHC.getOptionsFromFile parser_opts0 file
    -- (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
    -- Turn this on last to avoid T10942
    let dflags2 = dflags0 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
    -- Prevent parsing of .ghc.environment.* "package environment files"
    (dflags3, _, _) <-
        GHC.parseDynamicFlagsCmdLine
            dflags2
            [GHC.noLoc "-hide-all-packages"]
    _ <- GHC.setSessionDynFlags dflags3
    return dflags3

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: LibDir -> GHC.Ghc a -> IO a
ghcWrapper libdir a =
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
        $ GHC.runGhc (Just libdir) a

-- ---------------------------------------------------------------------

-- =====================================================================
-- ---------------------------------------------------------------------

printToks :: [Located Token] -> IO ()
printToks toks = mapM_ go toks
  where
    go (L _ tk) = putStrLn (show tk)

-- Testing

libdirNow :: LibDir
libdirNow = "/home/alanz/mysrc/git.haskell.org/worktree/bisect/_build/stage1/lib"

doTest :: [String] -> IO ()
doTest strings = doTestWithIncludes [] strings

doTestWithIncludes :: Includes -> [String] -> IO ()
doTestWithIncludes includes strings = do
    let test = intercalate "\n" strings
    !tks <- parseString libdirNow includes test
    putStrLn "-----------------------------------------"
    printToks (reverse tks)

t0 :: IO ()
t0 = do
    doTest
        [ "# define FOO"
        , "#  ifdef FOO"
        , "x = 1"
        , "#endif"
        , ""
        ]

t1 :: IO ()
t1 = do
    doTest
        [ "data X = X"
        , ""
        ]

t2 :: IO ()
t2 = do
    doTest
        [ "#define FOO"
        , "#ifndef FOO"
        , "x = 1"
        , "#else"
        , "x = 5"
        , "#endif"
        ]

t3 :: IO ()
t3 = do
    doTest
        [ "{-# LANGUAGE GhcCPP #-}"
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

t3a :: IO ()
t3a = do
    doTest
        [ "{-# LANGUAGE GhcCPP #-}"
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
        ]

t5 :: IO ()
t5 = do
    doTest
        [ "#define MIN_VERSION_ghc_exactprint(major1,major2,minor) (\\"
        , "  (major1) <  1 || \\"
        , "  (major1) == 1 && (major2) <  7 || \\"
        , "  (major1) == 1 && (major2) == 7 && (minor) <= 0)"
        , "x = x"
        ]

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

t7 :: Maybe (String, [String])
t7 = parseDefine (mkFastString "#define VERSION_ghc_exactprint \"1.7.0.1\"")

t8 :: Maybe (String, [String])
t8 = parseDefine (mkFastString "#define MIN_VERSION_ghc_exactprint(major1,major2,minor) (  (major1) <  1 ||   (major1) == 1 && (major2) <  7 ||   (major1) == 1 && (major2) == 7 && (minor) <= 0)")

t9 :: Either Parsec.ParseError CppDirective
t9 = regularParse cppDirective "#define VERSION_ghc_exactprint \"1.7.0.1\""

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
        ]

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
