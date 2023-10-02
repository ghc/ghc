-- Note: this file formatted with fourmolu
{-# LANGUAGE BangPatterns #-}

import Control.Monad.IO.Class
import Data.Char
import Data.Data hiding (Fixity)
import Data.List
import qualified Data.Map as Map
import Debug.Trace
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser
import GHC.Driver.Errors.Types
import qualified GHC.Driver.Errors.Types as GHC
import qualified GHC.Driver.Session as GHC
import GHC.Hs.Dump
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), PpState (..), Token (..))
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Outputable

import qualified Text.Parsec as Parsec
import Text.Parsec.Char as PS
import Text.Parsec.Combinator as PS
import Text.Parsec.Prim as PS

-- import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)

-- import Text.Parsec.Char
-- import FunctionsAndTypesForParsing (regularParse, parseWithEof, parseWithLeftOver)
-- import Text.Parsec.String.Combinator (many1)
-- import Text.Parsec.Combinator (many1)

-- ---------------------------------------------------------------------

showAst :: (Data a) => a -> String
showAst ast =
    showSDocUnsafe
        $ showAstData BlankSrcSpanFile NoBlankEpAnnotations ast

-- =====================================================================

ppLexer, ppLexerDbg :: Bool -> (Located Token -> P a) -> P a
-- Use this instead of 'lexer' in GHC.Parser to dump the tokens for debugging.
ppLexerDbg queueComments cont = ppLexer queueComments contDbg
  where
    contDbg tok = trace ("pptoken: " ++ show (unLoc tok)) (cont tok)
ppLexer queueComments cont =
    Lexer.lexer
        queueComments
        ( \tk ->
            let
                contInner t = (trace ("ppLexer: tk=" ++ show (unLoc tk, unLoc t)) cont) t
                -- contPush = pushContext (unLoc tk) >> contIgnoreTok tk
                contIgnoreTok (L l tok) = do
                    case l of
                        RealSrcSpan r (Strict.Just b) -> Lexer.queueIgnoredToken (L (PsSpan r b) tok)
                        _ -> return ()
                    ppLexer queueComments cont
             in
                -- case tk of
                case (trace ("M.ppLexer:tk=" ++ show (unLoc tk)) tk) of
                    L _ ITeof -> contInner tk
                    L _ (ITcpp continuation s) -> do
                        if continuation
                            then pushContinuation tk
                            else processCppToks s
                        contIgnoreTok tk
                    _ -> do
                        state <- getCppState
                        case (trace ("CPP state:" ++ show state) state) of
                            CppIgnoring -> contIgnoreTok tk
                            _ -> contInner tk
        )

preprocessElse :: P ()
preprocessElse = do
    accepting <- getAccepting
    setAccepting (not accepting)

preprocessEnd :: P ()
preprocessEnd = do
    -- TODO: nested context
    setAccepting True

processCppToks :: FastString -> P ()
processCppToks fs = do
    let
        get (L _ (ITcpp _ s)) = s
        get _ = error "should not"
    -- Combine any prior continuation tokens
    cs <- popContinuation
    processCpp (reverse $ fs : map get cs)
    return ()

processCpp :: [FastString] -> P ()
processCpp fs = do
    -- traceM $ "processCpp: fs=" ++ show fs
    -- let s = cppInitial fs
    let s = cppInitial fs
    case regularParse cppDirective s of
      Left err -> error $ show err
      Right (CppDefine name def) -> do
        ppDefine name def
      Right (CppIfdef name) -> do
        defined <- ppIsDefined name
        setAccepting defined
      Right (CppIfndef name) -> do
        defined <- ppIsDefined name
        setAccepting (not defined)
      Right CppElse -> do
        accepting <- getAccepting
        setAccepting (not accepting)
        return ()
      Right CppEndif -> do
        -- TODO: nested states
        setAccepting True
        return ()

    return (trace ("processCpp:s=" ++ show s) ())

-- ---------------------------------------------------------------------
-- Preprocessor state functions

data CppState
    = CppIgnoring
    | CppNormal
    deriving (Show)

getCppState :: P CppState
getCppState = do
    accepting <- getAccepting
    if accepting
        then return CppNormal
        else return CppIgnoring

-- pp_context stack start -----------------

pushContext :: Token -> P ()
pushContext new =
    P $ \s -> POk s{pp = (pp s){pp_context = new : pp_context (pp s)}} ()

popContext :: P ()
popContext =
    P $ \s ->
        let
            new_context = case pp_context (pp s) of
                [] -> []
                (_ : t) -> t
         in
            POk s{pp = (pp s){pp_context = new_context}} ()

peekContext :: P Token
peekContext =
    P $ \s ->
        let
            r = case pp_context (pp s) of
                [] -> ITeof -- Anthing really, for now, except a CPP one
                (h : _) -> h
         in
            POk s r

setAccepting :: Bool -> P ()
setAccepting on =
    P $ \s -> POk s{pp = (pp s){pp_accepting = on}} ()

getAccepting :: P Bool
getAccepting = P $ \s -> POk s (pp_accepting (pp s))

-- -------------------------------------

pushContinuation :: Located Token -> P ()
pushContinuation new =
    P $ \s -> POk s{pp = (pp s){pp_continuation = new : pp_continuation (pp s)}} ()

popContinuation :: P [Located Token]
popContinuation =
    P $ \s -> POk s{pp = (pp s){pp_continuation = []}} (pp_continuation (pp s))

-- pp_context stack end -------------------

-- definitions start --------------------

ppDefine :: String -> [String] -> P ()
ppDefine name val = P $ \s ->
    -- POk s{pp = (pp s){pp_defines = Set.insert (cleanTokenString def) (pp_defines (pp s))}} ()
    POk s{pp = (pp s){pp_defines = Map.insert (trace ("ppDefine:def=[" ++ name ++ "]") name) val (pp_defines (pp s))}} ()

ppIsDefined :: String -> P Bool
ppIsDefined def = P $ \s ->
    -- POk s (Map.member def (pp_defines (pp s)))
    POk s (Map.member (trace ("ppIsDefined:def=[" ++ def ++ "]") def) (pp_defines (pp s)))

-- | Take a @FastString@ of the form "#define FOO\n" and strip off all but "FOO"
cleanTokenString :: FastString -> String
cleanTokenString fs = r
  where
    ss = dropWhile (\c -> not $ isSpace c) (unpackFS fs)
    r = init ss

parseDefine :: FastString -> Maybe (String, [String])
parseDefine fs = r
  where
    -- r = Just (cleanTokenString s, "")
    r = case regularParse cppDefinition (unpackFS fs) of
        Left _ -> Nothing
        Right v -> Just v

-- =====================================================================
-- Parsec parsing
type CppParser = Parsec String ()

regularParse :: Parser a -> String -> Either Parsec.ParseError a
regularParse p = PS.parse p ""


-- TODO: delete this
cppDefinition :: CppParser (String, [String])
cppDefinition = do
    _ <- PS.char '#'
    _ <- whiteSpace
    _ <- lexeme (PS.string "define")
    name <- cppToken
    definition <- cppTokens
    return (name, definition)

data CppDirective
  = CppDefine String [String]
  | CppIfdef String
  | CppIfndef String
  | CppElse
  | CppEndif
    deriving (Show, Eq)

cppDirective :: CppParser CppDirective
cppDirective = do
    _ <- PS.char '#'
    _ <- whiteSpace
    choice
        [ cppKw "define" >> cmdDefinition
        -- , cppKw "include" CppIncludeKw
        -- , cppKw "undef" CppUndefKw
        -- , cppKw "error" CppErrorKw
        , try$ cppKw "ifdef" >> cmdIfdef
        , cppKw "ifndef" >> cmdIfndef
        -- , cppKw "if" CppIfKw
        -- , cppKw "elif" CppElifKw
        , try $ cppKw "else" >> return CppElse
        , cppKw "endif" >> return CppEndif
        ]

cmdDefinition :: CppParser CppDirective
cmdDefinition = do
      name <- cppToken
      definition <- cppTokens
      return $ CppDefine name definition

cmdIfdef :: CppParser CppDirective
cmdIfdef = do
      name <- cppToken
      return $ CppIfdef name

cmdIfndef :: CppParser CppDirective
cmdIfndef = do
      name <- cppToken
      return $ CppIfndef name

cppKw :: String -> CppParser ()
cppKw kw = do
    _ <- lexeme (PS.string kw)
    return ()

cppComment :: CppParser ()
cppComment = do
    _ <- PS.string "/*"
    _ <- PS.manyTill PS.anyChar (PS.try (PS.string "*/"))
    return ()

whiteSpace :: CppParser ()
whiteSpace = do
    _ <- PS.many (PS.choice [cppComment, PS.space >> return ()])
    return ()

lexeme :: CppParser a -> CppParser a
lexeme p = p <* whiteSpace

cppToken :: CppParser String
cppToken = lexeme (PS.many1 (PS.satisfy (\c -> not (isSpace c))))

cppTokens :: CppParser [String]
cppTokens = PS.many cppToken

{- | Do cpp initial processing, as per https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html
See Note [GhcCPP Initial Processing]
-}
cppInitial :: [FastString] -> String
cppInitial fs = r
  where
    -- go fs' = reverse $ drop 2 $ reverse $ unpackFS fs'
    r = concatMap unpackFS fs

{-
Note [GhcCPP Initial Processing]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This processing is based on the description at
https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html

It is only done for lines starting with a preprocessor
directive.

1. Broken into lines.  We rely on the GHC Lexer to do this
2. Trigraphs are not processed
3. Continued lines are merged into a single line
   and is handled in the Lexer.
4. All comments are replaced with a single space

-}

-- =====================================================================
-- Emulate the parser

type LibDir = FilePath

-- parseString :: LibDir -> String -> IO (WarningMessages, Either ErrorMessages [Located Token])
parseString :: LibDir -> String -> IO [Located Token]
parseString libdir str = ghcWrapper libdir $ do
    dflags0 <- initDynFlags
    let dflags = dflags0{extensionFlags = EnumSet.insert LangExt.GhcCpp (extensionFlags dflags0)}
    let pflags = initParserOpts dflags
    -- return $ strParser str dflags "fake_test_file.hs"
    liftIO $ putStrLn "-- parsing ----------"
    liftIO $ putStrLn str
    liftIO $ putStrLn "---------------------"
    return $ strGetToks pflags "fake_test_file.hs" str

strGetToks :: Lexer.ParserOpts -> FilePath -> String -> [Located Token]
strGetToks popts filename str = reverse $ lexAll pstate
  where
    pstate = Lexer.initParserState popts buf loc
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
        case unP parseModuleNoHaddock (Lexer.initParserState (initParserOpts dflags) buf loc) of
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

parseModuleNoHaddock :: P [Located Token]
parseModuleNoHaddock = happySomeParser
  where
    -- happySomeParser = happyThen (happyParse 0#) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))
    happySomeParser = (>>=) (happyParse 0) (\x -> return x)

happyParse :: Int -> P [Located Token]
happyParse start_state = happyNewToken start_state [] []

happyNewToken :: Int -> [Int] -> [Located Token] -> P [Located Token]
happyNewToken action sts stk =
    -- lexer
    ppLexerDbg
        True
        ( \tk ->
            let cont i =
                    trace ("happyNewToken:tk=" ++ show tk)
                        $ happyDoAction i tk action sts stk
             in case tk of
                    L _ ITeof -> happyDoAction 169 tk action sts stk
                    _ -> cont 5
                    -- _ -> happyError' (tk, [])
        )

happyDoAction :: Int -> Located Token -> Int -> [Int] -> [Located Token] -> P [Located Token]
-- happyDoAction num tk action sts stk = P $ \s -> POk s tk
happyDoAction num tk action sts stk =
    case num of
        1 -> happyShift 2 num tk action sts stk
        2 -> happyShift 5 num tk action sts stk
        3 -> happyShift 5 num tk action sts stk
        4 -> happyShift 5 num tk action sts stk
        5 -> happyShift 5 num tk action sts stk
        50 -> happyAccept num tk action sts stk
        169 -> happyAccept num tk action sts stk
        i -> happyFail ["failing:" ++ show i] i tk action sts stk

-- happyAccept j tk st sts (HappyStk ans _) =
--         (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

happyAccept :: Int -> Located Token -> Int -> [Int] -> [Located Token] -> P [Located Token]
happyAccept _j tk _st _sts stk =
    trace ("happyAccept:" ++ show tk)
        $ return stk

-- happyReturn1 :: a -> P a
-- happyReturn1 = return

happyShift :: Int -> Int -> Located Token -> Int -> [Int] -> [Located Token] -> P [Located Token]
happyShift new_state _i tk st sts stk = do
    happyNewToken new_state (st : sts) (tk : stk)

-- happyShift new_state i tk st sts stk =
--      happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

happyFail :: [String] -> Int -> Located Token -> p2 -> p3 -> p4 -> P a
happyFail explist i tk _old_st _ _stk =
    trace ("failing" ++ show explist)
        $ happyError_ explist i tk

happyError_ :: [String] -> p -> Located Token -> P a
happyError_ explist _ tk = happyError' (tk, explist)

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

happyError' :: (Located Token, [String]) -> P a
happyError' tk = (\(_tokens, _explist) -> happyError) tk

happyError :: P a
happyError = Lexer.srcParseFail

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
doTest strings = do
    let test = intercalate "\n" strings
    !tks <- parseString libdirNow test
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
