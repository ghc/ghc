-- Note: this file formatted with fourmolu

import Control.Monad.IO.Class
import Data.Data hiding (Fixity)
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString
import GHC.Data.Maybe
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

-- NOTE: instead of pulling tokens and calling cont, consider putting
-- this inside Lexer.lexer, much like the queueComments stuff
-- That sorts out
--    - ALR
--    - queueing comments
-- ppLexer _queueComments cont = do
--     tok <- ppLexToken
--     trace ("ppLexer:" ++ show (unLoc tok)) $ do
--         tok' <- case tok of
--             L _ ITcppIf -> preprocessIf tok
--             L _ ITcppDefine -> preprocessDefine tok
--             L _ ITcppIfdef -> preprocessIfDef tok
--             L _ ITcppElse -> preprocessElse tok
--             L _ ITcppEndif -> preprocessEnd tok
--             L l _ -> do
--                 accepting <- getAccepting
--                 if accepting
--                     then return tok
--                     else return (L l (ITcppIgnored [tok]))
--         cont tok'

ppLexer queueComments cont =
    Lexer.lexer
        queueComments
        ( \tk@(L lt _) ->
            let
                contInner t = (trace ("ppLexer: tk=" ++ show (unLoc tk, unLoc t)) cont) t
                contPush = pushContext (unLoc tk) >> contInner (L lt (ITcppIgnored [tk]))
             in
                case tk of
                    L _ ITcppDefine -> contPush
                    L _ ITcppIf -> contPush
                    L _ ITcppIfdef -> contPush
                    L _ ITcppElse -> do
                      tk' <- preprocessElse tk
                      contInner tk'
                    L _ ITcppEndif -> do
                      tk' <- preprocessEnd tk
                      contInner tk'
                    L l tok -> do
                        state <- getCppState
                        case (trace ("CPP state:" ++ show state) state) of
                            CppIgnoring -> contInner (L l (ITcppIgnored [tk]))
                            CppInDefine -> do
                                ppDefine (trace ("ppDefine:" ++ show tok) (show tok))
                                popContext
                                contInner (L l (ITcppIgnored [tk]))
                            CppInIfdef -> do
                                defined <- ppIsDefined (show tok)
                                if defined
                                    then setAccepting True
                                    else setAccepting False
                                popContext
                                contInner (L l (ITcppIgnored [tk]))
                            _ -> contInner tk
        )

-- Swallow tokens until ITcppEndif
preprocessIf :: Located Token -> P (Located Token)
preprocessIf tok = go [tok]
  where
    go :: [Located Token] -> P (Located Token)
    go acc = do
        tok' <- ppLexToken
        case tok' of
            L l ITcppEndif -> return $ L l (ITcppIgnored (reverse (tok' : acc)))
            _ -> go (tok' : acc)

-- preprocessDefine :: Located Token -> P (Located Token)
-- preprocessDefine tok@(L l ITcppDefine) = do
--     L ll cond <- ppLexToken
--     -- ppDefine (show cond)
--     ppDefine (trace ("ppDefine:" ++ show cond) (show cond))
--     return (L l (ITcppIgnored [tok, L ll cond]))
-- preprocessDefine tok = return tok

preprocessIfDef :: Located Token -> P (Located Token)
preprocessIfDef tok@(L l ITcppIfdef) = do
    L ll cond <- ppLexToken
    defined <- ppIsDefined (show cond)
    if defined
        then do
            pushContext ITcppIfdef
            setAccepting True
        else setAccepting False
    return (L l (ITcppIgnored [tok, L ll cond]))
preprocessIfDef tok = return tok

preprocessElse :: Located Token -> P (Located Token)
preprocessElse tok@(L l _) = do
    accepting <- getAccepting
    setAccepting (not accepting)
    return (L l (ITcppIgnored [tok]))

preprocessEnd :: Located Token -> P (Located Token)
preprocessEnd tok@(L l _) = do
    -- TODO: nested context
    setAccepting True
    return (L l (ITcppIgnored [tok]))

-- ---------------------------------------------------------------------
-- Preprocessor state functions

data CppState
    = CppIgnoring
    | CppInDefine
    | CppInIfdef
    | CppNormal
    deriving (Show)

getCppState :: P CppState
getCppState = do
    context <- peekContext
    accepting <- getAccepting
    case context of
        ITcppDefine -> return CppInDefine
        ITcppIfdef -> return CppInIfdef
        _ ->
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

-- pp_context stack end -------------------

-- pp_pushed_back token start --------------

pushBack :: Located Token -> P ()
pushBack tok = P $ \s ->
    if isJust (pp_pushed_back (pp s))
        then
            PFailed
                $ s
        else -- { errors =
        --     ("pushBack: " ++ show tok ++ ", we already have a token:" ++ show (pp_pushed_back (pp s)))
        --         : errors s
        -- }

            let
                ppVal = pp s
                pp' = ppVal{pp_pushed_back = Just tok}
                s' = s{pp = pp'}
             in
                POk s' ()

-- | Destructive read of the pp_pushed back token (if any)
getPushBack :: P (Maybe (Located Token))
getPushBack = P $ \s ->
    POk s{pp = (pp s){pp_pushed_back = Nothing}} (pp_pushed_back (pp s))

-- | Get next token, which may be the pushed back one
ppLexToken :: P (Located Token)
ppLexToken = do
    mtok <- getPushBack
    case mtok of
        Just t -> return t
        Nothing -> do
            -- TODO: do we need this? Issues with ALR, comments, etc being bypassed
            (L sp tok) <- Lexer.lexToken
            return (L (mkSrcSpanPs sp) tok)

-- pp_pushed_back token end ----------------

-- definitions start --------------------

ppDefine :: String -> P ()
ppDefine def = P $ \s ->
    POk s{pp = (pp s){pp_defines = Set.insert def (pp_defines (pp s))}} ()

ppIsDefined :: String -> P Bool
ppIsDefined def = P $ \s ->
    POk s (Set.member def (pp_defines (pp s)))

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

printToks :: Int -> [Located Token] -> IO ()
printToks indent toks = mapM_ go toks
  where
    go (L _ (ITcppIgnored ts)) = do
      putStr "ITcppIgnored ["
      printToks (indent + 4) ts
      putStrLn "]"
    go (L _ tk) = putStrLn (show tk)

-- Testing

libdirNow :: LibDir
libdirNow = "/home/alanz/mysrc/git.haskell.org/worktree/bisect/_build/stage1/lib"

t0 :: IO ()
t0 = do
    tks <- parseString libdirNow "#define FOO\n#ifdef FOO\nx = 1\n#endif\n"
    -- putStrLn $ show (reverse $ map unLoc tks)
    printToks 0 (reverse tks)

t1 :: IO ()
t1 = do
    tks <- parseString libdirNow "data X = X\n"
    putStrLn $ show (reverse $ map unLoc tks)
