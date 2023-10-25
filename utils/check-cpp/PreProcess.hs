module PreProcess where

import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import GHC
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Data.StringBuffer
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), Token (..))
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.SrcLoc

import Debug.Trace

import Macro

-- import ParseOld
import Parse
import Types

-- ---------------------------------------------------------------------

type PP = P PpState

-- initPpState :: PpState
-- initPpState =
--     PpState
--         { pp_defines = Map.empty
--         , pp_includes = Map.empty
--         , pp_include_stack = []
--         , pp_continuation = []
--         , pp_context = []
--         , pp_accepting = True
--         }

-- data PpState = PpState
--     { pp_defines :: !(Map String [String])
--     , pp_includes :: !(Map String StringBuffer)
--     , pp_include_stack :: ![Lexer.AlexInput]
--     , pp_continuation :: ![Located Token]
--     , pp_context :: ![Token] -- What preprocessor directive we are currently processing
--     , pp_accepting :: !Bool
--     }
--     deriving (Show)

-- deriving instance Show Lexer.AlexInput
-- ---------------------------------------------------------------------

data CppState
    = CppIgnoring
    | CppNormal
    deriving (Show)

-- ---------------------------------------------------------------------

ppLexer, ppLexerDbg :: Bool -> (Located Token -> PP a) -> PP a
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
                    L _ ITeof -> do
                        mInp <- popIncludeLoc
                        case mInp of
                            Nothing -> contInner tk
                            Just inp -> do
                                Lexer.setInput inp
                                ppLexer queueComments cont
                    L _ (ITcpp continuation s) -> do
                        if continuation
                            then pushContinuation tk
                            else processCppToks s
                        contIgnoreTok tk
                    _ -> do
                        state <- getCppState
                        -- case (trace ("CPP state:" ++ show state) state) of
                        case state of
                            CppIgnoring -> contIgnoreTok tk
                            _ -> contInner tk
        )

-- ---------------------------------------------------------------------

preprocessElse :: PP ()
preprocessElse = do
    accepting <- getAccepting
    setAccepting (not accepting)

preprocessEnd :: PP ()
preprocessEnd = do
    -- TODO: nested context
    setAccepting True

processCppToks :: FastString -> PP ()
processCppToks fs = do
    let
        get (L _ (ITcpp _ s)) = s
        get _ = error "should not"
    -- Combine any prior continuation tokens
    cs <- popContinuation
    processCpp (reverse $ fs : map get cs)
    return ()

processCpp :: [FastString] -> PP ()
processCpp fs = do
    -- traceM $ "processCpp: fs=" ++ show fs
    -- let s = cppInitial fs
    let s = cppInitial fs
    case regularParse cppDirective s of
        Left err -> error $ show err
        Right (CppInclude filename) -> do
            ppInclude filename
        Right (CppDefine name def) -> do
            ppDefine name def
        Right (CppIf cond) -> do
            ppIf cond
            return ()
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

    -- return (trace ("processCpp:s=" ++ show s) ())
    return ()

-- ---------------------------------------------------------------------
-- Preprocessor state functions

getCppState :: PP CppState
getCppState = do
    accepting <- getAccepting
    if accepting
        then return CppNormal
        else return CppIgnoring

-- pp_context stack start -----------------

pushContext :: Token -> PP ()
pushContext new =
    P $ \s -> POk s{pp = (pp s){pp_context = new : pp_context (pp s)}} ()

popContext :: PP ()
popContext =
    P $ \s ->
        let
            new_context = case pp_context (pp s) of
                [] -> []
                (_ : t) -> t
         in
            POk s{pp = (pp s){pp_context = new_context}} ()

peekContext :: PP Token
peekContext =
    P $ \s ->
        let
            r = case pp_context (pp s) of
                [] -> ITeof -- Anthing really, for now, except a CPP one
                (h : _) -> h
         in
            POk s r

setAccepting :: Bool -> PP ()
setAccepting on =
    P $ \s -> POk s{pp = (pp s){pp_accepting = on}} ()

getAccepting :: PP Bool
getAccepting = P $ \s -> POk s (pp_accepting (pp s))

-- -------------------------------------

pushContinuation :: Located Token -> PP ()
pushContinuation new =
    P $ \s -> POk s{pp = (pp s){pp_continuation = new : pp_continuation (pp s)}} ()

popContinuation :: PP [Located Token]
popContinuation =
    P $ \s -> POk s{pp = (pp s){pp_continuation = []}} (pp_continuation (pp s))

-- pp_context stack end -------------------

-- pp_include start -----------------------

getInclude :: String -> PP (Maybe StringBuffer)
getInclude filename = P $ \s -> POk s (Map.lookup filename (pp_includes (pp s)))

pushIncludeLoc :: Lexer.AlexInput -> PP ()
pushIncludeLoc pos =
    P $ \s -> POk s{pp = (pp s){pp_include_stack = pos : pp_include_stack (pp s)}} ()

popIncludeLoc :: PP (Maybe Lexer.AlexInput)
popIncludeLoc =
    P $ \s ->
        let
            (new_st, r) = case pp_include_stack (pp s) of
                [] -> ([], Nothing)
                (h : t) -> (t, Just h)
         in
            POk s{pp = (pp s){pp_include_stack = new_st}} r

-- pp_include end -------------------------

-- definitions start --------------------

ppInclude :: String -> PP ()
ppInclude filename = do
    mSrc <- getInclude filename
    case mSrc of
        Nothing -> return ()
        Just src -> do
            origInput <- Lexer.getInput
            pushIncludeLoc origInput
            let loc = PsLoc (mkRealSrcLoc (mkFastString filename) 1 1) (BufPos 0)
            Lexer.setInput (Lexer.AI loc src)
    return $ trace ("ppInclude:mSrc=[" ++ show mSrc ++ "]") ()

-- return $ trace ("ppInclude:filename=[" ++ filename ++ "]") ()

ppDefine :: String -> [String] -> PP ()
ppDefine name val = P $ \s ->
    -- POk s{pp = (pp s){pp_defines = Set.insert (cleanTokenString def) (pp_defines (pp s))}} ()
    POk s{pp = (pp s){pp_defines = Map.insert (trace ("ppDefine:def=[" ++ name ++ "]") (MacroName name Nothing)) val (pp_defines (pp s))}} ()

ppIsDefined :: String -> PP Bool
ppIsDefined def = P $ \s ->
    -- POk s (Map.member def (pp_defines (pp s)))
    POk s (Map.member (trace ("ppIsDefined:def=[" ++ def ++ "]") (MacroName def Nothing)) (pp_defines (pp s)))

ppIf :: [String] -> PP Bool
ppIf toks = P $ \s ->
    -- -- POk s (Map.member def (pp_defines (pp s)))
    -- POk s (Map.member (trace ("ppIsDefined:def=[" ++ def ++ "]") def) (pp_defines (pp s)))
    let
        s' = cppIf (pp s) toks
     in
        POk s{pp = s'} (pp_accepting s')

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
