-- Implement a subset of CPP, sufficient for conditional compilation
-- (only)
-- Note: this file formatted with fourmolu
{-# LANGUAGE BangPatterns #-}

module GHC.Parser.PreProcess (
    lexer,
    lexerDbg,
    initPpState,
    initParserState,
    initPragState,
    PpState,
    dumpGhcCpp,
) where

import Data.List (intercalate, sortBy)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Debug.Trace (trace)
import GHC.Data.FastString
import GHC.Data.Strict qualified as Strict
import GHC.Data.StringBuffer
import GHC.Driver.DynFlags (DynFlags, xopt)
import GHC.LanguageExtensions qualified as LangExt
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Errors.Types (PsMessage (PsErrGhcCpp))
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), Token (..))
import GHC.Parser.Lexer qualified as Lexer
import GHC.Parser.PreProcess.Macro
import GHC.Parser.PreProcess.ParsePP
import GHC.Parser.PreProcess.ParserM qualified as PM
import GHC.Parser.PreProcess.State
import GHC.Prelude
import GHC.Types.SrcLoc
import GHC.Utils.Error
import GHC.Utils.Outputable (text)
import GHC.Utils.Panic.Plain (panic)

-- ---------------------------------------------------------------------

dumpGhcCpp :: DynFlags -> PState PpState -> SDoc
dumpGhcCpp dflags pst = output
  where
    ghc_cpp_enabled = xopt LangExt.GhcCpp dflags
    output =
        if ghc_cpp_enabled
            then text $ sepa ++ defines ++ sepa ++ final ++ sepa
            else text "GHC_CPP not enabled"
    -- Note: pst is the state /before/ the parser runs, so we can use it to lex.
    (pst_final, bare_toks) = lexAll pst
    comments = reverse (Lexer.comment_q pst_final)
    -- We are going to addSourceToTokens, only need the location
    to_tok (L (EpaSpan l) _) = L l (ITunknown "-")
    to_tok (L (EpaDelta l _ _) _) = L l (ITunknown "-")
    comments_as_toks = map to_tok comments
    defines =
        showDefines (pp_defines (pp pst_final))
    sepa = "\n------------------------------\n"
    startLoc = mkRealSrcLoc (srcLocFile (psRealLoc $ loc pst)) 1 1
    buf1 = (buffer pst){cur = 0}
    all_toks =
        sortBy cmpBs (bare_toks ++ comments_as_toks)
    toks =
        addSourceToTokens startLoc buf1 all_toks
    final = renderCombinedToks toks

cmpBs :: Located Token -> Located Token -> Ordering
cmpBs (L (RealSrcSpan _ (Strict.Just bs1)) _) (L (RealSrcSpan _ (Strict.Just bs2)) _) =
    compare bs1 bs2
cmpBs (L (RealSrcSpan r1 _) _) (L (RealSrcSpan r2 _) _) =
    compare r1 r2
cmpBs _ _ = EQ

renderCombinedToks :: [(Located Token, String)] -> String
renderCombinedToks toks = showCppTokenStream toks

-- ---------------------------------------------------------------------
-- addSourceToTokens copied here to unbreak an import loop.
-- It should probably move somewhere else
-- TODO: We should be able to do away with this once #26095 is done

{- | Given a source location and a StringBuffer corresponding to this
location, return a rich token stream with the source associated to the
tokens.
-}
addSourceToTokens ::
    RealSrcLoc ->
    StringBuffer ->
    [Located Token] ->
    [(Located Token, String)]
addSourceToTokens _ _ [] = []
addSourceToTokens loc0 buf0 (t@(L sp _) : ts) =
    case sp of
        UnhelpfulSpan _ -> (t, "") : addSourceToTokens loc0 buf0 ts
        RealSrcSpan s _ -> (t, str) : addSourceToTokens newLoc newBuf ts
          where
            (newLoc, newBuf, str) = go "" loc0 buf0
            start = realSrcSpanStart s
            end = realSrcSpanEnd s
            go acc loc buf
                | loc < start = go acc nLoc nBuf
                | start <= loc && loc < end = go (ch : acc) nLoc nBuf
                | otherwise = (loc, buf, reverse acc)
              where
                (ch, nBuf) = nextChar buf
                nLoc = advanceSrcLoc loc ch

-- ---------------------------------------------------------------------

-- Tweaked from showRichTokenStream, to add markers per line if it is
-- currently active or not
showCppTokenStream :: [(Located Token, String)] -> String
showCppTokenStream ts0 = go startLoc ts0 ""
  where
    sourceFile = getFile $ map (getLoc . fst) ts0
    getFile [] = panic "showCppTokenStream: No source file found"
    getFile (UnhelpfulSpan _ : xs) = getFile xs
    getFile (RealSrcSpan s _ : _) = srcSpanFile s
    startLoc = mkRealSrcLoc sourceFile 0 1
    go _ [] = id
    go loc ((L span' tok, str) : ts) =
        case span' of
            UnhelpfulSpan _ -> go loc ts
            RealSrcSpan s _
                | locLine == tokLine ->
                    ((replicate (tokCol - locCol) ' ') ++)
                        . (str ++)
                        . go tokEnd ts
                | otherwise ->
                    ((replicate (tokLine - locLine) '\n') ++)
                        . (extra ++)
                        . ((replicate (tokCol - 1) ' ') ++)
                        . (str ++)
                        . go tokEnd ts
              where
                (locLine, locCol) = (srcLocLine loc, srcLocCol loc)
                (tokLine, tokCol) = (srcSpanStartLine s, srcSpanStartCol s)
                tokEnd = realSrcSpanEnd s
                extra = case tok of
                    ITunknown _ -> "- |"
                    _ -> "  |"

showDefines :: MacroDefines -> String
showDefines defines = Map.foldlWithKey' (\acc k d -> acc ++ "\n" ++ renderDefine k d) "" defines
  where
    renderDefine :: String -> (Map.Map (Maybe Int) ((Maybe MacroArgs), MacroDef)) -> String
    renderDefine k defs = Map.foldl' (\acc d -> acc ++ "\n" ++ renderArity k d) "" defs

    renderArity :: String -> ((Maybe MacroArgs), MacroDef) -> String
    renderArity n (Nothing, rhs) =
        "#define " ++ n ++ " " ++ (intercalate " " (map PM.t_str rhs))
    renderArity n (Just args, rhs) =
        "#define " ++ n ++ "(" ++ (intercalate "," args) ++ ") " ++ (intercalate " " (map PM.t_str rhs))

lexAll :: Lexer.PState PpState -> (Lexer.PState PpState, [Located Token])
lexAll state = case unP (lexer True return) state of
    POk s t@(L _ ITeof) -> (s, [t])
    POk state' t -> (ss, t : rest)
      where
        (ss, rest) = lexAll state'
    PFailed _pst -> panic $ "GHC.Parser.PreProcess.lexAll failed"

-- ---------------------------------------------------------------------

-- | Set parser options for parsing OPTIONS pragmas
initPragState :: Lexer.ParserOpts -> StringBuffer -> RealSrcLoc -> PState PpState
initPragState = Lexer.initPragState initPpState

-- | Creates a parse state from a 'ParserOpts' value
initParserState :: Lexer.ParserOpts -> StringBuffer -> RealSrcLoc -> PState PpState
initParserState = Lexer.initParserState initPpState

-- ---------------------------------------------------------------------

-- | Continuation based lexer, provides input to GHC.Parser
lexer :: Bool -> (Located Token -> PP a) -> PP a
lexer = ppLexer

-- | Debug version of @lexer@
lexerDbg :: Bool -> (Located Token -> PP a) -> PP a
lexerDbg = ppLexerDbg

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
                -- contInner t = (trace ("ppLexer: tk=" ++ show (unLoc tk, unLoc t)) cont) t
                contInner t = cont t
                contIgnoreTok (L l tok) = do
                    case l of
                        RealSrcSpan r (Strict.Just b) -> Lexer.queueIgnoredToken (L (PsSpan r b) tok)
                        _ -> return ()
                    ppLexer queueComments cont
             in
                case tk of
                    -- case (trace ("M.ppLexer:tk=" ++ show (unLoc tk)) tk) of
                    L _ ITeof -> do
                        mInp <- popIncludeLoc
                        case mInp of
                            Nothing -> contInner tk
                            Just inp -> do
                                Lexer.setInput inp
                                ppLexer queueComments cont
                    L l (ITcpp continuation s sp) -> do
                        ghcpp <- ghcCppEnabled
                        -- Only process the directive if GhcCpp is explicitly enabled.
                        -- Otherwise we are scanning for pragmas
                        if ghcpp
                            then
                                if continuation
                                    then do
                                        pushContinuation tk
                                        contIgnoreTok tk
                                    else do
                                        mdump <- processCppToks tk
                                        case mdump of
                                            Just dump ->
                                                -- We have a dump of the state, put it into an ignored token
                                                -- AZ: TODO: is this actually useful?
                                                contIgnoreTok (L l (ITcpp continuation (appendFS s (fsLit dump)) sp))
                                            Nothing -> contIgnoreTok tk
                            else contInner tk
                    L _ (ITcppIgnored _ _) -> contIgnoreTok tk
                    _ -> do
                        state <- getCppState
                        inLinePragma <- getInLinePragma
                        if inLinePragma
                            then do
                                case tk of
                                    L _ ITclose_prag -> setInLinePragma False
                                    _ -> return ()
                                contIgnoreTok tk
                            else
                                -- case (trace ("CPP state:" ++ show state) state) of
                                case state of
                                    CppIgnoring -> contIgnoreTok tk
                                    _ -> contInner tk
        )

-- ---------------------------------------------------------------------

processCppToks :: Located Lexer.Token -> PP (Maybe String)
processCppToks fs = do
    let
        get (L _ (ITcpp False s _)) = unpackFS s
        get (L _ (ITcpp True s _)) = init $ unpackFS s
        get _ = panic "Should be ITcpp"
    -- Combine any prior continuation tokens
    cs <- popContinuation
    let loc = combineLocs fs (fromMaybe fs (listToMaybe cs))
    processCpp loc (concat $ reverse $ map get (fs : cs))

processCpp :: SrcSpan -> String -> PP (Maybe String)
processCpp loc s = do
    let directive = parseDirective s
    if directive == Right CppDumpState
        then return (Just "\ndumped state\n")
        else do
            case directive of
                Left err -> Lexer.addError $ mkPlainErrorMsgEnvelope loc $ PsErrGhcCpp (text err)
                Right (CppInclude filename) -> do
                    ppInclude filename
                Right (CppDefine name args def) -> ppDefine (MacroName name args) def
                Right (CppUndef name) -> ppUndef name
                Right (CppIf cond) -> do
                    val <- cppCond loc cond
                    ar <- pushAccepting val
                    acceptStateChange ar
                Right (CppIfdef name) -> do
                    defined <- ppIsDefined (MacroName name Nothing)
                    ar <- pushAccepting defined
                    acceptStateChange ar
                Right (CppIfndef name) -> do
                    defined <- ppIsDefined (MacroName name Nothing)
                    ar <- pushAccepting (not defined)
                    acceptStateChange ar
                Right CppElse -> do
                    accepting <- getAccepting
                    ar <- setAccepting loc (text "#else") (not accepting)
                    acceptStateChange ar
                Right (CppElIf cond) -> do
                    val <- cppCond loc cond
                    ar <- setAccepting loc (text "#elif") val
                    acceptStateChange ar
                Right CppEndif -> do
                    ar <- popAccepting loc
                    acceptStateChange ar
                Right CppDumpState -> do
                    return ()
            return Nothing

acceptStateChange :: AcceptingResult -> PP ()
acceptStateChange ArNoChange = return ()
acceptStateChange ArNowIgnoring = do
    Lexer.startSkipping
acceptStateChange ArNowAccepting = do
    _ <- Lexer.stopSkipping
    return ()

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

-- =====================================================================

{- | Do cpp initial processing, as per https://gcc.gnu.org/onlinedocs/cpp/Initial-processing.html
See Note [GhcCPP Initial Processing]
-}

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

Note [GhcCPP Processing Overview]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC.Parser calls `GHC.PreProcess.lexer` to provide it with the next
token to parse, until it gets the EOF token.

Without GHC_CPP, this simply calls `GHC.Parser.Lexer.lexer` to get the
next token. But `GHC.PreProcess.lexer` runs its own loop between the
two.

- It calls `GHC.Parser.Lexer.lexer`

- If the GhcCpp option is not set, it returns a normal token, which is
  passed to the parser.

- If the GhcCpp option is set, it may in addition return an `ITcpp`
  token.

  This is either one containing a whole line starting with a
  preprocessor directive, or a continuation of the prior line if it
  was a directive ending with a backlash

- The lexing loop in this file accumulates these continuation tokens
  until it has a full preprocessor line.

- It does basic token-based analysis of this, to determine the
  specific PP directive it refers to

- The preprocessor can be in one of two states: `CppNormal` or
  `CppIgnoring`.

  When it is in `CppNormal` it passes non-PP tokens to the parser as
  normal.

  When it is in `CppIgnoring` it does not pass the non-PP tokens to
  the parser, but inserts them into the parser queued comments store,
  as if each was a comment.

- When it has a full preprocessor directive, this is processed as expected.
  `#define` : records a macro definition in the PP state
  `#include` : not currently processed

  `#ifdef` / `#ifndef` : If the following token is the name of a macro, switch to
  `CppNormal` or `CppIgnoring` as appropriate

  `#if` : perform macro expansion on the text, until it reaches a
  fixpoint. Then parse it with `GHC.Parser.PreProcess.Parser/Lexer` as
  an expression, and evaluate it. Set the state according to the outcome.

- The `#if` / `#ifdef` / `#ifndef` directives also open a new macro
  scope. Any macros defined will be stored in this scope.

- `#else` : flip the state between `CppIgnoring` and `CppNormal`, and
  pop the scope. Start a new scope.

- `#endif` : pop the scope, set the state according to the surrounding
  scope.

-}
