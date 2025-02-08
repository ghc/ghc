module PreProcess where

import Data.Char

-- import Data.Map (Map)
import qualified Data.Map as Map
import GHC
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Data.StringBuffer
import GHC.Parser.Errors.Ppr ()
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.SrcLoc

import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), Token (..))

import Macro
import ParsePP
import State

import Debug.Trace

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
                    L l (ITcpp continuation s) -> do
                        if continuation
                            then do
                                pushContinuation tk
                                contIgnoreTok tk
                            else do
                                mdump <- processCppToks s
                                case mdump of
                                    Just dump ->
                                        -- We have a dump of the state, put it into an ignored token
                                        contIgnoreTok (L l (ITcpp continuation (appendFS s (fsLit dump))))
                                    Nothing -> contIgnoreTok tk
                    _ -> do
                        state <- getCppState
                        -- case (trace ("CPP state:" ++ show state) state) of
                        case state of
                            CppIgnoring -> contIgnoreTok tk
                            _ -> contInner tk
        )

-- ---------------------------------------------------------------------

processCppToks :: FastString -> PP (Maybe String)
processCppToks fs = do
    let
        get (L _ (ITcpp _ s)) = s
        get _ = error "should not"
    -- Combine any prior continuation tokens
    cs <- popContinuation
    processCpp (reverse $ fs : map get cs)

processCpp :: [FastString] -> PP (Maybe String)
processCpp fs = do
    let s = concatMap unpackFS fs
    let directive = parseDirective s
    if directive == Right CppDumpState
        then return (Just "\ndumped state\n")
        else do
            case directive of
                Left err -> error $ show (err, s)
                Right (CppInclude filename) -> do
                    ppInclude filename
                Right (CppDefine name def) -> do
                    ppDefine (MacroName name Nothing) def
                Right (CppIf cond) -> do
                    ppIf cond
                Right (CppIfdef name) -> do
                    defined <- ppIsDefined (MacroName name Nothing)
                    pushAccepting defined
                Right (CppIfndef name) -> do
                    defined <- ppIsDefined (MacroName name Nothing)
                    pushAccepting (not defined)
                Right CppElse -> do
                    accepting <- getAccepting
                    setAccepting (not accepting)
                Right CppEndif -> do
                    popScope
                Right CppDumpState -> do
                    return ()
            -- accepting <- getAccepting
            -- return (trace ("processCpp:" ++ show (accepting,directive)) Nothing)
            return Nothing

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

ppIf :: String -> PP ()
ppIf str = P $ \s ->
    let
        s' = cppIf (pp s) str
     in
        POk s{pp = s'} ()

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
