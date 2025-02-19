module PreProcess where

import Control.Monad.IO.Class
import Data.Data hiding (Fixity)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace
import GHC
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Data.StringBuffer
import GHC.Driver.Config.Parser hiding (predefinedMacros)
import GHC.Driver.Env.Types
import GHC.Driver.Errors.Types
import qualified GHC.Driver.Errors.Types as GHC
import qualified GHC.Driver.Session as GHC
import GHC.Hs.Dump
import qualified GHC.LanguageExtensions as LangExt
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), Token (..))
import qualified GHC.Parser.Lexer as GHC
import qualified GHC.Parser.Lexer as Lexer
import GHC.SysTools.Cpp
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Unit.Env
import GHC.Unit.State
import GHC.Utils.Error
import GHC.Utils.Outputable

import Macro
import ParsePP
import qualified ParserM as PM
import State

import Debug.Trace

-- ---------------------------------------------------------------------

dumpGhcCpp :: PState PpState -> SDoc
dumpGhcCpp pst = text $ sep ++ defines ++ sep ++ comments ++ sep ++ orig ++ sep ++ final ++ sep
  -- ++ show startLoc ++ sep ++ show bare_toks ++ sep
  where
    -- Note: pst is the state /before/ the parser runs, so we can use it to lex.
    (pst_final, bare_toks) = lexAll pst
    defines = showDefines (pp_defines (pp pst_final))
    sep = "\n------------------------------\n"
    comments = showPprUnsafe (Lexer.comment_q pst_final)
    buf = (buffer pst){cur = 0}
    orig = lexemeToString buf (len buf)
    startLoc = mkRealSrcLoc (srcLocFile (psRealLoc $ loc pst)) 1 1
    buf1 = (buffer pst){cur = 0}
    toks = GHC.addSourceToTokens startLoc buf1 bare_toks
    final = renderCombinedToks toks (Lexer.comment_q pst_final)

renderCombinedToks :: [(Located Token, String)] -> [LEpaComment] -> String
renderCombinedToks toks ctoks = show toks1 ++ show ctoks
  where
    toks1 = map (\(L l _, s) -> (l,s)) toks
    ctoks1 = map (\(L l t) -> (l, ghcCommentText t)) ctoks


ghcCommentText :: EpaComment -> String
ghcCommentText (GHC.EpaComment (EpaDocComment s) _)      = exactPrintHsDocString s
ghcCommentText (GHC.EpaComment (EpaDocOptions s) _)      = s
ghcCommentText (GHC.EpaComment (EpaLineComment s) _)     = s
ghcCommentText (GHC.EpaComment (EpaBlockComment s) _)    = s
ghcCommentText (GHC.EpaComment (EpaCppIgnored [L _ s]) _)= s
ghcCommentText (GHC.EpaComment (EpaCppIgnored _) _)      = ""

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
lexAll state = case unP (lexerDbg True return) state of
    POk s t@(L _ ITeof) -> (s, [t])
    POk state' t -> (ss, t : rest)
      where
        (ss, rest) = lexAll state'
    PFailed pst -> error $ "failed" ++ showErrorMessages (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)

showErrorMessages :: Messages GhcMessage -> String
showErrorMessages msgs =
    renderWithContext defaultSDocContext $
        vcat $
            pprMsgEnvelopeBagWithLocDefault $
                getMessages $
                    msgs

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
                Right (CppDefine name args def) -> do
                    ppDefine (MacroName name args) def
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
