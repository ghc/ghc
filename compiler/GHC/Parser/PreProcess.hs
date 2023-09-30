-- Implement a subset of CPP, sufficient for conditional compilation
-- (only)
-- Note: this file formatted with fourmolu
{-# LANGUAGE BangPatterns #-}

module GHC.Parser.PreProcess (
    ppLexer,
    ppLexerDbg,
    lexer,
    lexerDbg,
) where

import Data.Char
import qualified Data.Set as Set
import Debug.Trace (trace)
import GHC.Data.FastString
import qualified GHC.Data.Strict as Strict
import GHC.Parser.Errors.Ppr ()
import GHC.Parser.Lexer (P (..), PState (..), ParseResult (..), PpState (..), Token (..))
import qualified GHC.Parser.Lexer as Lexer
import GHC.Prelude
import GHC.Types.SrcLoc

-- ---------------------------------------------------------------------

lexer, lexerDbg :: Bool -> (Located Token -> P a) -> P a
-- bypass for now, work in ghci
lexer = Lexer.lexer
lexerDbg = Lexer.lexerDbg

-- lexer = ppLexer
-- -- lexer = ppLexerDbg
-- lexerDbg = ppLexerDbg

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
                -- contInner t = (trace ("ppLexer: tk=" ++ show (unLoc tk, unLoc t)) cont) t
                contInner t = cont t
                contPush = pushContext (unLoc tk) >> contIgnoreTok tk
                contIgnoreTok (L l tok) = do
                    case l of
                        RealSrcSpan r (Strict.Just b) -> Lexer.queueIgnoredToken (L (PsSpan r b) tok)
                        _ -> return ()
                    ppLexer queueComments cont
             in
                case tk of
                    L _ (ITcppDefine s) -> do
                        ppDefine (trace ("ppDefine:" ++ show s) s)
                        popContext
                        contIgnoreTok tk
                    L _ (ITcppIf _) -> contPush
                    L _ (ITcppIfdef s) -> do
                        defined <- ppIsDefined s
                        -- setAccepting defined
                        setAccepting (trace ("ifdef:" ++ show (s, defined)) defined)
                        contIgnoreTok tk
                    L _ (ITcppIfndef s) -> do
                        defined <- ppIsDefined s
                        -- setAccepting (not defined)
                        setAccepting (trace ("ifdef:" ++ show (s, defined)) (not defined))
                        contIgnoreTok tk
                    L _ ITcppElse -> do
                        preprocessElse
                        contIgnoreTok tk
                    L _ ITcppEndif -> do
                        preprocessEnd
                        contIgnoreTok tk
                    _ -> do
                        state <- getCppState
                        -- case (trace ("CPP state:" ++ show state) state) of
                        case state of
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

-- ---------------------------------------------------------------------
-- Preprocessor state functions

data CppState
    = CppIgnoring
    | CppInDefine
    | CppInIfdef
    | CppInIfndef
    | CppNormal
    deriving (Show)

getCppState :: P CppState
getCppState = do
    context <- peekContext
    accepting <- getAccepting
    case context of
        ITcppDefine _ -> return CppInDefine
        ITcppIfndef _ -> return CppInIfndef
        ITcppIfdef _ -> return CppInIfdef
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
            POk s{pp = (pp s){pp_context = (trace ("pop:new_context:" ++ show new_context) new_context)}} ()

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

-- definitions start --------------------

ppDefine :: FastString -> P ()
ppDefine def = P $ \s ->
    -- POk s{pp = (pp s){pp_defines = Set.insert (cleanTokenString def) (pp_defines (pp s))}} ()
    POk s{pp = (pp s){pp_defines = Set.insert (trace ("ppDefine:def=[" ++ show (cleanTokenString def) ++ "]") (cleanTokenString def)) (pp_defines (pp s))}} ()

ppIsDefined :: FastString -> P Bool
ppIsDefined def = P $ \s ->
    -- POk s (Set.member def (pp_defines (pp s)))
    POk s (Set.member (trace ("ppIsDefined:def=[" ++ show (cleanTokenString def) ++ "]") (cleanTokenString def)) (pp_defines (pp s)))

-- | Take a @FastString@ of the form "#define FOO\n" and strip off all but "FOO"
cleanTokenString :: FastString -> String
cleanTokenString fs = r
  where
    ss = dropWhile (\c -> not $ isSpace c) (unpackFS fs)
    r = init ss

-- definitions end --------------------
