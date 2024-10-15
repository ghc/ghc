{-# LANGUAGE MultiWayIf #-}

module GHC.Parser.Layouter where

import GHC.Prelude
import GHC.Hs
import GHC.Parser.Lexer
import GHC.Types.SrcLoc
import GHC.Utils.Panic
import Data.Maybe
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import GHC.Utils.Error
import GHC.Parser.Errors.Types
import Data.Sequence
import Debug.Trace

data LayItem
  = LayFlexi !PsSpan !LayHerald
  | LayImplicit !LayHerald !Int
    -- ^ We are inside an implicit layout block.
    -- We know the layout herald (e.g., `do`), the indentation level and have a
    -- list of pending closing delimiters (such as `)`, `]`, `||]`) that must
    -- be closed before closing the implicit layout context.
  | LayExplicit
    -- ^ We are inside an explicit layout block.
    -- We know which delimiters must be closed before closing the layout block.

data LayHerald
  = LetHerald
  | DoHerald
  | IfHerald -- ^ For -XMultiWayIf
  | RecHerald
  | WhereHerald
  | OfHerald
  | LCaseHerald
  | LCasesHerald
  deriving (Eq, Ord, Show)

type LayContext = (LayItem, [Token])

data LayState = LS
  { lay_stack   :: ![LayContext]
  , lay_output  :: !(Seq (PsLocated Token))
  }

initLayState :: LayState
initLayState = LS { lay_stack = [], lay_output = Empty }

overLayState :: (LayState -> (a, LayState)) -> P a
overLayState f = P $ \s -> case f (lay_state s) of (a, ls) -> POk s{lay_state=ls} a
{-# INLINE overLayState #-}
layouter, layouterDbg :: ((PsLocated Token -> P a) -> P a)
                      -> ((Located Token -> P a) -> P a)

getLayStack :: P [LayContext]
getLayStack = P $ \s -> POk s (lay_stack (lay_state s))

-- Use this instead of 'lexer' in GHC.Parser to dump the tokens for debugging.
layouterDbg lexer cont = layouter lexer contDbg
  where
    contDbg tok = trace ("token: " ++ show (unLoc tok)) (cont tok)

layouter lexer = runContT $ yieldOutput $ ContT lexer >>= lift . interp lay

data LayLang = PsLocated Token ::: LayLang | Done [LayContext]
infixr :::

interp :: ((ExtBits -> Bool) -> PsLocated Token -> [LayContext] -> LayLang) -> PsLocated Token -> P ()
interp lay tok = do
  stk <- getLayStack
  bits <- pExtsBitmap <$> getParserOpts
  go (lay (`xtest` bits) tok stk)
  where
    go (Done stk) = overLayState $ \s -> ((), s{lay_stack = stk})
    go (tok ::: l) = enqueueOutput tok >> go l

lay :: (ExtBits -> Bool) -> PsLocated Token -> [LayContext] -> LayLang
lay ext ltok@(L span tok) = lay_module
  where
    offset | ITeof <- tok = 0
           | otherwise    = srcSpanStartCol (psRealSpan span)
    indent_decreasing h old new
      | isLayHeraldNonDecreasing h      = old > new
      | ext NondecreasingIndentationBit = old > new
      | otherwise                       = old >= new

    lay_module []
      | ITocurly <- tok = ltok ::: Done [(LayExplicit,[])]
      | ITwhere <- tok  = ltok ::: Done [(LayFlexi span WhereHerald,[])]
      | otherwise       = ltok ::: Done []
    lay_module stk      = lay_bol stk

    lay_bol [] | ITeof <- tok = ltok ::: Done []
               | otherwise    = panic (show span ++ " " ++ show tok)
    lay_bol stk@((item,clos):stk') = case item of
      LayImplicit herald offs -> case compare offset offs of
        LT | LetHerald <- herald, ITin <- tok
           -> L span ITvccurly ::: ltok ::: Done stk' -- So that `in` does not close multiple blocks
           | otherwise
           -> L span ITvccurly ::: lay_bol stk' -- NB: pop item
        EQ | isInfixForm tok -- an (e.g.) `where` on the same indent would require something to the left of where; leave the current block
           -> L span ITvccurly ::: lay_bol stk'
           | generateSemis herald
           -> L span ITsemi    ::: lay_rest ((item,clos):stk')
        _  -> lay_rest ((item,clos):stk')
      LayFlexi span herald
        | ITocurly <- tok
        -> ltok ::: Done ((LayExplicit,[]):stk')
        | IfHerald <- herald, not (isVBar tok)
        -> lay_rest stk' -- Vanilla If -> no layout
        | (LayImplicit _ prev_off, _) : _ <- stk'
        , indent_decreasing herald prev_off offset
        -> L span ITvocurly ::: L span ITvccurly ::: lay_bol stk'
        | [] <- stk'
        , ITeof <- tok -- Directory.Internal.Windows: `module M where\n <eof>`
        -> L span ITvocurly ::: L span ITvccurly ::: lay_bol stk'
        | otherwise
        -> L span ITvocurly ::: lay_rest ((LayImplicit herald offset,[]):stk')
      LayExplicit{} -> lay_rest stk
    lay_rest stk = case tok of
      ITccurly | (LayExplicit,_):stk' <- stk -> ltok ::: Done stk'
               | (LayImplicit{},_):stk' <- stk -> L span ITvccurly ::: lay_rest stk'
      ITocurly -> ltok ::: Done ((LayExplicit,[]):stk)
      _ | (item@LayImplicit{},clos):stk' <- stk, isClosingDelim tok
        -> case clos of
             clo:clos | clo `eqClosingDelim` tok
               -> ltok ::: Done ((item,clos):stk')
             _ -> L span ITvccurly ::: lay_rest stk'
        | (LayImplicit LetHerald _,_):stk' <- stk, ITin <- tok
        -> L span ITvccurly ::: ltok ::: Done stk' -- for let .. in
        | (LayImplicit herald _,_):stk' <- stk, tok `killsLayoutOf` herald
        -> L span ITvccurly ::: lay_rest stk'
        | Just clo <- isOpeningDelim_maybe tok
        , (item,clos):stk' <- stk
        -> ltok ::: Done ((item,clo:clos):stk')
        | Just herald <- isLayHerald_maybe tok
        -> ltok ::: Done ((LayFlexi span herald,[]):stk)
        | otherwise
        -> ltok ::: Done stk

yieldOutput :: ContT r P () -> ContT r P (Located Token)
yieldOutput next = lift dequeueOutput >>= \mb_ltok -> case mb_ltok of
  Nothing -> next >> yieldOutput (panic "should not need to do next twice")
  Just (L span tok) -> return (L (mkSrcSpanPs span) tok)

enqueueOutput :: PsLocated Token -> P ()
enqueueOutput tk = overLayState $ \s -> trace ("token: " ++ show (unLoc tk)) ((), s{lay_output = lay_output s :|> tk})

dequeueOutput :: P (Maybe (PsLocated Token))
dequeueOutput = overLayState $ \s -> case lay_output s of
  Empty -> (Nothing, s)
  tk :<| tks -> (Just tk, s {lay_output = tks})

pushLayStack :: LayItem -> P ()
pushLayStack l = overLayState $ \s -> ((), s{lay_stack = (l,[]):lay_stack s})

popLayStack :: P (Maybe LayContext)
popLayStack = overLayState $ \s -> case lay_stack s of
  []    -> (Nothing, s)
  l:stk -> (Just l, s{lay_stack = stk})

pushClosingTok :: Token -> P ()
pushClosingTok tok = overLayState $ \s -> case lay_stack s of
  []           -> panic "impossible"
  (l,toks):stk -> ((), s{lay_stack = (l,tok:toks):stk})

popClosingTok :: P ()
popClosingTok = overLayState $ \s -> case lay_stack s of
  (l,_:toks):stk -> ((), s{lay_stack = (l,toks):stk})
  (l,[]):stk -> ((), s{lay_stack = (l,[]):stk}) -- genuinely can happen on error, I think. ex: `do foo)`
  []         -> panic "impossible"

isInfixForm :: Token -> Bool
isInfixForm ITwhere    = True
isInfixForm ITvarsym{} = True
isInfixForm _          = False

isOpeningDelim_maybe :: Token -> Maybe Token
isOpeningDelim_maybe IToparen = Just ITcparen
isOpeningDelim_maybe ITobrack = Just ITcbrack
-- isOpeningDelim_maybe ITocurly = Just ITccurly
isOpeningDelim_maybe IToubxparen = Just ITcubxparen
isOpeningDelim_maybe (IToparenbar uni) = Just (ITcparenbar uni)
isOpeningDelim_maybe (ITopenExpQuote _ uni) = Just (ITcloseQuote uni)
isOpeningDelim_maybe ITopenTypQuote = Just (ITcloseQuote NormalSyntax)
isOpeningDelim_maybe ITopenPatQuote = Just (ITcloseQuote NormalSyntax)
isOpeningDelim_maybe ITopenDecQuote = Just (ITcloseQuote NormalSyntax)
isOpeningDelim_maybe ITopenTExpQuote{} = Just ITcloseTExpQuote
isOpeningDelim_maybe _ = Nothing

isClosingDelim :: Token -> Bool
isClosingDelim ITcparen = True
isClosingDelim ITcbrack = True
-- isClosingDelim ITccurly = True
isClosingDelim ITcubxparen = True
isClosingDelim ITcparenbar{} = True
isClosingDelim ITcloseQuote{} = True
isClosingDelim ITcloseTExpQuote = True
isClosingDelim _ = False

eqClosingDelim :: Token -> Token -> Bool
eqClosingDelim ITcparen ITcparen = True
eqClosingDelim ITcbrack ITcbrack = True
-- eqClosingDelim ITccurly ITccurly = True
eqClosingDelim ITcubxparen ITcubxparen = True
eqClosingDelim (ITcparenbar uni1) (ITcparenbar uni2) = uni1 == uni2
eqClosingDelim (ITcloseQuote uni1) (ITcloseQuote uni2) = uni1 == uni2
eqClosingDelim ITcloseTExpQuote ITcloseTExpQuote = True
eqClosingDelim _ _ = False

separatesDelim :: Token -> Token -> Bool
separatesDelim ITcomma ITcparen    = True
separatesDelim ITcomma ITcbrack    = True
separatesDelim ITcomma ITcubxparen = True
separatesDelim _       _           = False

generateSemis :: LayHerald -> Bool
generateSemis IfHerald = False
generateSemis _        = True

isVBar :: Token -> Bool
isVBar ITvbar = True
isVBar _      = False

isPragma :: Token -> Bool
isPragma ITinline_prag{} = True
isPragma ITopaque_prag{} = True
isPragma ITspec_prag{} = True
isPragma ITspec_inline_prag{} = True
isPragma ITsource_prag{} = True
isPragma ITrules_prag{} = True
isPragma ITwarning_prag{} = True
isPragma ITdeprecated_prag{} = True
isPragma ITline_prag{} = True
isPragma ITcolumn_prag{} = True
isPragma ITscc_prag{} = True
isPragma ITunpack_prag{} = True
isPragma ITnounpack_prag{} = True
isPragma ITann_prag{} = True
isPragma ITcomplete_prag{} = True
isPragma IToptions_prag{} = True
isPragma ITinclude_prag{} = True
isPragma ITlanguage_prag = True
isPragma ITminimal_prag{} = True
isPragma IToverlappable_prag{} = True
isPragma IToverlapping_prag{} = True
isPragma IToverlaps_prag{} = True
isPragma ITincoherent_prag{} = True
isPragma ITctype{} = True
isPragma ITcomment_line_prag = True
isPragma _ = False

isLayHerald_maybe :: Token -> Maybe LayHerald
isLayHerald_maybe (ITdo _)  = Just DoHerald
isLayHerald_maybe (ITmdo _) = Just DoHerald
isLayHerald_maybe ITof      = Just OfHerald
isLayHerald_maybe ITlcase   = Just LCaseHerald
isLayHerald_maybe ITlcases  = Just LCasesHerald
isLayHerald_maybe ITlet     = Just LetHerald
isLayHerald_maybe ITwhere   = Just WhereHerald
isLayHerald_maybe ITrec     = Just RecHerald
isLayHerald_maybe ITif      = Just IfHerald
isLayHerald_maybe _         = Nothing

isLayHeraldNonDecreasing :: LayHerald -> Bool
isLayHeraldNonDecreasing DoHerald = True
isLayHeraldNonDecreasing _        = False

killsLayoutOf :: Token -> LayHerald -> Bool
killsLayoutOf ITin LetHerald = True
killsLayoutOf ITwhere DoHerald = True
killsLayoutOf ITin OfHerald = True
-- killsLayoutOf ITwhere OfHerald = True -- not true! `case x of True -> bar where ...; False -> foo where ...` is OK
killsLayoutOf _ _ = False
