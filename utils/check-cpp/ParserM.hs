{-# LANGUAGE BinaryLiterals #-}

module ParserM (
    -- Parser Monad
    ParserM (..),
    AlexInput (..),
    run_parser,
    -- Parser state
    St,
    init_state,
    StartCode,
    start_code,
    setStartCode,
    -- Tokens
    Token (..),
    -- Actions
    Action,
    andBegin,
    mkT,
    mkTv,
    -- Positions
    init_pos,
    get_pos,
    show_pos,
    -- Input
    alexGetByte,
    alexInputPrevChar,
    -- Other
    happyError,
) where

import Control.Applicative

import Control.Monad.Fail (MonadFail (..))
import Prelude hiding (fail)

import Control.Monad (ap, liftM)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.Char (ord)
import Data.Word (Word8)

-- Parser Monad
newtype ParserM a = ParserM {unParserM :: AlexInput -> St -> Either String (AlexInput, St, a)}

-- newtype P a       = P     { unP :: PState -> ParseResult a }

instance Functor ParserM where
    fmap = liftM

instance Applicative ParserM where
    pure a = ParserM $ \i s -> Right (i, s, a)
    (<*>) = ap

instance Monad ParserM where
    ParserM m >>= k = ParserM $ \i s -> case m i s of
        Right (i', s', x) ->
            case k x of
                ParserM y -> y i' s'
        Left err ->
            Left err

instance MonadFail ParserM where
    fail err = ParserM $ \_ _ -> Left err

run_parser :: ParserM a -> (String -> Either String a)
run_parser (ParserM f) =
    \s -> case f (AlexInput init_pos [] s) init_state of
        Left es -> Left es
        Right (_, _, x) -> Right x

-- Parser state

data St = St
    { start_code :: !StartCode
    , brace_depth :: !Int
    }
    deriving (Show)
type StartCode = Int

init_state :: St
init_state =
    St
        { start_code = 0
        , brace_depth = 0
        }

-- Tokens

data Token
    = TEOF {t_str :: String}
    | TOpenBrace {t_str :: String}
    | TCloseBrace {t_str :: String}
    | TOpenBracket {t_str :: String}
    | TCloseBracket {t_str :: String}
    | THash {t_str :: String}
    | THashHash {t_str :: String}
    | TOpenParen {t_str :: String}
    | TCloseParen {t_str :: String}
    | TLtColon {t_str :: String}
    | TColonGt {t_str :: String}
    | TLtPercent {t_str :: String}
    | TPercentGt {t_str :: String}
    | TPercentColon {t_str :: String}
    | TPercentColonTwice {t_str :: String}
    | TSemi {t_str :: String}
    | TColon {t_str :: String}
    | TDotDotDot {t_str :: String}
    | TNew {t_str :: String}
    | TDelete {t_str :: String}
    | TQuestion {t_str :: String}
    | TColonColon {t_str :: String}
    | TDot {t_str :: String}
    | TDotStar {t_str :: String}
    | TPlus {t_str :: String}
    | TMinus {t_str :: String}
    | TStar {t_str :: String}
    | TSlash {t_str :: String}
    | TPercent {t_str :: String}
    | TUpArrow {t_str :: String}
    | TAmpersand {t_str :: String}
    | TPipe {t_str :: String}
    | TTilde {t_str :: String}
    | TExclamation {t_str :: String}
    | TEqual {t_str :: String}
    | TOpenAngle {t_str :: String}
    | TCloseAngle {t_str :: String}
    | TPlusEqual {t_str :: String}
    | TMinusEqual {t_str :: String}
    | TStarEqual {t_str :: String}
    | TSlashEqual {t_str :: String}
    | TPercentEqual {t_str :: String}
    | TUpEqual {t_str :: String}
    | TAmpersandEqual {t_str :: String}
    | TPipeEqual {t_str :: String}
    | TLtLt {t_str :: String}
    | TGtGt {t_str :: String}
    | TGtGtEqual {t_str :: String}
    | TLtLtEqual {t_str :: String}
    | TEqualEqual {t_str :: String}
    | TExclaimEqual {t_str :: String}
    | TLtEqual {t_str :: String}
    | TGtEqual {t_str :: String}
    | TAmpersandTwice {t_str :: String}
    | TPipePipe {t_str :: String}
    | TPlusPlus {t_str :: String}
    | TMinusMinus {t_str :: String}
    | TComma {t_str :: String}
    | TMinusGtStar {t_str :: String}
    | TMinusGt {t_str :: String}
    | TAnd {t_str :: String}
    | TAndEq {t_str :: String}
    | TBitand {t_str :: String}
    | TBitor {t_str :: String}
    | TCompl {t_str :: String}
    | TNot {t_str :: String}
    | TNotEq {t_str :: String}
    | TOr {t_str :: String}
    | TOrEq {t_str :: String}
    | TXor {t_str :: String}
    | TXorEq {t_str :: String}
    | TLowerName {t_str :: String}
    | TUpperName {t_str :: String}
    | TString {t_str :: String}
    | TInteger {t_str :: String}
    | TOther {t_str :: String}
    deriving (Show)

-- Actions

type Action = String -> ParserM Token

setStartCode :: StartCode -> ParserM ()
setStartCode sc = ParserM $ \i st -> Right (i, st{start_code = sc}, ())

andBegin :: Action -> StartCode -> Action
(act `andBegin` sc) x = do
    setStartCode sc
    act x

mkT :: Token -> Action
mkT t = mkTv (const t)

mkTv :: (String -> Token) -> Action
mkTv f str = ParserM (\i st -> Right (i, st, f str))

-- begin :: Int -> Action
-- begin sc _span _buf _len _buf2 =
--   do setStartCode sc
--      lex_tok

-- Positions

data Pos = Pos !Int {- Line -} !Int {- Column -}
    deriving (Show)

get_pos :: ParserM Pos
get_pos = ParserM $ \i@(AlexInput p _ _) st -> Right (i, st, p)

alexMove :: Pos -> Char -> Pos
alexMove (Pos l _) '\n' = Pos (l + 1) 1
alexMove (Pos l c) '\t' = Pos l ((c + 8) `div` 8 * 8)
alexMove (Pos l c) _ = Pos l (c + 1)

init_pos :: Pos
init_pos = Pos 1 0

show_pos :: Pos -> String
show_pos (Pos l c) = "line " ++ show l ++ ", column " ++ show c

-- Input

data AlexInput = AlexInput
    { position :: !Pos
    , char_bytes :: [Word8]
    , input :: String
    }
    deriving (Show)

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p (w : ws) cs) =
    Just (w, AlexInput p ws cs)
alexGetByte (AlexInput p [] (c : cs)) =
    alexGetByte (AlexInput (alexMove p c) (utf8_encode c) cs)
alexGetByte (AlexInput _ [] []) =
    Nothing

-- annoyingly, this doesn't seem to exist anywhere else as a standalone function
utf8_encode :: Char -> [Word8]
utf8_encode c = case ord c of
    n
        | n < 0x80 -> [fromIntegral n]
        | n < 0x800 ->
            [ fromIntegral $ 0b11000000 .|. (n `shiftR` 6)
            , fromIntegral $ 0b10000000 .|. (n .&. 0b111111)
            ]
        | n < 0x10000 ->
            [ fromIntegral $ 0b11100000 .|. (n `shiftR` 12)
            , fromIntegral $ 0b10000000 .|. ((n `shiftR` 6) .&. 0b111111)
            , fromIntegral $ 0b10000000 .|. (n .&. 0b111111)
            ]
        | otherwise ->
            [ fromIntegral $ 0b11110000 .|. (n `shiftR` 18)
            , fromIntegral $ 0b10000000 .|. ((n `shiftR` 12) .&. 0b111111)
            , fromIntegral $ 0b10000000 .|. ((n `shiftR` 6) .&. 0b111111)
            , fromIntegral $ 0b10000000 .|. (n .&. 0b111111)
            ]

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

happyError :: ParserM a
happyError = do
    p <- get_pos
    fail $ "Parse error at " ++ show_pos p
