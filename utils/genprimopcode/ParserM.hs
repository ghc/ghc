{-# LANGUAGE BinaryLiterals #-}

module ParserM (
    -- Parser Monad
    ParserM(..), AlexInput(..), run_parser,
    -- Parser state
    St,
    StartCode, start_code, set_start_code,
    inc_brace_depth, dec_brace_depth,
    -- Tokens
    Token(..),
    -- Actions
    Action, andBegin, mkT, mkTv,
    -- Positions
    get_pos, show_pos,
    -- Input
    alexGetByte, alexInputPrevChar,
    -- Other
    happyError
 ) where

import Control.Applicative

import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail (..))

import Control.Monad (ap, liftM)
import Data.Bits ((.&.), (.|.), shiftR)
import Data.Char (ord)
import Data.Word (Word8)

-- Parser Monad
newtype ParserM a = ParserM (AlexInput -> St -> Either String (AlexInput, St, a))

instance Functor ParserM where
  fmap = liftM

instance Applicative ParserM where
  pure  a = ParserM $ \i s -> Right (i, s, a)
  (<*>)   = ap

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
run_parser (ParserM f)
 = \s -> case f (AlexInput init_pos [] s) init_state of
             Left es -> Left es
             Right (_, _, x) -> Right x

-- Parser state

data St = St {
              start_code :: !StartCode,
              brace_depth :: !Int
          }
    deriving Show
type StartCode = Int

init_state :: St
init_state = St {
                 start_code = 0,
                 brace_depth = 0
             }

-- Tokens

data Token = TEOF
           | TArrow
           | TDArrow
           | TEquals
           | TComma
           | TOpenParen
           | TCloseParen
           | TOpenParenHash
           | THashCloseParen
           | TOpenBrace
           | TCloseBrace
           | TOpenBracket
           | TCloseBracket
           | TOpenAngle
           | TCloseAngle
           | TSection
           | TPrimop
           | TPseudoop
           | TPrimtype
           | TWith
           | TDefaults
           | TTrue
           | TFalse
           | TCompare
           | TGenPrimOp
           | TByteArrayAccessOps
           | TAddrAccessOps
           | TThatsAllFolks
           | TLowerName String
           | TUpperName String
           | TString String
           | TNoBraces String
           | TInteger Int
           | TFixity
           | TInfixN
           | TInfixL
           | TInfixR
           | TNothing
           | TEffect
           | TNoEffect
           | TCanFail
           | TThrowsException
           | TReadWriteEffect
           | TCanFailWarnFlag
           | TDoNotWarnCanFail
           | TWarnIfEffectIsCanFail
           | TYesWarnCanFail
           | TVector
           | TSCALAR
           | TVECTOR
           | TVECTUPLE
           | TINTVECTUPLE
    deriving Show

-- Actions

type Action = String -> ParserM Token

set_start_code :: StartCode -> ParserM ()
set_start_code sc = ParserM $ \i st -> Right (i, st { start_code = sc }, ())

inc_brace_depth :: ParserM ()
inc_brace_depth = ParserM $ \i st ->
                  Right (i, st { brace_depth = brace_depth st + 1 }, ())

dec_brace_depth :: ParserM ()
dec_brace_depth = ParserM $ \i st ->
                  let bd = brace_depth st - 1
                      sc = if bd == 0 then 0 else 1
                  in Right (i, st { brace_depth = bd, start_code = sc }, ())

andBegin :: Action -> StartCode -> Action
(act `andBegin` sc) x = do set_start_code sc
                           act x

mkT :: Token -> Action
mkT t = mkTv (const t)

mkTv :: (String -> Token) -> Action
mkTv f str = ParserM (\i st -> Right (i, st, f str))

-- Positions

data Pos = Pos !Int{- Line -} !Int{- Column -}

get_pos :: ParserM Pos
get_pos = ParserM $ \i@(AlexInput p _ _) st -> Right (i, st, p)

alexMove :: Pos -> Char -> Pos
alexMove (Pos l _) '\n' = Pos (l+1) 1
alexMove (Pos l c) '\t' = Pos l ((c+8) `div` 8 * 8)
alexMove (Pos l c) _    = Pos l (c+1)

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

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (AlexInput p (w:ws) cs)
  = Just (w, AlexInput p ws cs)
alexGetByte (AlexInput p [] (c:cs))
  = alexGetByte (AlexInput (alexMove p c) (utf8_encode c) cs)
alexGetByte (AlexInput _ [] [])
  = Nothing

-- annoyingly, this doesn't seem to exist anywhere else as a standalone function
utf8_encode :: Char -> [Word8]
utf8_encode c = case ord c of
  n | n < 0x80    -> [ fromIntegral n ]
    | n < 0x800   -> [ fromIntegral $ 0b11000000 .|. (n `shiftR` 6)
                     , fromIntegral $ 0b10000000 .|. (n .&. 0b111111) ]
    | n < 0x10000 -> [ fromIntegral $ 0b11100000 .|. (n `shiftR` 12)
                     , fromIntegral $ 0b10000000 .|. ((n `shiftR` 6) .&. 0b111111)
                     , fromIntegral $ 0b10000000 .|. (n .&. 0b111111) ]
    | otherwise   -> [ fromIntegral $ 0b11110000 .|. (n `shiftR` 18)
                     , fromIntegral $ 0b10000000 .|. ((n `shiftR` 12) .&. 0b111111)
                     , fromIntegral $ 0b10000000 .|. ((n `shiftR` 6) .&. 0b111111)
                     , fromIntegral $ 0b10000000 .|. (n .&. 0b111111) ]

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

happyError :: ParserM a
happyError = do p <- get_pos
                fail $ "Parse error at " ++ show_pos p
