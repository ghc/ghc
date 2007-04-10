
module ParserM (
    -- Parser Monad
    ParserM(..), AlexInput, run_parser,
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
    alexGetChar, alexInputPrevChar, input, position,
    -- Other
    happyError
 ) where

import Syntax

-- Parser Monad
newtype ParserM a = ParserM (AlexInput -> St -> Either String (AlexInput, St, a))

instance Monad ParserM where
    ParserM m >>= k = ParserM $ \i s -> case m i s of
                                            Right (i', s', x) ->
                                                case k x of
                                                    ParserM y -> y i' s'
                                            Left err ->
                                                Left err
    return a = ParserM $ \i s -> Right (i, s, a)
    fail err = ParserM $ \_ _ -> Left err

run_parser :: ParserM a -> (String -> Either String a)
run_parser (ParserM f)
 = \s -> case f (AlexInput init_pos s) init_state of
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
           | TEquals
           | TComma
           | TOpenParen
           | TCloseParen
           | TOpenParenHash
           | THashCloseParen
           | TOpenBrace
           | TCloseBrace
           | TSection
           | TPrimop
           | TPseudoop
           | TPrimtype
           | TWith
           | TDefaults
           | TTrue
           | TFalse
           | TDyadic
           | TMonadic
           | TCompare
           | TGenPrimOp
           | TThatsAllFolks
           | TLowerName String
           | TUpperName String
           | TString String
           | TNoBraces String
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
get_pos = ParserM $ \i@(AlexInput p _) st -> Right (i, st, p)

alexMove :: Pos -> Char -> Pos
alexMove (Pos l _) '\n' = Pos (l+1) 1
alexMove (Pos l c) '\t' = Pos l ((c+8) `div` 8 * 8)
alexMove (Pos l c) _    = Pos l (c+1)

init_pos :: Pos
init_pos = Pos 1 1

show_pos :: Pos -> String
show_pos (Pos l c) = "line " ++ show l ++ ", column " ++ show c

-- Input

data AlexInput = AlexInput {position :: !Pos, input :: String}

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (AlexInput p (x:xs)) = Just (x, AlexInput (alexMove p x) xs)
alexGetChar (AlexInput _ []) = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "Lexer doesn't implement alexInputPrevChar"

happyError :: ParserM a
happyError = do p <- get_pos
                fail $ "Parse error at " ++ show_pos p

