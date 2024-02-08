{-# LANGUAGE LambdaCase #-}

module GHC.Parser.String (
  LexedString,
  LexedChar (..),
  StringLexError (..),
  resolveLexedString,
  resolveEscapeCharacter,

  -- * Unicode smart quote helpers
  isDoubleSmartQuote,
  isSingleSmartQuote,
) where

import GHC.Prelude

import Control.Monad (guard, unless, when, (>=>))
import Data.Char (chr, isSpace, ord)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Parser.CharClass (
  hexDigit,
  is_decdigit,
  is_hexdigit,
  is_octdigit,
  octDecDigit,
 )
import GHC.Parser.Errors.Types (LexErr (..))
import GHC.Utils.Panic (panic)

data LexedChar loc = LexedChar !Char !loc
type LexedString loc = [LexedChar loc]

unLexedChar :: LexedChar loc -> Char
unLexedChar (LexedChar c _) = c

unLexedString :: LexedString loc -> String
unLexedString = map unLexedChar

-- | Apply the given StringProcessors to the given LexedString left-to-right,
-- and return the processed string.
resolveLexedString ::
  LexedString loc ->
  Either (StringLexError loc) String
resolveLexedString = fmap unLexedString . foldr (>=>) pure processString
  where
    processString =
      [ collapseStringGaps
      , resolveEscapeCharacters
      ]

data StringLexError loc
  = SmartQuoteError !Char !loc
  | StringLexError !Char !loc !LexErr

type StringProcessor loc = LexedString loc -> Either (StringLexError loc) (LexedString loc)

collapseStringGaps :: StringProcessor loc
collapseStringGaps s0 = pure (go s0)
  where
    go = \case
      [] -> []

      backslash@(LexedChar '\\' _) : c : s
        | isLexedSpace c ->
            -- lexer should have validated that this is a valid gap,
            -- so we'll panic if we find any invalid characters
            case dropWhile isLexedSpace s of
              LexedChar '\\' _ : s -> go s
              _ -> panic $ "Invalid string gap in " ++ show (unLexedString s0)
        | otherwise ->
            backslash : c : go s

      c : s -> c : go s

    isLexedSpace = isSpace . unLexedChar

resolveEscapeCharacters :: StringProcessor loc
resolveEscapeCharacters = go
  where
    go = \case
      [] -> pure []
      LexedChar '\\' _ : LexedChar '&' _ : s -> go s
      backslashChar@(LexedChar '\\' _) : s -> do
        (c, s') <- resolveEscapeCharacter backslashChar s
        (c :) <$> go s'
      c : s ->
        (c :) <$> go s

-- | After finding a backslash, parse the rest of the escape character.
resolveEscapeCharacter ::
  LexedChar loc ->   -- the backslash character
  LexedString loc -> -- the rest of the string to parse
  Either
    (StringLexError loc)
    (LexedChar loc, LexedString loc) -- the resolved escape character and the rest of the string
resolveEscapeCharacter backslashChar s0 = do
  (firstChar@(LexedChar c loc), s1) <- expectNext backslashChar s0
  let rewrap c' = pure (LexedChar c' loc, s1)
  case c of
    'a'  -> rewrap '\a'
    'b'  -> rewrap '\b'
    'f'  -> rewrap '\f'
    'n'  -> rewrap '\n'
    'r'  -> rewrap '\r'
    't'  -> rewrap '\t'
    'v'  -> rewrap '\v'
    '\\' -> rewrap '\\'
    '"'  -> rewrap '\"'
    '\'' -> rewrap '\''
    -- escape codes
    'x' -> expectNum is_hexdigit 16 hexDigit (firstChar, s1)
    'o' -> expectNum is_octdigit 8 octDecDigit (firstChar, s1)
    _ | is_decdigit c -> expectNum is_decdigit 10 octDecDigit (backslashChar, s0)
    -- control characters (e.g. '\^M')
    '^' -> do
      (LexedChar c1 loc1, s2) <- expectNext firstChar s1
      unless (c1 >= '@' && c1 <= '_') $
        Left $ StringLexError c1 loc1 LexStringCharLit
      let c' = chr $ ord c1 - ord '@'
      pure (LexedChar c' loc, s2)
    -- long form escapes (e.g. '\NUL')
    _ | Just (c', s2) <- parseLongEscape firstChar s1 -> pure (LexedChar c' loc, s2)
    -- check unicode smart quotes (#21843)
    _ | isDoubleSmartQuote c -> Left $ SmartQuoteError c loc
    _ | isSingleSmartQuote c -> Left $ SmartQuoteError c loc
    -- unknown escape
    _ -> Left $ StringLexError c loc LexStringCharLit
  where
    expectNext lastChar = \case
      [] -> do
        let LexedChar c loc = lastChar
        Left $ StringLexError c loc LexStringCharLitEOF
      c : cs -> pure (c, cs)

    expectNum isDigit base toDigit (lastChar, s0) = do
      (LexedChar c loc, s1) <- expectNext lastChar s0
      unless (isDigit c) $ Left $ StringLexError c loc LexStringCharLit
      let parseNum x = \case
            LexedChar c' loc' : s' | isDigit c' -> do
              let x' = x * base + toDigit c'
              when (x' > 0x10ffff) $ Left $ StringLexError c' loc' LexNumEscapeRange
              parseNum x' s'
            s ->
              pure (LexedChar (chr x) loc, s)
      parseNum (toDigit c) s1

-- | Check if the escape characters match a long escape code.
--
-- >>> parseLongEscape 'C' [LexedChar 'R', LexedChar 'X', ...s] = Just ('\CR', [LexedChar 'X', ...s])
-- >>> parseLongEscape 'X' [LexedChar 'X', LexedChar 'X', ...s] = Nothing
parseLongEscape :: LexedChar loc -> LexedString loc -> Maybe (Char, LexedString loc)
parseLongEscape (LexedChar c _) s = listToMaybe $ mapMaybe tryParse longEscapeCodes
  where
    tryParse (prefix, c') = do
      p0 : p <- pure prefix
      guard (p0 == c)       -- see if the first character matches
      s' <- parsePrefix p s -- see if the rest of the prefix matches
      pure (c', s')

    parsePrefix (p : ps) (LexedChar t _ : ts) | p == t = parsePrefix ps ts
    parsePrefix [] s' = Just s' -- we've matched the whole prefix, return the rest
    parsePrefix _ _ = Nothing

    longEscapeCodes =
      [ ("NUL", '\NUL')
      , ("SOH", '\SOH')
      , ("STX", '\STX')
      , ("ETX", '\ETX')
      , ("EOT", '\EOT')
      , ("ENQ", '\ENQ')
      , ("ACK", '\ACK')
      , ("BEL", '\BEL')
      , ("BS", '\BS')
      , ("HT", '\HT')
      , ("LF", '\LF')
      , ("VT", '\VT')
      , ("FF", '\FF')
      , ("CR", '\CR')
      , ("SO", '\SO')
      , ("SI", '\SI')
      , ("DLE", '\DLE')
      , ("DC1", '\DC1')
      , ("DC2", '\DC2')
      , ("DC3", '\DC3')
      , ("DC4", '\DC4')
      , ("NAK", '\NAK')
      , ("SYN", '\SYN')
      , ("ETB", '\ETB')
      , ("CAN", '\CAN')
      , ("EM", '\EM')
      , ("SUB", '\SUB')
      , ("ESC", '\ESC')
      , ("FS", '\FS')
      , ("GS", '\GS')
      , ("RS", '\RS')
      , ("US", '\US')
      , ("SP", '\SP')
      , ("DEL", '\DEL')
      ]

-- -----------------------------------------------------------------------------
-- Unicode Smart Quote detection (#21843)

isDoubleSmartQuote :: Char -> Bool
isDoubleSmartQuote = \case
  '“' -> True
  '”' -> True
  _ -> False

isSingleSmartQuote :: Char -> Bool
isSingleSmartQuote = \case
  '‘' -> True
  '’' -> True
  _ -> False
