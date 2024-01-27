{-# LANGUAGE LambdaCase #-}

module GHC.Parser.String (
  StringLexError (..),
  ContainsSmartQuote (..),
  LexStringType (..),
  lexString,

  -- * Unicode smart quote helpers
  isDoubleSmartQuote,
  isSingleSmartQuote,

  -- * Other helpers
  isAnyChar,
  resolveEscapeCharacter,
) where

import GHC.Prelude

import Control.Arrow ((>>>))
import Control.Monad (guard, unless, when)
import Data.Char (chr, isPrint, ord)
import Data.List (unfoldr)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Parser.CharClass (
  hexDigit,
  is_any,
  is_decdigit,
  is_hexdigit,
  is_octdigit,
  is_space,
  octDecDigit,
 )
import GHC.Utils.Panic (panic)

data LexStringType = StringTypeSingle | StringTypeMulti

-- | State to accumulate while iterating through string literal.
--
-- Fields are strict here to avoid space leak when iterating through large string literal
-- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12089#note_576175
data LexStringState loc = LexStringState
  { stringAcc :: !String
    -- ^ The string seen so far, reversed
  , multilineCommonWsPrefix :: !Int
    -- ^ The common prefix for multiline strings. See Note [Multiline string literals]
  , initialLoc :: !loc
    -- ^ The location of the beginning of the string literal
  }

-- | Get the character at the given location, with the location
-- of the next character. Returns Nothing if at the end of the
-- input.
type GetChar loc = loc -> Maybe (Char, loc)

lexString :: LexStringType -> GetChar loc -> loc -> Either (StringLexError loc) (String, loc)
lexString strType getChar initialLoc = go initialState initialLoc
  where
    initialState =
      LexStringState
        { stringAcc = ""
        , multilineCommonWsPrefix =
            case strType of
              StringTypeMulti -> maxBound
              _ -> 0
        , initialLoc = initialLoc
        }

    -- 's' is strict here to avoid space leak when iterating through large string literal
    -- https://gitlab.haskell.org/ghc/ghc/-/merge_requests/12089#note_576175
    go !s loc0 =
      case getChar loc0 of
        -- found closing delimiter
        Just ('"', _) | Just loc1 <- checkDelimiter strType getChar loc0 -> do
          let postprocess =
                case strType of
                  StringTypeSingle -> id
                  StringTypeMulti -> postprocessMultiline (multilineCommonWsPrefix s)
          Right (postprocess . reverse $ stringAcc s, loc1)

        -- found backslash
        Just (c0@'\\', loc1) -> do
          case getChar loc1 of
            -- found '\&' character, which should be elided
            Just ('&', loc2) -> go s loc2
            -- found start of a string gap
            Just (c1, loc2) | is_space c1 -> collapseStringGap getChar s loc2 >>= go s
            -- some other escape character
            Just (c1, loc2) ->
              case strType of
                StringTypeSingle -> do
                  (c', loc') <- resolveEscapeCharacter getChar loc1
                  go (addChar c' s) loc'
                StringTypeMulti -> do
                  -- keep escape characters unresolved until after post-processing,
                  -- to distinguish between a user-newline and the user writing "\n".
                  -- but still process the characters here, to find any errors
                  _ <- resolveEscapeCharacter getChar loc1
                  go (addChar c1 . addChar c0 $ s) loc2
            -- backslash at end of input
            Nothing -> Left $ BadCharInitialLex loc1 (hasSQuote getChar s)

        -- found newline character in multiline string
        Just (c0@'\n', loc1) | StringTypeMulti <- strType ->
          uncurry go $ parseLeadingWS getChar (addChar c0 s) loc1

        -- found some other character
        Just (c0, loc1) | isAnyChar c0 -> go (addChar c0 s) loc1

        -- found some unknown character
        Just (_, _) -> Left $ BadCharInitialLex loc0 (hasSQuote getChar s)

        -- reached EOF before finding end of string
        Nothing -> Left $ BadCharInitialLex loc0 (hasSQuote getChar s)
{-# INLINE lexString #-}

checkDelimiter :: LexStringType -> GetChar loc -> loc -> Maybe loc
checkDelimiter strType getChar loc0 =
  case strType of
    StringTypeSingle -> do
      ('"', loc1) <- getChar loc0
      Just loc1
    StringTypeMulti -> do
      ('"', loc1) <- getChar loc0
      ('"', loc2) <- getChar loc1
      ('"', loc3) <- getChar loc2
      Just loc3
{-# INLINE checkDelimiter #-}

-- | A helper for adding the given character to the lexed string.
addChar :: Char -> LexStringState loc -> LexStringState loc
addChar c s = s{stringAcc = c : stringAcc s}
{-# INLINE addChar #-}

-- | Return whether the string we've parsed so far contains any smart quotes.
hasSQuote :: GetChar loc -> LexStringState loc -> ContainsSmartQuote loc
hasSQuote getChar s
  | any isDoubleSmartQuote (stringAcc s)
  , (c, loc) : _ <- filter (isDoubleSmartQuote . fst) allChars =
      SmartQuote c loc
  | otherwise =
      NoSmartQuote
  where
    allChars = unfoldr getCharWithLoc (initialLoc s)
    getCharWithLoc loc =
      case getChar loc of
        Just (c, loc') -> Just ((c, loc), loc')
        Nothing -> Nothing
{-# INLINE hasSQuote #-}

-- | After parsing a backslash and a space character, consume the rest of
-- the string gap and return the next location.
collapseStringGap :: GetChar loc -> LexStringState loc -> loc -> Either (StringLexError loc) loc
collapseStringGap getChar s = go
  where
    go loc0 =
      case getChar loc0 of
        Just ('\\', loc1) -> pure loc1
        Just (c0, loc1) | is_space c0 -> go loc1
        Just _ -> Left $ BadCharInitialLex loc0 (hasSQuote getChar s)
        Nothing -> Left $ UnexpectedEOF loc0 (hasSQuote getChar s)
{-# INLINE collapseStringGap #-}

-- | See Note [Multiline string literals]
parseLeadingWS :: GetChar loc -> LexStringState loc -> loc -> (LexStringState loc, loc)
parseLeadingWS getChar = go 0
  where
    go !col s loc =
      case getChar loc of
        Just (c@' ', loc') -> go (col + 1) (addChar c s) loc'
        -- expand tabs
        Just ('\t', loc') ->
          let fill = 8 - (col `mod` 8)
              s' = applyN fill (addChar ' ') s
           in go (col + fill) s' loc'
        -- if we see a newline or string delimiter, then this line only contained whitespace, so
        -- don't include it in the common whitespace prefix
        Just ('\n', _) -> (s, loc)
        Just ('"', _) | Just _ <- checkDelimiter StringTypeMulti getChar loc -> (s, loc)
        -- found some other character, so we're done parsing leading whitespace
        _ ->
          let s' = s{multilineCommonWsPrefix = min col (multilineCommonWsPrefix s)}
           in (s', loc)

    applyN :: Int -> (a -> a) -> a -> a
    applyN n f x0 = iterate f x0 !! n
{-# INLINE parseLeadingWS #-}

data StringLexError loc
  = UnexpectedEOF !loc !(ContainsSmartQuote loc)
    -- ^ Unexpectedly hit EOF when lexing string
  | BadCharInitialLex !loc !(ContainsSmartQuote loc)
    -- ^ Found invalid character when initially lexing string
  | EscapeBadChar !loc
    -- ^ Found invalid character when parsing an escaped character
  | EscapeUnexpectedEOF !loc
    -- ^ Unexpectedly hit EOF when parsing an escaped character
  | EscapeNumRangeError !loc
    -- ^ Escaped number exceeds range
  | EscapeSmartQuoteError !Char !loc
    -- ^ Found escaped smart unicode chars as `\’` or `\”`
  deriving (Show)

-- | When initially lexing the string, we want to track if we've
-- seen a smart quote, to show a helpful "you might be accidentally
-- using a smart quote" error.
data ContainsSmartQuote loc
  = NoSmartQuote
  | SmartQuote !Char !loc
  deriving (Show)

-- -----------------------------------------------------------------------------
-- Escape characters

-- | After finding a backslash, parse the rest of the escape character, starting
-- at the given location.
resolveEscapeCharacter :: GetChar loc -> loc -> Either (StringLexError loc) (Char, loc)
resolveEscapeCharacter getChar loc0 = do
  (c0, loc1) <- expectChar loc0
  case c0 of
    'a'  -> pure ('\a', loc1)
    'b'  -> pure ('\b', loc1)
    'f'  -> pure ('\f', loc1)
    'n'  -> pure ('\n', loc1)
    'r'  -> pure ('\r', loc1)
    't'  -> pure ('\t', loc1)
    'v'  -> pure ('\v', loc1)
    '\\' -> pure ('\\', loc1)
    '"'  -> pure ('\"', loc1)
    '\'' -> pure ('\'', loc1)
    -- escape codes
    'x' -> expectNum is_hexdigit 16 hexDigit loc1
    'o' -> expectNum is_octdigit 8 octDecDigit loc1
    _ | is_decdigit c0 -> expectNum is_decdigit 10 octDecDigit loc0
    -- control characters (e.g. '\^M')
    '^' -> do
      (c1, loc2) <- expectChar loc1
      unless ('@' <= c1 && c1 <= '_') $ Left $ EscapeBadChar loc1
      pure (chr $ ord c1 - ord '@', loc2)
    -- long form escapes (e.g. '\NUL')
    _ | Just (c1, loc2) <- parseLongEscape getChar c0 loc1 -> pure (c1, loc2)
    -- check unicode smart quotes (#21843)
    _ | isDoubleSmartQuote c0 -> Left $ EscapeSmartQuoteError c0 loc0
    _ | isSingleSmartQuote c0 -> Left $ EscapeSmartQuoteError c0 loc0
    -- unknown escape
    _ -> Left $ EscapeBadChar loc0
  where
    expectChar loc =
      case getChar loc of
        Just x -> pure x
        Nothing -> Left $ EscapeUnexpectedEOF loc

    expectNum isDigit base toDigit loc1 = do
      (c1, loc2) <- expectChar loc1
      unless (isDigit c1) $ Left $ EscapeBadChar loc1
      let parseNum x loc =
            case getChar loc of
              Just (c, loc') | isDigit c -> do
                let x' = x * base + toDigit c
                when (x' > 0x10ffff) $ Left $ EscapeNumRangeError loc
                parseNum x' loc'
              _ ->
                pure (chr x, loc)
      parseNum (toDigit c1) loc2
{-# INLINE resolveEscapeCharacter #-}

parseLongEscape :: GetChar loc -> Char -> loc -> Maybe (Char, loc)
parseLongEscape getChar c0 loc1 = listToMaybe $ mapMaybe tryParse longEscapeCodes
  where
    tryParse (prefix, c) = do
      p0 : p <- pure prefix
      guard (p0 == c0)          -- see if the first character matches
      loc <- parsePrefix loc1 p -- see if the rest of the prefix matches
      pure (c, loc)

    parsePrefix loc = \case
      [] -> pure loc
      p : ps -> do
        (c, loc') <- getChar loc
        guard (p == c)
        parsePrefix loc' ps

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
{-# INLINE parseLongEscape #-}

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

{-
Note [Multiline string literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Multiline string literals were added following the acceptance of the
proposal: https://github.com/ghc-proposals/ghc-proposals/pull/569

Multiline string literals are syntax sugar for normal string literals,
with an extra post processing step. This all happens in the Lexer; that
is, HsMultilineString will contain the post-processed string. This matches
the same behavior as HsString, which contains the normalized string
(see Note [Literal source text]).

The canonical steps for post processing a multiline string are:
1. Collapse string gaps
2. Split the string by newlines
3. Convert leading tabs into spaces
    * In each line, any tabs preceding non-whitespace characters are replaced with spaces up to the next tab stop
4. Remove common whitespace prefix in every line (see below)
5. If a line contains only whitespace, remove all of the whitespace
6. Join the string back with `\n` delimiters
7. If the first character of the string is a newline, remove it
8. Interpret escaped characters

However, for performance reasons, we do as much of this in one pass as possible:
1. As we lex the string, do the following steps as they appear:
    a. Collapse string gaps
    b. Keep track of the common whitespace prefix so far
    c. Validate escaped characters
2. At the very end, post process the lexed string:
    a. Remove the common whitespace prefix from every line
    b. Remove all whitespace from all-whitespace lines
    c. Remove initial newline character
    d. Resolve escaped characters

The common whitespace prefix can be informally defined as "The longest
prefix of whitespace shared by all lines in the string, excluding the
first line and any whitespace-only lines".

It's more precisely defined with the following algorithm:

1. Take a list representing the lines in the string
2. Ignore the following elements in the list:
    * The first line (we want to ignore everything before the first newline)
    * Empty lines
    * Lines with only whitespace characters
3. Calculate the longest prefix of whitespace shared by all lines in the remaining list
-}

-- | See Note [Multiline string literals]
postprocessMultiline :: Int -> String -> String
postprocessMultiline commonWSPrefix =
      rmCommonWhitespacePrefix
  >>> collapseOnlyWsLines
  >>> rmFirstNewline
  >>> rmLastNewline
  >>> resolveEscapeChars
  where
    rmCommonWhitespacePrefix =
      let go = \case
            '\n' : s -> '\n' : go (dropLine commonWSPrefix s)
            c : s -> c : go s
            [] -> []
          -- drop x characters from the string, or up to a newline, whichever
          -- comes first
          dropLine !x = \case
            s | x <= 0 -> s
            s@('\n' : _) -> s
            _ : s -> dropLine (x - 1) s
            [] -> []
       in go

    collapseOnlyWsLines =
      let go = \case
            '\n' : s | Just s' <- checkAllWs s -> '\n' : go s'
            c : s -> c : go s
            [] -> []
          checkAllWs = \case
            -- got all the way to a newline or the end of the string, return
            s@('\n' : _) -> Just s
            s@[] -> Just s
            -- found whitespace, continue
            c : s | is_space c -> checkAllWs s
            -- anything else, stop
            _ -> Nothing
       in go

    rmFirstNewline = \case
      '\n' : s -> s
      s -> s

    rmLastNewline =
      let go = \case
            [] -> []
            ['\n'] -> []
            c : cs -> c : go cs
       in go

    -- resolve escape characters, deferred from lexString. guaranteed
    -- to not throw any errors, since we already checked them in lexString
    resolveEscapeChars = \case
      [] -> []
      '\\' : s ->
        -- concretizing 'loc' to String:
        --   resolveEscapeCharacter :: (String -> Maybe (Char, String)) -> String -> Either _ (Char, String)
        case resolveEscapeCharacter uncons s of
          Left e -> panic $ "resolving escape characters in multiline string unexpectedly found errors: " ++ show e
          Right (c, s') -> c : resolveEscapeChars s'
      c : s -> c : resolveEscapeChars s

    uncons = \case
      c : cs -> Just (c, cs)
      [] -> Nothing

-- -----------------------------------------------------------------------------
-- Helpers

isAnyChar :: Char -> Bool
isAnyChar c
  | c > '\x7f' = isPrint c
  | otherwise  = is_any c
