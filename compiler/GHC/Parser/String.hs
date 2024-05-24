{-# LANGUAGE LambdaCase #-}

module GHC.Parser.String (
  LexedString,
  LexedChar (..),
  StringLexError (..),
  LexStringType (..),
  resolveLexedString,
  resolveEscapeCharacter,

  -- * Unicode smart quote helpers
  isDoubleSmartQuote,
  isSingleSmartQuote,
) where

import GHC.Prelude

import Control.Monad (forM_, guard, unless, when, (>=>))
import Data.Char (chr, isSpace, ord)
import qualified Data.Foldable1 as Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import GHC.Parser.CharClass (
  hexDigit,
  is_decdigit,
  is_hexdigit,
  is_octdigit,
  octDecDigit,
 )
import GHC.Parser.Errors.Types (LexErr (..))
import GHC.Utils.Panic (panic)

data LexStringType = StringTypeSingle | StringTypeMulti deriving (Eq)

data LexedChar loc = LexedChar !Char !loc
type LexedString loc = [LexedChar loc]

unLexedChar :: LexedChar loc -> Char
unLexedChar (LexedChar c _) = c

unLexedString :: LexedString loc -> String
unLexedString = map unLexedChar

-- | Apply the given StringProcessors to the given LexedString left-to-right,
-- and return the processed string.
resolveLexedString ::
  LexStringType ->
  LexedString loc ->
  Either (StringLexError loc) String
resolveLexedString strType = fmap unLexedString . foldr (>=>) pure processString
  where
    processString =
      case strType of
        StringTypeSingle ->
          [ collapseStringGaps
          , resolveEscapeCharacters
          ]
        StringTypeMulti ->
          [ collapseStringGaps
          , resolveMultilineString
          , checkInnerTabs
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

-- | Error if string contains any tab characters.
--
-- Normal strings don't lex tab characters in the first place, but we
-- have to allow them in multiline strings for leading indentation. So
-- we allow them in the initial lexing pass, then check for any remaining
-- tabs after replacing leading tabs in resolveMultilineString.
checkInnerTabs :: StringProcessor loc
checkInnerTabs s = do
  forM_ s $ \(LexedChar c loc) ->
    when (c == '\t') $ Left $ StringLexError c loc LexStringCharLit
  pure s

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

The string is post-processed with the following steps:
1. Collapse string gaps
2. Split the string by newlines
3. Convert leading tabs into spaces
    * In each line, any tabs preceding non-whitespace characters are replaced with spaces up to the next tab stop
4. Remove common whitespace prefix in every line (see below)
5. If a line contains only whitespace, remove all of the whitespace
6. Join the string back with `\n` delimiters
7. If the first character of the string is a newline, remove it
8. Interpret escaped characters

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

-- | A lexed line, with the string and the location info of the ending newline
-- character, if one exists
data LexedLine loc = LexedLine !(LexedString loc) (Maybe loc)

mapLine :: (LexedString loc -> LexedString loc) -> LexedLine loc -> LexedLine loc
mapLine f (LexedLine line nl) = LexedLine (f line) nl

mapLines :: (LexedString loc -> LexedString loc) -> [LexedLine loc] -> [LexedLine loc]
mapLines f = map (mapLine f)

filterLines :: (LexedString loc -> Bool) -> [LexedLine loc] -> [LexedLine loc]
filterLines f = filter (\(LexedLine line _) -> f line)

splitLines :: LexedString loc -> [LexedLine loc]
splitLines =
  foldr
    ( curry $ \case
        (LexedChar '\n' loc, ls) -> LexedLine [] (Just loc) : ls
        (c, l : ls) -> mapLine (c :) l : ls
        (c, []) -> LexedLine [c] Nothing : [] -- should not happen
    )
    [emptyLine]
  where
    emptyLine = LexedLine [] Nothing

joinLines :: [LexedLine loc] -> LexedString loc
joinLines = concatMap (\(LexedLine line nl) -> line ++ maybeToList (LexedChar '\n' <$> nl))

-- | See Note [Multiline string literals]
resolveMultilineString :: StringProcessor loc
resolveMultilineString = pure . process
  where
    (.>) :: (a -> b) -> (b -> c) -> (a -> c)
    (.>) = flip (.)

    process =
         splitLines
      .> convertLeadingTabs
      .> rmCommonWhitespacePrefix
      .> stripOnlyWhitespace
      .> joinLines
      .> rmFirstNewline

    convertLeadingTabs =
      let convertLine col = \case
            [] -> []
            c@(LexedChar ' ' _) : cs -> c : convertLine (col + 1) cs
            LexedChar '\t' loc : cs ->
              let fill = 8 - (col `mod` 8)
               in replicate fill (LexedChar ' ' loc) ++ convertLine (col + fill) cs
            c : cs -> c : cs
       in mapLines (convertLine 0)

    rmCommonWhitespacePrefix = \case
      [] -> []
      -- exclude the first line from this calculation
      firstLine : strLines ->
        let excludeWsOnlyLines = filterLines (not . all isLexedSpace)
            commonWSPrefix =
              case NonEmpty.nonEmpty (excludeWsOnlyLines strLines) of
                Nothing -> 0
                Just strLines' ->
                  Foldable1.minimum $
                    flip NonEmpty.map strLines' $ \(LexedLine line _) ->
                      length $ takeWhile isLexedSpace line
         in firstLine : mapLines (drop commonWSPrefix) strLines

    stripOnlyWhitespace =
      let stripWsOnlyLine line = if all isLexedSpace line then [] else line
       in mapLines stripWsOnlyLine

    rmFirstNewline = \case
      LexedChar '\n' _ : s -> s
      s -> s

-- -----------------------------------------------------------------------------
-- Helpers

isLexedSpace :: LexedChar loc -> Bool
isLexedSpace = isSpace . unLexedChar
