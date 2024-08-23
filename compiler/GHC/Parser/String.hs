{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Parser.String (
  StringLexError (..),
  lexString,
  lexMultilineString,

  -- * Unicode smart quote helpers
  isDoubleSmartQuote,
  isSingleSmartQuote,
) where

import GHC.Prelude hiding (getChar)

import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Char (chr, ord)
import qualified Data.Foldable1 as Foldable1
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Data.StringBuffer (StringBuffer)
import qualified GHC.Data.StringBuffer as StringBuffer
import GHC.Parser.CharClass (
  hexDigit,
  is_decdigit,
  is_hexdigit,
  is_octdigit,
  is_space,
  octDecDigit,
 )
import GHC.Parser.Errors.Types (LexErr (..))
import GHC.Utils.Panic (panic)

type BufPos = Int
data StringLexError = StringLexError LexErr BufPos

lexString :: Int -> StringBuffer -> Either StringLexError String
lexString = lexStringWith processChars processChars
  where
    processChars :: HasChar c => [c] -> Either (c, LexErr) [c]
    processChars =
          collapseGaps
      >>> resolveEscapes

-- -----------------------------------------------------------------------------
-- Lexing interface

{-
Note [Lexing strings]
~~~~~~~~~~~~~~~~~~~~~

After verifying if a string is lexically valid with Alex, we still need to do
some post processing of the string, namely:
1. Collapse string gaps
2. Resolve escape characters

The problem: 'lexemeToString' is more performant than manually reading
characters from the StringBuffer. However, that completely erases the position
of each character, which we need in order to report the correct position for
error messages (e.g. when resolving escape characters).

So what we'll do is do two passes. The first pass is optimistic; just convert
to a plain String and process it. If this results in an error, we do a second
pass, this time where each character is annotated with its position. Now, the
error has all the information it needs.

Ideally, lexStringWith would take a single (forall c. HasChar c => ...) function,
but to help the specializer, we pass it in twice to concretize it for the two
types we actually use.
-}

-- | See Note [Lexing strings]
lexStringWith ::
  ([Char] -> Either (Char, LexErr) [Char])
  -> ([CharPos] -> Either (CharPos, LexErr) [CharPos])
  -> Int
  -> StringBuffer
  -> Either StringLexError String
lexStringWith processChars processCharsPos len buf =
  case processChars $ bufferChars buf len of
    Right s -> Right s
    Left _ ->
      case processCharsPos $ bufferLocatedChars buf len of
        Right _ -> panic "expected lex error on second pass"
        Left ((_, pos), e) -> Left $ StringLexError e pos

class HasChar c where
  getChar :: c -> Char
  setChar :: Char -> c -> c

instance HasChar Char where
  getChar = id
  setChar = const

instance HasChar (Char, x) where
  getChar = fst
  setChar c (_, x) = (c, x)

pattern Char :: HasChar c => Char -> c
pattern Char c <- (getChar -> c)
{-# COMPLETE Char #-}

bufferChars :: StringBuffer -> Int -> [Char]
bufferChars = StringBuffer.lexemeToString

type CharPos = (Char, BufPos)

bufferLocatedChars :: StringBuffer -> Int -> [CharPos]
bufferLocatedChars initialBuf len = go initialBuf
  where
    go buf
      | atEnd buf = []
      | otherwise  =
          let (c, buf') = StringBuffer.nextChar buf
           in (c, StringBuffer.cur buf) : go buf'

    atEnd buf = StringBuffer.byteDiff initialBuf buf >= len

-- -----------------------------------------------------------------------------
-- Lexing phases

collapseGaps :: HasChar c => [c] -> [c]
collapseGaps = go
  where
    go = \case
      c1@(Char '\\') : c2@(Char c) : cs
        | is_space c -> go $ dropGap cs
        | otherwise  -> c1 : c2 : go cs
      c : cs -> c : go cs
      [] -> []

    dropGap = \case
      Char '\\' : cs -> cs
      _ : cs -> dropGap cs
      [] -> panic "gap unexpectedly ended"

resolveEscapes :: HasChar c => [c] -> Either (c, LexErr) [c]
resolveEscapes = go dlistEmpty
  where
    go !acc = \case
      [] -> pure $ dlistToList acc
      Char '\\' : Char '&' : cs -> go acc cs
      backslash@(Char '\\') : cs ->
        case resolveEscapeChar cs of
          Right (esc, cs') -> go (acc `dlistSnoc` setChar esc backslash) cs'
          Left (c, e) -> Left (c, e)
      c : cs -> go (acc `dlistSnoc` c) cs

-- -----------------------------------------------------------------------------
-- Escape characters

-- | Resolve a escape character, after having just lexed a backslash.
-- Assumes escape character is valid.
resolveEscapeChar :: HasChar c => [c] -> Either (c, LexErr) (Char, [c])
resolveEscapeChar = \case
  Char 'a'  : cs -> pure ('\a', cs)
  Char 'b'  : cs -> pure ('\b', cs)
  Char 'f'  : cs -> pure ('\f', cs)
  Char 'n'  : cs -> pure ('\n', cs)
  Char 'r'  : cs -> pure ('\r', cs)
  Char 't'  : cs -> pure ('\t', cs)
  Char 'v'  : cs -> pure ('\v', cs)
  Char '\\' : cs -> pure ('\\', cs)
  Char '"'  : cs -> pure ('\"', cs)
  Char '\'' : cs -> pure ('\'', cs)
  -- escape codes
  Char 'x' : cs -> parseNum is_hexdigit 16 hexDigit cs
  Char 'o' : cs -> parseNum is_octdigit 8 octDecDigit cs
  cs@(Char c : _) | is_decdigit c -> parseNum is_decdigit 10 octDecDigit cs
  -- control characters (e.g. '\^M')
  Char '^' : Char c : cs -> pure (chr $ ord c - ord '@', cs)
  -- long form escapes (e.g. '\NUL')
  cs | Just (esc, cs') <- parseLongEscape cs -> pure (esc, cs')
  -- shouldn't happen
  Char c : _ -> panic $ "found unexpected escape character: " ++ show c
  [] -> panic "escape character unexpectedly ended"
  where
    parseNum isDigit base toDigit =
      let go x = \case
            ch@(Char c) : cs | isDigit c -> do
              let x' = x * base + toDigit c
              when (x' > 0x10ffff) $ Left (ch, LexNumEscapeRange)
              go x' cs
            cs -> pure (chr x, cs)
       in go 0

parseLongEscape :: HasChar c => [c] -> Maybe (Char, [c])
parseLongEscape cs = listToMaybe (mapMaybe tryParse longEscapeCodes)
  where
    tryParse (code, esc) =
      case splitAt (length code) cs of
        (pre, cs') | map getChar pre == code -> Just (esc, cs')
        _ -> Nothing

    longEscapeCodes =
      [ ("NUL", '\NUL')
      , ("SOH", '\SOH')
      , ("STX", '\STX')
      , ("ETX", '\ETX')
      , ("EOT", '\EOT')
      , ("ENQ", '\ENQ')
      , ("ACK", '\ACK')
      , ("BEL", '\BEL')
      , ("BS" , '\BS' )
      , ("HT" , '\HT' )
      , ("LF" , '\LF' )
      , ("VT" , '\VT' )
      , ("FF" , '\FF' )
      , ("CR" , '\CR' )
      , ("SO" , '\SO' )
      , ("SI" , '\SI' )
      , ("DLE", '\DLE')
      , ("DC1", '\DC1')
      , ("DC2", '\DC2')
      , ("DC3", '\DC3')
      , ("DC4", '\DC4')
      , ("NAK", '\NAK')
      , ("SYN", '\SYN')
      , ("ETB", '\ETB')
      , ("CAN", '\CAN')
      , ("EM" , '\EM' )
      , ("SUB", '\SUB')
      , ("ESC", '\ESC')
      , ("FS" , '\FS' )
      , ("GS" , '\GS' )
      , ("RS" , '\RS' )
      , ("US" , '\US' )
      , ("SP" , '\SP' )
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

-- -----------------------------------------------------------------------------
-- Multiline strings

-- | See Note [Multiline string literals]
--
-- Assumes string is lexically valid. Skips the steps about splitting
-- and rejoining lines, and instead manually find newline characters,
-- for performance.
lexMultilineString :: Int -> StringBuffer -> Either StringLexError String
lexMultilineString = lexStringWith processChars processChars
  where
    processChars :: HasChar c => [c] -> Either (c, LexErr) [c]
    processChars =
          collapseGaps             -- Step 1
      >>> expandLeadingTabs        -- Step 3
      >>> rmCommonWhitespacePrefix -- Step 4
      >>> collapseOnlyWsLines      -- Step 5
      >>> rmFirstNewline           -- Step 7a
      >>> rmLastNewline            -- Step 7b
      >>> resolveEscapes           -- Step 8

    -- expands all tabs, since the lexer will verify that tabs can only appear
    -- as leading indentation
    expandLeadingTabs :: HasChar c => [c] -> [c]
    expandLeadingTabs =
      let go !col = \case
            c@(Char '\t') : cs ->
              let fill = 8 - (col `mod` 8)
               in replicate fill (setChar ' ' c) ++ go (col + fill) cs
            c : cs -> c : go (if getChar c == '\n' then 0 else col + 1) cs
            [] -> []
       in go 0

    rmCommonWhitespacePrefix :: HasChar c => [c] -> [c]
    rmCommonWhitespacePrefix cs0 =
      let commonWSPrefix = getCommonWsPrefix (map getChar cs0)
          go = \case
            c@(Char '\n') : cs -> c : go (dropLine commonWSPrefix cs)
            c : cs -> c : go cs
            [] -> []
          -- drop x characters from the string, or up to a newline, whichever
          -- comes first
          dropLine !x = \case
            cs | x <= 0 -> cs
            cs@(Char '\n' : _) -> cs
            _ : cs -> dropLine (x - 1) cs
            [] -> []
       in go cs0

    collapseOnlyWsLines :: HasChar c => [c] -> [c]
    collapseOnlyWsLines =
      let go = \case
            c@(Char '\n') : cs | Just cs' <- checkAllWs cs -> c : go cs'
            c : cs -> c : go cs
            [] -> []
          checkAllWs = \case
            -- got all the way to a newline or the end of the string, return
            cs@(Char '\n' : _) -> Just cs
            cs@[] -> Just cs
            -- found whitespace, continue
            Char c : cs | is_space c -> checkAllWs cs
            -- anything else, stop
            _ -> Nothing
       in go

    rmFirstNewline :: HasChar c => [c] -> [c]
    rmFirstNewline = \case
      Char '\n' : cs -> cs
      cs -> cs

    rmLastNewline :: HasChar c => [c] -> [c]
    rmLastNewline =
      let go = \case
            [] -> []
            [Char '\n'] -> []
            c : cs -> c : go cs
       in go

-- | See step 4 in Note [Multiline string literals]
--
-- Assumes tabs have already been expanded.
getCommonWsPrefix :: String -> Int
getCommonWsPrefix s =
  case NonEmpty.nonEmpty includedLines of
    Nothing -> 0
    Just ls -> Foldable1.minimum $ NonEmpty.map (length . takeWhile is_space) ls
  where
    includedLines =
        filter (not . all is_space) -- ignore whitespace-only lines
      . drop 1                      -- ignore first line in calculation
      $ lines s

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
4. Remove common whitespace prefix in every line except the first (see below)
5. If a line contains only whitespace, remove all of the whitespace
6. Join the string back with `\n` delimiters
7a. If the first character of the string is a newline, remove it
7b. If the last character of the string is a newline, remove it
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

-- -----------------------------------------------------------------------------
-- DList

newtype DList a = DList ([a] -> [a])

dlistEmpty :: DList a
dlistEmpty = DList id

dlistToList :: DList a -> [a]
dlistToList (DList f) = f []

dlistSnoc :: DList a -> a -> DList a
dlistSnoc (DList f) x = DList (f . (x :))
