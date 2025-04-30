{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module GHC.Parser.String (
  StringLexError (..),
  StringType (..),
  lexString,

  -- * Raw strings
  RawLexedString,
  lexStringRaw,
  fromRawLexedStringSingle,
  fromRawLexedStringMulti,

  -- * Unicode smart quote helpers
  isDoubleSmartQuote,
  isSingleSmartQuote,
) where

import GHC.Prelude hiding (getChar)

import Control.Arrow ((>>>))
import Control.Monad (when)
import Data.Char (chr, ord)
import qualified Data.Foldable1 as Foldable1
import Data.Functor.Identity (Identity (..))
import Data.List (uncons, unsnoc)
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
  deriving (Show, Eq)

data StringType = StringTypeSingle | StringTypeMulti deriving (Show)

lexString :: StringType -> Int -> StringBuffer -> Either StringLexError String
lexString strType = lexStringWith processChars
  where
    processChars :: String -> Maybe String
    processChars =
      case strType of
        StringTypeSingle -> fromRight . processCharsSingle
        StringTypeMulti -> processCharsMulti

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
-}

-- | See Note [Lexing strings]
lexStringWith :: (String -> Maybe String) -> Int -> StringBuffer -> Either StringLexError String
lexStringWith processChars len buf =
  case processChars $ bufferChars buf len of
    Just s -> Right s
    Nothing -> do
      validateString len buf -- should return Left
      panic "expected lex error on second pass"

-- | Find any lexical errors in the string.
--
-- Can validate both single- and multi-line strings, since multi-line strings
-- have the same validation logic as single-line strings, and none of the
-- multi-line string processing steps affect the validity of the string.
validateString :: Int -> StringBuffer -> Either StringLexError ()
validateString len buf =
  case processCharsSingle $ bufferLocatedChars buf len of
    Right _ -> Right ()
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

processCharsSingle :: HasChar c => [c] -> Either (c, LexErr) [c]
processCharsSingle =
      collapseGaps
  >>> resolveEscapes

-- | Collapse all string gaps in the given input.
--
-- Iterates through the input in `go` until we encounter a backslash. The
-- @stringchar Alex regex only allows backslashes in two places: escape codes
-- and string gaps.
--
--   * If the next character is a space, it has to be the start of a string gap
--     AND it must end, since the @gap Alex regex will only match if it ends.
--     Collapse the gap and continue the main iteration loop.
--
--   * Otherwise, this is an escape code. If it's an escape code, there are
--     ONLY three possibilities (see the @escape Alex regex):
--       1. The escape code is "\\"
--       2. The escape code is "\^\"
--       3. The escape code does not have a backslash, other than the initial
--          backslash
--
--     In the first two possibilities, just skip them and continue the main
--     iteration loop ("skip" as in "keep in the list as-is"). In the last one,
--     we can just skip the backslash, then continue the main iteration loop.
--     the rest of the escape code will be skipped as normal characters in the
--     string; no need to fully parse a proper escape code.
collapseGaps :: HasChar c => [c] -> [c]
collapseGaps = go
  where
    go = \case
      -- Match the start of a string gap + drop gap
      -- #25784: string gaps are semantically equivalent to "\&"
      c1@(Char '\\') : Char c : cs
        | is_space c -> c1 : setChar '&' c1 : go (dropGap cs)
      -- Match all possible escape characters that include a backslash
      c1@(Char '\\') : c2@(Char '\\') : cs
        -> c1 : c2 : go cs
      c1@(Char '\\') : c2@(Char '^') : c3@(Char '\\') : cs
        -> c1 : c2 : c3 : go cs
      -- Otherwise, just keep looping
      c : cs -> c : go cs
      [] -> []

    dropGap = \case
      Char '\\' : cs -> cs
      _ : cs -> dropGap cs
      -- Unreachable since gaps must end; see docstring
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

resolveEscapesMaybe :: HasChar c => [c] -> Maybe [c]
resolveEscapesMaybe = fromRight . resolveEscapes

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
-- Interpolated strings

-- | A string that's been validated to be lexically correct, but still
-- contains the raw string lexed, without anything resolved.
newtype RawLexedString = RawLexedString {unRawLexedString :: String}
  deriving (Show, Semigroup, Monoid)

-- | Load and validate the string in the given StringBuffer.
--
-- e.g. Lexing "a\nb" will return RawLexedString ['a', '\\', 'n', 'b'].
lexStringRaw :: Int -> StringBuffer -> Either StringLexError RawLexedString
lexStringRaw len buf = RawLexedString (bufferChars buf len) <$ validateString len buf

fromRawLexedStringSingle :: RawLexedString -> String
fromRawLexedStringSingle (RawLexedString s) =
  case processCharsSingle s of
    Right s' -> s'
    Left _ -> panic "Unexpectedly got an error when re-lexing the string"

fromRawLexedStringMulti :: [Either (src, RawLexedString) x] -> [Either (src, String) x]
fromRawLexedStringMulti s =
  case processCharsMulti' $ mapRaw unRawLexedString s of
    Just s' -> s'
    Nothing -> panic "Unexpectedly got an error when re-lexing the string"

{-
Note [Parsing interpolated strings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interpolated string syntax was accepted in this proposal:
https://github.com/ghc-proposals/ghc-proposals/pull/570

Interpolated strings are parsed in the following manner:

1. Lexer takes the string as input:

    s"Hello ${Text.toUpper name}!"

  and outputs the following tokens:

    [ ITstringInterBegin    Nothing StringTypeSingle
    , ITstringInterRaw      src "Hello "
    , ITstringInterExpOpen
    , ITqvarid                  ("Text.toUpper", "name")
    , ITvarid                   "name"
    , ITstringInterExpClose
    , ITstringInterRaw      src "!"
    , ITstringInterEnd          StringTypeSingle
    ]

2. The parser will then parse the tokens into the following HsExpr:

    HsInterString ext Nothing StringTypeSingle
      [ HsInterStringRaw ext "Hello "
      , HsInterStringExp ext $
          HsApp ext
            (HsVar ext 'Text.toUpper)
            (HsVar ext 'name)
      , HsInterStringRaw ext "!"
      ]

This gets desugared in the renamer. See Note [Desugaring interpolated strings].
-}

-- -----------------------------------------------------------------------------
-- Multiline strings

-- | See Note [Multiline string literals]
--
-- Assumes string is lexically valid. Skips the steps about splitting
-- and rejoining lines, and instead manually find newline characters,
-- for performance.
processCharsMulti :: String -> Maybe String
processCharsMulti = fmap from . processCharsMulti' . to
  where
    -- Convert a normal multiline string to/from an interpolated multiline string
    -- with no interpolated expressions.
    to s = [Left ((), s)]
    from = \case
      [Left (_, s)] -> s
      _ -> panic "Got unexpected result when processing characters in multiline string"

-- | An interpolated, multiline string to be processed.
--
-- `src` and `x` here will only ever be instantiated as `SourceText` and
-- `HsExpr`, respectively, but we'll leave it general to ensure we never modify
-- it, we only ever propagate it.
type InterMultiString src x = [Either (src, String) x]

-- | Run the given function over all raw strings, ignoring expressions
mapRaw :: (s1 -> s2) -> [Either (src, s1) x] -> [Either (src, s2) x]
mapRaw f = runIdentity . mapRawM (Identity . f)

-- | Run the given monadic function over all raw strings, ignoring expressions
mapRawM :: Monad m => (s1 -> m s2) -> [Either (src, s1) x] -> m [Either (src, s2) x]
mapRawM f = mapM go
  where
    go = \case
      Left (src, s) -> Left . (src,) <$> f s
      Right x -> pure $ Right x

-- | Process multiline characters generally, for both normal multiline strings and interpolated
-- multiline strings.
processCharsMulti' :: InterMultiString src x -> Maybe (InterMultiString src x)
processCharsMulti' =
      mapRaw collapseGaps         -- Step 1
  >>> mapRaw normalizeEOL
  >>> expandLeadingTabs            -- Step 3
  >>> rmCommonWhitespacePrefix     -- Step 4
  >>> collapseOnlyWsLines          -- Step 5
  >>> rmFirstNewline               -- Step 7a
  >>> rmLastNewline                -- Step 7b
  >>> mapRawM resolveEscapesMaybe -- Step 8

-- | Expands all tabs blindly, since the lexer will verify that tabs can only appear
-- as leading indentation
expandLeadingTabs :: InterMultiString src x -> InterMultiString src x
expandLeadingTabs =
  -- we can expand each raw string part independently, because leading
  -- indentation will never contain an interpolated expression
  mapRaw $ go 0
  where
    go !col = \case
      '\t' : cs ->
        let fill = 8 - (col `mod` 8)
         in replicate fill ' ' ++ go (col + fill) cs
      c : cs -> c : go (if c == '\n' then 0 else col + 1) cs
      [] -> []

-- Normalize line endings to LF. The spec dictates that lines should be
-- split on newline characters and rejoined with ``\n``. But because we
-- aren't actually splitting/rejoining, we'll manually normalize here
normalizeEOL :: String -> String
normalizeEOL = go
  where
    go = \case
      Char '\r' : c@(Char '\n') : cs -> c : go cs
      c@(Char '\r') : cs -> setChar '\n' c : go cs
      c@(Char '\f') : cs -> setChar '\n' c : go cs
      c : cs -> c : go cs
      [] -> []

rmCommonWhitespacePrefix :: InterMultiString src x -> InterMultiString src x
rmCommonWhitespacePrefix s0 =
  -- Whitespace prefix, by definition, only comes after newline characters, and there can
  -- never be an interpolated expr within a whitespace prefix (since the expr would end
  -- the prefix). So we can use a plain `map` to just process the string parts, because
  -- the "drop prefix" logic will never span over multiple parts.
  mapRaw go s0
  where
    commonWSPrefix =
      getCommonWsPrefix . flip concatMap s0 $ \case
        Left (_, s) -> s
        -- treat interpolated exprs as a single, non-space character string
        Right _ -> "x"

    go = \case
      c@'\n' : cs -> c : go (dropPrefix commonWSPrefix cs)
      c : cs -> c : go cs
      [] -> []

    -- drop x characters from the string, or up to a newline, whichever comes first
    dropPrefix !x = \case
      cs | x <= 0 -> cs
      cs@('\n' : _) -> cs
      _ : cs -> dropPrefix (x - 1) cs
      [] -> []

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

collapseOnlyWsLines :: InterMultiString src x -> InterMultiString src x
collapseOnlyWsLines = \case
  [] -> []
  Right x : rest -> Right x : collapseOnlyWsLines rest
  Left (src, s) : rest ->
    case rest of
      Left (_, s2) : _ -> panic $ "Expected alternating raw-string/expression parts, got: " ++ show (s, s2)
      _ -> Left (src, go (null rest) s) : collapseOnlyWsLines rest
  where
    go isLast = \case
      c@'\n' : cs | Just cs' <- checkAllWs isLast cs -> c : go isLast cs'
      c : cs -> c : go isLast cs
      [] -> []

    checkAllWs isLast = \case
      -- got all the way to a newline or the end of the string, return
      cs@('\n' : _) -> Just cs
      cs@[] | isLast -> Just cs
      -- found whitespace, continue
      c : cs | is_space c -> checkAllWs isLast cs
      -- anything else, stop
      _ -> Nothing

rmFirstNewline :: InterMultiString src x -> InterMultiString src x
rmFirstNewline s0 =
  case uncons s0 of
    Just (Left (src, uncons -> Just ('\n', s)), rest)
      -> [Left (src, s)] ++ rest
    _ -> s0

rmLastNewline :: InterMultiString src x -> InterMultiString src x
rmLastNewline s0 =
  case unsnoc s0 of
    Just (rest, Left (src, unsnoc -> Just (s, '\n')))
      -> rest ++ [Left (src, s)]
    _ -> s0

{-
Note [Multiline string literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Multiline string literals were added following the acceptance of the
proposal: https://github.com/ghc-proposals/ghc-proposals/pull/569

Multiline string literals are syntax sugar for normal string literals,
with an extra post processing step. This all happens in the Lexer; that
is, HsString will contain the post-processed string. This matches the same
behavior as single-line HsString, which contains the normalized string
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

-- -----------------------------------------------------------------------------
-- Other utilities

fromRight :: Either e a -> Maybe a
fromRight = either (const Nothing) Just
