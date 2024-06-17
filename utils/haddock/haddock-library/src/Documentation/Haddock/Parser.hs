{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      :  Documentation.Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013-2014,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Parser used for Haddock comments. For external users of this
-- library, the most commonly used combination of functions is going
-- to be
--
-- @'toRegular' . '_doc' . 'parseParas'@
module Documentation.Haddock.Parser
  ( parseString
  , parseParas
  , overIdentifier
  , toRegular
  , Identifier
  ) where

import Control.Applicative
import Control.Arrow (first)
import Control.Monad
import Data.Char (chr, isAlpha, isSpace, isUpper)
import Data.List (elemIndex, intercalate, unfoldr, intersperse)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid
import qualified Data.Set as Set
import Documentation.Haddock.Doc
import Documentation.Haddock.Markup (markup, plainMarkup)
import Documentation.Haddock.Parser.Identifier
import Documentation.Haddock.Parser.Monad
import Documentation.Haddock.Parser.Util
import Documentation.Haddock.Types
import Prelude hiding (takeWhile)
import qualified Prelude as P

import Text.Parsec (try)
import qualified Text.Parsec as Parsec

import Data.Text (Text)
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings

-- | Drops the quotes/backticks around all identifiers, as if they
-- were valid but still 'String's.
toRegular :: DocH mod Identifier -> DocH mod String
toRegular = fmap (\(Identifier _ _ x _) -> x)

-- | Maps over 'DocIdentifier's over 'String' with potentially failing
-- conversion using user-supplied function. If the conversion fails,
-- the identifier is deemed to not be valid and is treated as a
-- regular string.
overIdentifier
  :: (Namespace -> String -> Maybe a)
  -> DocH mod Identifier
  -> DocH mod a
overIdentifier f d = g d
  where
    g (DocIdentifier (Identifier ns o x e)) = case f ns x of
      Nothing -> DocString $ renderNs ns ++ [o] ++ x ++ [e]
      Just x' -> DocIdentifier x'
    g DocEmpty = DocEmpty
    g (DocAppend x x') = DocAppend (g x) (g x')
    g (DocString x) = DocString x
    g (DocParagraph x) = DocParagraph $ g x
    g (DocIdentifierUnchecked x) = DocIdentifierUnchecked x
    g (DocModule (ModLink m x)) = DocModule (ModLink m (fmap g x))
    g (DocWarning x) = DocWarning $ g x
    g (DocEmphasis x) = DocEmphasis $ g x
    g (DocMonospaced x) = DocMonospaced $ g x
    g (DocBold x) = DocBold $ g x
    g (DocUnorderedList x) = DocUnorderedList $ fmap g x
    g (DocOrderedList x) = DocOrderedList $ fmap (\(index, a) -> (index, g a)) x
    g (DocDefList x) = DocDefList $ fmap (\(y, z) -> (g y, g z)) x
    g (DocCodeBlock x) = DocCodeBlock $ g x
    g (DocCodeBlockHighlight hl) = DocCodeBlockHighlight hl
    g (DocHyperlink (Hyperlink u x)) = DocHyperlink (Hyperlink u (fmap g x))
    g (DocPic x) = DocPic x
    g (DocMathInline x) = DocMathInline x
    g (DocMathDisplay x) = DocMathDisplay x
    g (DocAName x) = DocAName x
    g (DocProperty x) = DocProperty x
    g (DocExamples x) = DocExamples x
    g (DocHeader (Header l x)) = DocHeader . Header l $ g x
    g (DocTable (Table h b)) = DocTable (Table (map (fmap g) h) (map (fmap g) b))

choice' :: [Parser a] -> Parser a
choice' [] = empty
choice' [p] = p
choice' (p : ps) = try p <|> choice' ps

parse :: Parser a -> Text -> (ParserState, a)
parse p = either err id . parseOnly (p <* Parsec.eof)
  where
    err = error . ("Haddock.Parser.parse: " ++)

-- | Main entry point to the parser. Appends the newline character
-- to the input string.
parseParas
  :: Maybe Package
  -> String
  -- ^ String to parse
  -> MetaDoc mod Identifier
parseParas pkg input = case parseParasState input of
  (state, a) ->
    let defaultPackage s = s{sincePackage = sincePackage s <|> pkg}
     in MetaDoc
          { _meta = Meta{_metaSince = defaultPackage <$> parserStateSince state}
          , _doc = a
          }

parseParasState :: String -> (ParserState, DocH mod Identifier)
parseParasState = parse (emptyLines *> p) . T.pack . (++ "\n") . filter (/= '\r')
  where
    p :: Parser (DocH mod Identifier)
    p = docConcat <$> many (paragraph <* emptyLines)

    emptyLines :: Parser ()
    emptyLines = void $ many (try (skipHorizontalSpace *> "\n"))

parseParagraphs :: String -> Parser (DocH mod Identifier)
parseParagraphs input = case parseParasState input of
  (state, a) -> Parsec.putState state *> pure a

-- | Variant of 'parseText' for 'String' instead of 'Text'
parseString :: String -> DocH mod Identifier
parseString = parseText . T.pack

-- | Parse a text paragraph. Actually just a wrapper over 'parseParagraph' which
-- drops leading whitespace.
parseText :: Text -> DocH mod Identifier
parseText = parseParagraph . T.dropWhile isSpace . T.filter (/= '\r')

parseParagraph :: Text -> DocH mod Identifier
parseParagraph = snd . parse p
  where
    p :: Parser (DocH mod Identifier)
    p =
      docConcat
        <$> many
          ( choice'
              [ monospace
              , anchor
              , identifier
              , moduleName
              , picture
              , mathDisplay
              , mathInline
              , markdownImage
              , markdownLink
              , hyperlink
              , bold
              , emphasis
              , encodedChar
              , string'
              , skipSpecialChar
              ]
          )

-- | Parses and processes
-- <https://en.wikipedia.org/wiki/Numeric_character_reference Numeric character references>
--
-- >>> parseString "&#65;"
-- DocString "A"
encodedChar :: Parser (DocH mod a)
encodedChar = "&#" *> c <* ";"
  where
    c = DocString . return . chr <$> num
    num = hex <|> decimal
    hex = ("x" <|> "X") *> hexadecimal

-- | List of characters that we use to delimit any special markup.
-- Once we have checked for any of these and tried to parse the
-- relevant markup, we can assume they are used as regular text.
specialChar :: [Char]
specialChar = "_/<@\"&'`#[ "

-- | Plain, regular parser for text. Called as one of the last parsers
-- to ensure that we have already given a chance to more meaningful parsers
-- before capturing their characters.
string' :: Parser (DocH mod a)
string' = DocString . unescape . T.unpack <$> takeWhile1_ (`notElem` specialChar)
  where
    unescape "" = ""
    unescape ('\\' : x : xs) = x : unescape xs
    unescape (x : xs) = x : unescape xs

-- | Skips a single special character and treats it as a plain string.
-- This is done to skip over any special characters belonging to other
-- elements but which were not deemed meaningful at their positions.
skipSpecialChar :: Parser (DocH mod a)
skipSpecialChar = DocString . return <$> Parsec.oneOf specialChar

-- | Emphasis parser.
--
-- >>> parseString "/Hello world/"
-- DocEmphasis (DocString "Hello world")
emphasis :: Parser (DocH mod Identifier)
emphasis =
  DocEmphasis . parseParagraph
    <$> disallowNewline ("/" *> takeWhile1_ (/= '/') <* "/")

-- | Bold parser.
--
-- >>> parseString "__Hello world__"
-- DocBold (DocString "Hello world")
bold :: Parser (DocH mod Identifier)
bold = DocBold . parseParagraph <$> disallowNewline ("__" *> takeUntil "__")

disallowNewline :: Parser Text -> Parser Text
disallowNewline = mfilter (T.all (/= '\n'))

-- | Like `takeWhile`, but unconditionally take escaped characters.
takeWhile_ :: (Char -> Bool) -> Parser Text
takeWhile_ p = scan p_ False
  where
    p_ escaped c
      | escaped = Just False
      | not $ p c = Nothing
      | otherwise = Just (c == '\\')

-- | Like 'takeWhile1', but unconditionally take escaped characters.
takeWhile1_ :: (Char -> Bool) -> Parser Text
takeWhile1_ = mfilter (not . T.null) . takeWhile_

-- | Text anchors to allow for jumping around the generated documentation.
--
-- >>> parseString "#Hello world#"
-- DocAName "Hello world"
anchor :: Parser (DocH mod a)
anchor =
  DocAName . T.unpack
    <$> ("#" *> takeWhile1_ (\x -> x /= '#' && not (isSpace x)) <* "#")

-- | Monospaced strings.
--
-- >>> parseString "@cruel@"
-- DocMonospaced (DocString "cruel")
monospace :: Parser (DocH mod Identifier)
monospace =
  DocMonospaced . parseParagraph
    <$> ("@" *> takeWhile1_ (/= '@') <* "@")

-- | Module names.
--
-- Note that we allow '#' and '\' to support anchors (old style anchors are of
-- the form "SomeModule\#anchor").
moduleName :: Parser (DocH mod a)
moduleName = DocModule . flip ModLink Nothing <$> ("\"" *> moduleNameString <* "\"")

-- | A module name, optionally with an anchor
moduleNameString :: Parser String
moduleNameString = modid `maybeFollowedBy` anchor_
  where
    modid = intercalate "." <$> conid `Parsec.sepBy1` "."
    anchor_ =
      (++)
        <$> (Parsec.string "#" <|> Parsec.string "\\#")
        <*> many (Parsec.satisfy (\c -> c /= '"' && not (isSpace c)))

    maybeFollowedBy pre suf = (\x -> maybe x (x ++)) <$> pre <*> optional suf
    conid :: Parser String
    conid =
      (:)
        <$> Parsec.satisfy (\c -> isAlpha c && isUpper c)
        <*> many conChar

    conChar = Parsec.alphaNum <|> Parsec.char '_'

-- | A labeled link to an indentifier, module or url using markdown
-- syntax.
markdownLink :: Parser (DocH mod Identifier)
markdownLink = do
  lbl <- markdownLinkText
  choice' [markdownModuleName lbl, markdownURL lbl]
  where
    markdownModuleName lbl = do
      mn <-
        "("
          *> skipHorizontalSpace
          *> "\""
          *> moduleNameString
          <* "\""
          <* skipHorizontalSpace
          <* ")"
      pure $ DocModule (ModLink mn (Just lbl))

    markdownURL lbl = do
      target <- markdownLinkTarget
      pure $ DocHyperlink $ Hyperlink target (Just lbl)

-- | Picture parser, surrounded by \<\< and \>\>. It's possible to specify
-- a title for the picture.
--
-- >>> parseString "<<hello.png>>"
-- DocPic (Picture {pictureUri = "hello.png", pictureTitle = Nothing})
-- >>> parseString "<<hello.png world>>"
-- DocPic (Picture {pictureUri = "hello.png", pictureTitle = Just "world"})
picture :: Parser (DocH mod a)
picture =
  DocPic . makeLabeled Picture
    <$> disallowNewline ("<<" *> takeUntil ">>")

-- | Inline math parser, surrounded by \\( and \\).
--
-- >>> parseString "\\(\\int_{-\\infty}^{\\infty} e^{-x^2/2} = \\sqrt{2\\pi}\\)"
-- DocMathInline "\\int_{-\\infty}^{\\infty} e^{-x^2/2} = \\sqrt{2\\pi}"
mathInline :: Parser (DocH mod a)
mathInline =
  DocMathInline . T.unpack
    <$> disallowNewline ("\\(" *> takeUntil "\\)")

-- | Display math parser, surrounded by \\[ and \\].
--
-- >>> parseString "\\[\\int_{-\\infty}^{\\infty} e^{-x^2/2} = \\sqrt{2\\pi}\\]"
-- DocMathDisplay "\\int_{-\\infty}^{\\infty} e^{-x^2/2} = \\sqrt{2\\pi}"
mathDisplay :: Parser (DocH mod a)
mathDisplay =
  DocMathDisplay . T.unpack
    <$> ("\\[" *> takeUntil "\\]")

-- | Markdown image parser. As per the commonmark reference recommendation, the
-- description text for an image converted to its a plain string representation.
--
-- >>> parseString "![some /emphasis/ in a description](www.site.com)"
-- DocPic (Picture "www.site.com" (Just "some emphasis in a description"))
markdownImage :: Parser (DocH mod Identifier)
markdownImage = do
  text <- markup stringMarkup <$> ("!" *> markdownLinkText)
  url <- markdownLinkTarget
  pure $ DocPic (Picture url (Just text))
  where
    stringMarkup = plainMarkup (const "") renderIdent
    renderIdent (Identifier ns l c r) = renderNs ns <> [l] <> c <> [r]

-- | Paragraph parser, called by 'parseParas'.
paragraph :: Parser (DocH mod Identifier)
paragraph =
  choice'
    [ examples
    , table
    , do
        indent <- takeIndent
        choice'
          [ since
          , unorderedList indent
          , orderedList indent
          , birdtracks
          , codeblock
          , codeblockHighlight indent
          , property
          , header
          , textParagraphThatStartsWithMarkdownLink
          , definitionList indent
          , docParagraph <$> textParagraph
          ]
    ]

-- | Provides support for grid tables.
--
-- Tables are composed by an optional header and body. The header is composed by
-- a single row. The body is composed by a non-empty list of rows.
--
-- Example table with header:
--
-- > +----------+----------+
-- > | /32bit/  |   64bit  |
-- > +==========+==========+
-- > |  0x0000  | @0x0000@ |
-- > +----------+----------+
--
-- Algorithms loosely follows ideas in
-- http://docutils.sourceforge.net/docutils/parsers/rst/tableparser.py
table :: Parser (DocH mod Identifier)
table = do
  -- first we parse the first row, which determines the width of the table
  firstRow <- parseFirstRow
  let len = T.length firstRow

  -- then we parse all consecutive rows starting and ending with + or |,
  -- of the width `len`.
  restRows <- many (try (parseRestRows len))

  -- Now we gathered the table block, the next step is to split the block
  -- into cells.
  DocTable <$> tableStepTwo len (firstRow : restRows)
  where
    parseFirstRow :: Parser Text
    parseFirstRow = do
      skipHorizontalSpace
      cs <- takeWhile (\c -> c == '-' || c == '+')

      -- upper-left and upper-right corners are `+`
      guard
        ( T.length cs >= 2
            && T.head cs == '+'
            && T.last cs == '+'
        )

      -- trailing space
      skipHorizontalSpace
      _ <- Parsec.newline

      return cs

    parseRestRows :: Int -> Parser Text
    parseRestRows l = do
      skipHorizontalSpace
      bs <- scan predicate l

      -- Left and right edges are `|` or `+`
      guard
        ( T.length bs >= 2
            && (T.head bs == '|' || T.head bs == '+')
            && (T.last bs == '|' || T.last bs == '+')
        )

      -- trailing space
      skipHorizontalSpace
      _ <- Parsec.newline

      return bs
      where
        predicate n c
          | n <= 0 = Nothing
          | c == '\n' = Nothing
          | otherwise = Just (n - 1)

-- Second step searchs for row of '+' and '=' characters, records it's index
-- and changes to '=' to '-'.
tableStepTwo
  :: Int
  -- ^ width
  -> [Text]
  -- ^ rows
  -> Parser (Table (DocH mod Identifier))
tableStepTwo width = go 0 []
  where
    go _ left [] = tableStepThree width (reverse left) Nothing
    go n left (r : rs)
      | T.all (`elem` ['+', '=']) r =
          tableStepThree width (reverse left ++ r' : rs) (Just n)
      | otherwise =
          go (n + 1) (r : left) rs
      where
        r' = T.map (\c -> if c == '=' then '-' else c) r

-- Third step recognises cells in the table area, returning a list of TC, cells.
tableStepThree
  :: Int
  -- ^ width
  -> [Text]
  -- ^ rows
  -> Maybe Int
  -- ^ index of header separator
  -> Parser (Table (DocH mod Identifier))
tableStepThree width rs hdrIndex = do
  cells <- loop (Set.singleton (0, 0))
  tableStepFour rs hdrIndex cells
  where
    height = length rs

    loop :: Set.Set (Int, Int) -> Parser [TC]
    loop queue = case Set.minView queue of
      Nothing -> return []
      Just ((y, x), queue')
        | y + 1 >= height || x + 1 >= width -> loop queue'
        | otherwise -> case scanRight x y of
            Nothing -> loop queue'
            Just (x2, y2) -> do
              let tc = TC y x y2 x2
              fmap (tc :) $
                loop $
                  queue'
                    `Set.union` Set.fromList
                      [(y, x2), (y2, x), (y2, x2)]

    -- scan right looking for +, then try scan down
    --
    -- do we need to record + saw on the way left and down?
    scanRight :: Int -> Int -> Maybe (Int, Int)
    scanRight x y = go (x + 1)
      where
        bs = rs !! y
        go x'
          | x' >= width = fail "overflow right "
          | T.index bs x' == '+' = scanDown x y x' <|> go (x' + 1)
          | T.index bs x' == '-' = go (x' + 1)
          | otherwise = fail $ "not a border (right) " ++ show (x, y, x')

    -- scan down looking for +
    scanDown :: Int -> Int -> Int -> Maybe (Int, Int)
    scanDown x y x2 = go (y + 1)
      where
        go y'
          | y' >= height = fail "overflow down"
          | T.index (rs !! y') x2 == '+' = scanLeft x y x2 y' <|> go (y' + 1)
          | T.index (rs !! y') x2 == '|' = go (y' + 1)
          | otherwise = fail $ "not a border (down) " ++ show (x, y, x2, y')

    -- check that at y2 x..x2 characters are '+' or '-'
    scanLeft :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
    scanLeft x y x2 y2
      | all (\x' -> T.index bs x' `elem` ['+', '-']) [x .. x2] = scanUp x y x2 y2
      | otherwise = fail $ "not a border (left) " ++ show (x, y, x2, y2)
      where
        bs = rs !! y2

    -- check that at y2 x..x2 characters are '+' or '-'
    scanUp :: Int -> Int -> Int -> Int -> Maybe (Int, Int)
    scanUp x y x2 y2
      | all (\y' -> T.index (rs !! y') x `elem` ['+', '|']) [y .. y2] = return (x2, y2)
      | otherwise = fail $ "not a border (up) " ++ show (x, y, x2, y2)

-- | table cell: top left bottom right
data TC = TC !Int !Int !Int !Int
  deriving (Show)

tcXS :: TC -> [Int]
tcXS (TC _ x _ x2) = [x, x2]

tcYS :: TC -> [Int]
tcYS (TC y _ y2 _) = [y, y2]

-- | Fourth step. Given the locations of cells, forms 'Table' structure.
tableStepFour :: [Text] -> Maybe Int -> [TC] -> Parser (Table (DocH mod Identifier))
tableStepFour rs hdrIndex cells = case hdrIndex of
  Nothing -> return $ Table [] rowsDoc
  Just i -> case elemIndex i yTabStops of
    Nothing -> return $ Table [] rowsDoc
    Just i' -> return $ uncurry Table $ splitAt i' rowsDoc
  where
    xTabStops = sortNub $ concatMap tcXS cells
    yTabStops = sortNub $ concatMap tcYS cells

    sortNub :: Ord a => [a] -> [a]
    sortNub = Set.toList . Set.fromList

    init' :: [a] -> [a]
    init' [] = []
    init' [_] = []
    init' (x : xs) = x : init' xs

    rowsDoc = (fmap . fmap) parseParagraph rows

    rows = map makeRow (init' yTabStops)
      where
        makeRow y = TableRow $ mapMaybe (makeCell y) cells
        makeCell y (TC y' x y2 x2)
          | y /= y' = Nothing
          | otherwise = Just $ TableCell xts yts (extract (x + 1) (y + 1) (x2 - 1) (y2 - 1))
          where
            xts = length $ P.takeWhile (< x2) $ dropWhile (< x) xTabStops
            yts = length $ P.takeWhile (< y2) $ dropWhile (< y) yTabStops

    -- extract cell contents given boundaries
    extract :: Int -> Int -> Int -> Int -> Text
    extract x y x2 y2 =
      T.intercalate
        "\n"
        [ T.stripEnd $ T.stripStart $ T.take (x2 - x + 1) $ T.drop x $ rs !! y'
        | y' <- [y .. y2]
        ]

-- | Parse \@since annotations.
since :: Parser (DocH mod a)
since = do
  ("@since " *> version <* skipHorizontalSpace <* endOfLine) >>= setSince
  return DocEmpty
  where
    version = do
      pkg <- Parsec.optionMaybe $ Parsec.try $ package
      ver <- decimal `Parsec.sepBy1` "."
      return (MetaSince pkg ver)

    package = combine <$> (Parsec.many1 (Parsec.letter <|> Parsec.char '_')) `Parsec.endBy1` (Parsec.char '-')
    combine = concat . intersperse "-"

-- | Headers inside the comment denoted with @=@ signs, up to 6 levels
-- deep.
--
-- >>> snd <$> parseOnly header "= Hello"
-- Right (DocHeader (Header {headerLevel = 1, headerTitle = DocString "Hello"}))
-- >>> snd <$> parseOnly header "== World"
-- Right (DocHeader (Header {headerLevel = 2, headerTitle = DocString "World"}))
header :: Parser (DocH mod Identifier)
header = do
  let psers = map (string . flip T.replicate "=") [6, 5 .. 1]
      pser = Parsec.choice psers
  depth <- T.length <$> pser
  line <- parseText <$> (skipHorizontalSpace *> nonEmptyLine)
  rest <- try paragraph <|> return DocEmpty
  return $ DocHeader (Header depth line) `docAppend` rest

textParagraph :: Parser (DocH mod Identifier)
textParagraph = parseText . T.intercalate "\n" <$> some nonEmptyLine

textParagraphThatStartsWithMarkdownLink :: Parser (DocH mod Identifier)
textParagraphThatStartsWithMarkdownLink = docParagraph <$> (docAppend <$> markdownLink <*> optionalTextParagraph)
  where
    optionalTextParagraph :: Parser (DocH mod Identifier)
    optionalTextParagraph =
      choice'
        [ docAppend <$> whitespace <*> textParagraph
        , pure DocEmpty
        ]

    whitespace :: Parser (DocH mod a)
    whitespace = DocString <$> (f <$> takeHorizontalSpace <*> optional "\n")
      where
        f :: Text -> Maybe Text -> String
        f xs (fromMaybe "" -> x)
          | T.null (xs <> x) = ""
          | otherwise = " "

-- | Parses unordered (bullet) lists.
unorderedList :: Text -> Parser (DocH mod Identifier)
unorderedList indent = DocUnorderedList <$> p
  where
    p = ("*" <|> "-") *> innerList indent p

-- | Parses ordered lists (numbered or dashed).
orderedList :: Text -> Parser (DocH mod Identifier)
orderedList indent = DocOrderedList <$> p
  where
    p = do
      index <- paren <|> dot
      innerList' indent p index
    dot = (decimal :: Parser Int) <* "."
    paren = "(" *> decimal <* ")"

-- | Like 'innerList' but takes the parsed index of the list item
innerList'
  :: Text
  -> Parser [(Int, DocH mod Identifier)]
  -> Int
  -> Parser [(Int, DocH mod Identifier)]
innerList' indent item index = do
  c <- takeLine
  (cs, items) <- more indent item
  let contents = docParagraph . parseText . dropNLs . T.unlines $ c : cs
  return $ case items of
    Left p -> [(index, contents `docAppend` p)]
    Right i -> (index, contents) : i

-- | Generic function collecting any further lines belonging to the
-- list entry and recursively collecting any further lists in the
-- same paragraph. Usually used as
--
-- > someListFunction = listBeginning *> innerList someListFunction
innerList
  :: Text
  -> Parser [DocH mod Identifier]
  -> Parser [DocH mod Identifier]
innerList indent item = do
  c <- takeLine
  (cs, items) <- more indent item
  let contents = docParagraph . parseText . dropNLs . T.unlines $ c : cs
  return $ case items of
    Left p -> [contents `docAppend` p]
    Right i -> contents : i

-- | Parses definition lists.
definitionList :: Text -> Parser (DocH mod Identifier)
definitionList indent = DocDefList <$> p
  where
    p = do
      label <- "[" *> (parseParagraph <$> takeWhile1_ (`notElem` ("]\n" :: String))) <* ("]" <* optional ":")
      c <- takeLine
      (cs, items) <- more indent p
      let contents = parseText . dropNLs . T.unlines $ c : cs
      return $ case items of
        Left x -> [(label, contents `docAppend` x)]
        Right i -> (label, contents) : i

-- | Drops all trailing newlines.
dropNLs :: Text -> Text
dropNLs = T.dropWhileEnd (== '\n')

-- | Main worker for 'innerList' and 'definitionList'.
-- We need the 'Either' here to be able to tell in the respective functions
-- whether we're dealing with the next list or a nested paragraph.
more
  :: Monoid a
  => Text
  -> Parser a
  -> Parser ([Text], Either (DocH mod Identifier) a)
more indent item =
  choice'
    [ innerParagraphs indent
    , moreListItems indent item
    , moreContent indent item
    , pure ([], Right mempty)
    ]

-- | Used by 'innerList' and 'definitionList' to parse any nested paragraphs.
innerParagraphs
  :: Text
  -> Parser ([Text], Either (DocH mod Identifier) a)
innerParagraphs indent = (,) [] . Left <$> ("\n" *> indentedParagraphs indent)

-- | Attempts to fetch the next list if possibly. Used by 'innerList' and
-- 'definitionList' to recursively grab lists that aren't separated by a whole
-- paragraph.
moreListItems
  :: Text
  -> Parser a
  -> Parser ([Text], Either (DocH mod Identifier) a)
moreListItems indent item = (,) [] . Right <$> indentedItem
  where
    indentedItem = string indent *> Parsec.spaces *> item

-- | Helper for 'innerList' and 'definitionList' which simply takes
-- a line of text and attempts to parse more list content with 'more'.
moreContent
  :: Monoid a
  => Text
  -> Parser a
  -> Parser ([Text], Either (DocH mod Identifier) a)
moreContent indent item = first . (:) <$> nonEmptyLine <*> more indent item

-- | Parses an indented paragraph.
-- The indentation is 4 spaces.
indentedParagraphs :: Text -> Parser (DocH mod Identifier)
indentedParagraphs indent =
  (T.unpack . T.concat <$> dropFrontOfPara indent') >>= parseParagraphs
  where
    indent' = string $ indent <> "    "

-- | Grab as many fully indented paragraphs as we can.
dropFrontOfPara :: Parser Text -> Parser [Text]
dropFrontOfPara sp = do
  currentParagraph <- some (try (sp *> takeNonEmptyLine))
  followingParagraphs <-
    choice'
      [ skipHorizontalSpace *> nextPar -- we have more paragraphs to take
      , skipHorizontalSpace *> nlList -- end of the ride, remember the newline
      , Parsec.eof *> return [] -- nothing more to take at all
      ]
  return (currentParagraph ++ followingParagraphs)
  where
    nextPar = (++) <$> nlList <*> dropFrontOfPara sp
    nlList = "\n" *> return ["\n"]

nonSpace :: Text -> Parser Text
nonSpace xs
  | T.all isSpace xs = fail "empty line"
  | otherwise = return xs

-- | Takes a non-empty, not fully whitespace line.
--
--  Doesn't discard the trailing newline.
takeNonEmptyLine :: Parser Text
takeNonEmptyLine = do
  l <- takeWhile1 (/= '\n') >>= nonSpace
  _ <- "\n"
  pure (l <> "\n")

-- | Takes indentation of first non-empty line.
--
-- More precisely: skips all whitespace-only lines and returns indentation
-- (horizontal space, might be empty) of that non-empty line.
takeIndent :: Parser Text
takeIndent = do
  indent <- takeHorizontalSpace
  choice'
    [ "\n" *> takeIndent
    , return indent
    ]

-- | Blocks of text of the form:
--
-- >> foo
-- >> bar
-- >> baz
birdtracks :: Parser (DocH mod a)
birdtracks = DocCodeBlock . DocString . T.unpack . T.intercalate "\n" . stripSpace <$> some line
  where
    line = try (skipHorizontalSpace *> ">" *> takeLine)

stripSpace :: [Text] -> [Text]
stripSpace = fromMaybe <*> mapM strip'
  where
    strip' t = case T.uncons t of
      Nothing -> Just ""
      Just (' ', t') -> Just t'
      _ -> Nothing

-- | Parses examples. Examples are a paragraph level entity (separated by an empty line).
-- Consecutive examples are accepted.
examples :: Parser (DocH mod a)
examples = DocExamples <$> (many (try (skipHorizontalSpace *> "\n")) *> go)
  where
    go :: Parser [Example]
    go = do
      prefix <- takeHorizontalSpace <* ">>>"
      expr <- takeLine
      (rs, es) <- resultAndMoreExamples
      return (makeExample prefix expr rs : es)
      where
        resultAndMoreExamples :: Parser ([Text], [Example])
        resultAndMoreExamples = choice' [moreExamples, result, pure ([], [])]
          where
            moreExamples :: Parser ([Text], [Example])
            moreExamples = (,) [] <$> go

            result :: Parser ([Text], [Example])
            result = first . (:) <$> nonEmptyLine <*> resultAndMoreExamples

    makeExample :: Text -> Text -> [Text] -> Example
    makeExample prefix expression res =
      Example (T.unpack (T.strip expression)) result
      where
        result = map (T.unpack . substituteBlankLine . tryStripPrefix) res

        tryStripPrefix xs = fromMaybe xs (T.stripPrefix prefix xs)

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine xs = xs

nonEmptyLine :: Parser Text
nonEmptyLine = try (mfilter (T.any (not . isSpace)) takeLine)

takeLine :: Parser Text
takeLine = try (takeWhile (/= '\n') <* endOfLine)

endOfLine :: Parser ()
endOfLine = void "\n" <|> Parsec.eof

-- | Property parser.
--
-- >>> snd <$> parseOnly property "prop> hello world"
-- Right (DocProperty "hello world")
property :: Parser (DocH mod a)
property = DocProperty . T.unpack . T.strip <$> ("prop>" *> takeWhile1 (/= '\n'))

-- |
-- Paragraph level codeblock. Anything between the two delimiting \@ is parsed
-- for markup.
codeblock :: Parser (DocH mod Identifier)
codeblock =
  DocCodeBlock . parseParagraph . dropSpaces
    <$> ("@" *> skipHorizontalSpace *> "\n" *> block' <* "@")
  where
    dropSpaces xs =
      case splitByNl xs of
        [] -> xs
        ys -> case T.uncons (last ys) of
          Just (' ', _) -> case mapM dropSpace ys of
            Nothing -> xs
            Just zs -> T.intercalate "\n" zs
          _ -> xs

    -- This is necessary because ‘lines’ swallows up a trailing newline
    -- and we lose information about whether the last line belongs to @ or to
    -- text which we need to decide whether we actually want to be dropping
    -- anything at all.
    splitByNl =
      unfoldr
        ( \x -> case T.uncons x of
            Just ('\n', x') -> Just (T.span (/= '\n') x')
            _ -> Nothing
        )
        . ("\n" <>)

    dropSpace t = case T.uncons t of
      Nothing -> Just ""
      Just (' ', t') -> Just t'
      _ -> Nothing

    block' = scan p False
      where
        p isNewline c
          | isNewline && c == '@' = Nothing
          | isNewline && isSpace c = Just isNewline
          | otherwise = Just $ c == '\n'

-- | Parses a code block with triple backticks for highlighting (markdown syntax).
-- The indentation in the code block is relative to the position of the opening
-- backticks.
--
-- The parser does not process identifiers for linking (such as the codeBlock
-- parser).
--
-- Example syntax:
--
-- ```haskell
-- -- ```haskell
-- -- fac :: Int -> Int
-- -- fac 0 = 1
-- -- fac n = n * fac (n - 1)
-- --  ```
-- ```
codeblockHighlight :: Text -> Parser (DocH mod id)
codeblockHighlight indent = DocCodeBlockHighlight <$> pHighlight
  where
    pHighlight :: Parser Highlight
    pHighlight =
      Highlight
        <$  string "```"
        <*> pLang
        <*> (intercalate "\n" <$> Parsec.manyTill pCodeLine pBlockEnd)

    pLang :: Parser (Maybe String)
    pLang =
      skipHorizontalSpace
        *> optional (Parsec.many1 Parsec.alphaNum)
        <* skipHorizontalSpace
        <* Parsec.newline

    pBlockEnd :: Parser ()
    pBlockEnd = void (string indent *> string "```" <* Parsec.newline)

    pCodeLine :: Parser String
    pCodeLine =
      pure "" <$> Parsec.newline -- we don't require indentation for empty lines
        <|> string indent *> Parsec.manyTill Parsec.anyChar (try Parsec.newline)



hyperlink :: Parser (DocH mod Identifier)
hyperlink = choice' [angleBracketLink, autoUrl]

angleBracketLink :: Parser (DocH mod a)
angleBracketLink =
  DocHyperlink . makeLabeled (\s -> Hyperlink s . fmap DocString)
    <$> disallowNewline ("<" *> takeUntil ">")

-- | The text for a markdown link, enclosed in square brackets.
markdownLinkText :: Parser (DocH mod Identifier)
markdownLinkText = parseParagraph . T.strip <$> ("[" *> takeUntil "]")

-- | The target for a markdown link, enclosed in parenthesis.
markdownLinkTarget :: Parser String
markdownLinkTarget = whitespace *> url
  where
    whitespace :: Parser ()
    whitespace = skipHorizontalSpace <* optional ("\n" *> skipHorizontalSpace)

    url :: Parser String
    url = rejectWhitespace (decode <$> ("(" *> takeUntil ")"))

    rejectWhitespace :: MonadPlus m => m String -> m String
    rejectWhitespace = mfilter (all (not . isSpace))

    decode :: Text -> String
    decode = T.unpack . removeEscapes

-- | Looks for URL-like things to automatically hyperlink even if they
-- weren't marked as links.
autoUrl :: Parser (DocH mod a)
autoUrl = mkLink <$> url
  where
    url = mappend <$> choice' ["http://", "https://", "ftp://"] <*> takeWhile1 (not . isSpace)

    mkLink :: Text -> DocH mod a
    mkLink s = case T.unsnoc s of
      Just (xs, x) | x `elem` (",.!?" :: String) -> DocHyperlink (mkHyperlink xs) `docAppend` DocString [x]
      _ -> DocHyperlink (mkHyperlink s)

    mkHyperlink :: Text -> Hyperlink (DocH mod a)
    mkHyperlink lnk = Hyperlink (T.unpack lnk) Nothing

-- | Parses identifiers with help of 'parseValid'.
identifier :: Parser (DocH mod Identifier)
identifier = DocIdentifier <$> parseValid
