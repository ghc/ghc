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
-- @'toRegular' . 'parseParas'@
module Documentation.Haddock.Parser ( parseString, parseParas
                                    , overIdentifier, toRegular, Identifier
                                    ) where

import           Control.Applicative
import           Control.Arrow (first)
import           Control.Monad
import qualified Data.ByteString.Char8 as BS
import           Data.Char (chr, isAsciiUpper)
import           Data.List (stripPrefix, intercalate, unfoldr)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Documentation.Haddock.Doc
import           Documentation.Haddock.Parser.Monad hiding (take, endOfLine)
import           Documentation.Haddock.Parser.Util
import           Documentation.Haddock.Types
import           Documentation.Haddock.Utf8
import           Prelude hiding (takeWhile)

-- $setup
-- >>> :set -XOverloadedStrings

-- | Identifier string surrounded with opening and closing quotes/backticks.
type Identifier = (Char, String, Char)

-- | Drops the quotes/backticks around all identifiers, as if they
-- were valid but still 'String's.
toRegular :: DocH mod Identifier -> DocH mod String
toRegular = fmap (\(_, x, _) -> x)

-- | Maps over 'DocIdentifier's over 'String' with potentially failing
-- conversion using user-supplied function. If the conversion fails,
-- the identifier is deemed to not be valid and is treated as a
-- regular string.
overIdentifier :: (String -> Maybe a)
               -> DocH mod Identifier
               -> DocH mod a
overIdentifier f d = g d
  where
    g (DocIdentifier (o, x, e)) = case f x of
      Nothing -> DocString $ o : x ++ [e]
      Just x' -> DocIdentifier x'
    g DocEmpty = DocEmpty
    g (DocAppend x x') = DocAppend (g x) (g x')
    g (DocString x) = DocString x
    g (DocParagraph x) = DocParagraph $ g x
    g (DocIdentifierUnchecked x) = DocIdentifierUnchecked x
    g (DocModule x) = DocModule x
    g (DocWarning x) = DocWarning $ g x
    g (DocEmphasis x) = DocEmphasis $ g x
    g (DocMonospaced x) = DocMonospaced $ g x
    g (DocBold x) = DocBold $ g x
    g (DocUnorderedList x) = DocUnorderedList $ fmap g x
    g (DocOrderedList x) = DocOrderedList $ fmap g x
    g (DocDefList x) = DocDefList $ fmap (\(y, z) -> (g y, g z)) x
    g (DocCodeBlock x) = DocCodeBlock $ g x
    g (DocHyperlink x) = DocHyperlink x
    g (DocPic x) = DocPic x
    g (DocAName x) = DocAName x
    g (DocProperty x) = DocProperty x
    g (DocExamples x) = DocExamples x
    g (DocHeader (Header l x)) = DocHeader . Header l $ g x

parse :: Parser a -> BS.ByteString -> (ParserState, a)
parse p = either err id . parseOnly (p <* endOfInput)
  where
    err = error . ("Haddock.Parser.parse: " ++)

-- | Main entry point to the parser. Appends the newline character
-- to the input string.
parseParas :: String -- ^ String to parse
           -> MetaDoc mod Identifier
parseParas input = case parseParasState input of
  (state, a) -> MetaDoc { _meta = Meta { _version = parserStateSince state }
                        , _doc = a
                        }

parseParasState :: String -> (ParserState, DocH mod Identifier)
parseParasState = parse (p <* skipSpace) . encodeUtf8 . (++ "\n")
  where
    p :: Parser (DocH mod Identifier)
    p = docConcat <$> paragraph `sepBy` many (skipHorizontalSpace *> "\n")

parseParagraphs :: String -> Parser (DocH mod Identifier)
parseParagraphs input = case parseParasState input of
  (state, a) -> setParserState state >> return a

-- | Parse a text paragraph. Actually just a wrapper over 'parseStringBS' which
-- drops leading whitespace and encodes the string to UTF8 first.
parseString :: String -> DocH mod Identifier
parseString = parseStringBS . encodeUtf8 . dropWhile isSpace

parseStringBS :: BS.ByteString -> DocH mod Identifier
parseStringBS = snd . parse p
  where
    p :: Parser (DocH mod Identifier)
    p = docConcat <$> many (monospace <|> anchor <|> identifier <|> moduleName
                            <|> picture <|> markdownImage <|> hyperlink <|> bold
                            <|> emphasis <|> encodedChar <|> string'
                            <|> skipSpecialChar)

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
specialChar = "_/<@\"&'`# "

-- | Plain, regular parser for text. Called as one of the last parsers
-- to ensure that we have already given a chance to more meaningful parsers
-- before capturing their characers.
string' :: Parser (DocH mod a)
string' = DocString . unescape . decodeUtf8 <$> takeWhile1_ (`notElem` specialChar)
  where
    unescape "" = ""
    unescape ('\\':x:xs) = x : unescape xs
    unescape (x:xs) = x : unescape xs

-- | Skips a single special character and treats it as a plain string.
-- This is done to skip over any special characters belonging to other
-- elements but which were not deemed meaningful at their positions.
skipSpecialChar :: Parser (DocH mod a)
skipSpecialChar = DocString . return <$> satisfy (`elem` specialChar)

-- | Emphasis parser.
--
-- >>> parseString "/Hello world/"
-- DocEmphasis (DocString "Hello world")
emphasis :: Parser (DocH mod Identifier)
emphasis = DocEmphasis . parseStringBS <$>
  mfilter ('\n' `BS.notElem`) ("/" *> takeWhile1_ (/= '/') <* "/")

-- | Bold parser.
--
-- >>> parseString "__Hello world__"
-- DocBold (DocString "Hello world")
bold :: Parser (DocH mod Identifier)
bold = DocBold . parseStringBS <$> disallowNewline ("__" *> takeUntil "__")

disallowNewline :: Parser BS.ByteString -> Parser BS.ByteString
disallowNewline = mfilter ('\n' `BS.notElem`)

-- | Like `takeWhile`, but unconditionally take escaped characters.
takeWhile_ :: (Char -> Bool) -> Parser BS.ByteString
takeWhile_ p = scan False p_
  where
    p_ escaped c
      | escaped = Just False
      | not $ p c = Nothing
      | otherwise = Just (c == '\\')

-- | Like `takeWhile1`, but unconditionally take escaped characters.
takeWhile1_ :: (Char -> Bool) -> Parser BS.ByteString
takeWhile1_ = mfilter (not . BS.null) . takeWhile_

-- | Text anchors to allow for jumping around the generated documentation.
--
-- >>> parseString "#Hello world#"
-- DocAName "Hello world"
anchor :: Parser (DocH mod a)
anchor = DocAName . decodeUtf8 <$>
         disallowNewline ("#" *> takeWhile1_ (/= '#') <* "#")

-- | Monospaced strings.
--
-- >>> parseString "@cruel@"
-- DocMonospaced (DocString "cruel")
monospace :: Parser (DocH mod Identifier)
monospace = DocMonospaced . parseStringBS
            <$> ("@" *> takeWhile1_ (/= '@') <* "@")

-- | Module names: we try our reasonable best to only allow valid
-- Haskell module names, with caveat about not matching on technically
-- valid unicode symbols.
moduleName :: Parser (DocH mod a)
moduleName = DocModule <$> (char '"' *> modid <* char '"')
  where
    modid = intercalate "." <$> conid `sepBy1` "."
    conid = (:)
      <$> satisfy isAsciiUpper
      -- NOTE: According to Haskell 2010 we should actually only
      -- accept {small | large | digit | ' } here.  But as we can't
      -- match on unicode characters, this is currently not possible.
      -- Note that we allow ‘#’ to suport anchors.
      <*> (decodeUtf8 <$> takeWhile (`notElem` (" .&[{}(=*)+]!|@/;,^?\"\n"::String)))

-- | Picture parser, surrounded by \<\< and \>\>. It's possible to specify
-- a title for the picture.
--
-- >>> parseString "<<hello.png>>"
-- DocPic (Picture {pictureUri = "hello.png", pictureTitle = Nothing})
-- >>> parseString "<<hello.png world>>"
-- DocPic (Picture {pictureUri = "hello.png", pictureTitle = Just "world"})
picture :: Parser (DocH mod a)
picture = DocPic . makeLabeled Picture . decodeUtf8
          <$> disallowNewline ("<<" *> takeUntil ">>")

markdownImage :: Parser (DocH mod a)
markdownImage = fromHyperlink <$> ("!" *> linkParser)
  where
    fromHyperlink (Hyperlink url label) = DocPic (Picture url label)

-- | Paragraph parser, called by 'parseParas'.
paragraph :: Parser (DocH mod Identifier)
paragraph = examples <|> skipSpace *> (
      since
  <|> unorderedList
  <|> orderedList
  <|> birdtracks
  <|> codeblock
  <|> property
  <|> header
  <|> textParagraphThatStartsWithMarkdownLink
  <|> definitionList
  <|> docParagraph <$> textParagraph
  )

since :: Parser (DocH mod a)
since = ("@since " *> version <* skipHorizontalSpace <* endOfLine) >>= setSince >> return DocEmpty
  where
    version = decimal `sepBy1'` "."

-- | Headers inside the comment denoted with @=@ signs, up to 6 levels
-- deep.
--
-- >>> snd <$> parseOnly header "= Hello"
-- Right (DocHeader (Header {headerLevel = 1, headerTitle = DocString "Hello"}))
-- >>> snd <$> parseOnly header "== World"
-- Right (DocHeader (Header {headerLevel = 2, headerTitle = DocString "World"}))
header :: Parser (DocH mod Identifier)
header = do
  let psers = map (string . encodeUtf8 . concat . flip replicate "=") [6, 5 .. 1]
      pser = foldl1 (<|>) psers
  delim <- decodeUtf8 <$> pser
  line <- skipHorizontalSpace *> nonEmptyLine >>= return . parseString
  rest <- paragraph <|> return DocEmpty
  return $ DocHeader (Header (length delim) line) `docAppend` rest

textParagraph :: Parser (DocH mod Identifier)
textParagraph = parseString . intercalate "\n" <$> many1 nonEmptyLine

textParagraphThatStartsWithMarkdownLink :: Parser (DocH mod Identifier)
textParagraphThatStartsWithMarkdownLink = docParagraph <$> (docAppend <$> markdownLink <*> optionalTextParagraph)
  where
    optionalTextParagraph :: Parser (DocH mod Identifier)
    optionalTextParagraph = (docAppend <$> whitespace <*> textParagraph) <|> pure DocEmpty

    whitespace :: Parser (DocH mod a)
    whitespace = DocString <$> (f <$> takeHorizontalSpace <*> optional "\n")
      where
        f :: BS.ByteString -> Maybe BS.ByteString -> String
        f xs (fromMaybe "" -> x)
          | BS.null (xs <> x) = ""
          | otherwise = " "

-- | Parses unordered (bullet) lists.
unorderedList :: Parser (DocH mod Identifier)
unorderedList = DocUnorderedList <$> p
  where
    p = ("*" <|> "-") *> innerList p

-- | Parses ordered lists (numbered or dashed).
orderedList :: Parser (DocH mod Identifier)
orderedList = DocOrderedList <$> p
  where
    p = (paren <|> dot) *> innerList p
    dot = (decimal :: Parser Int) <* "."
    paren = "(" *> decimal <* ")"

-- | Generic function collecting any further lines belonging to the
-- list entry and recursively collecting any further lists in the
-- same paragraph. Usually used as
--
-- > someListFunction = listBeginning *> innerList someListFunction
innerList :: Parser [DocH mod Identifier] -> Parser [DocH mod Identifier]
innerList item = do
  c <- takeLine
  (cs, items) <- more item
  let contents = docParagraph . parseString . dropNLs . unlines $ c : cs
  return $ case items of
    Left p -> [contents `docAppend` p]
    Right i -> contents : i

-- | Parses definition lists.
definitionList :: Parser (DocH mod Identifier)
definitionList = DocDefList <$> p
  where
    p = do
      label <- "[" *> (parseStringBS <$> takeWhile1 (`notElem` ("]\n" :: String))) <* ("]" <* optional ":")
      c <- takeLine
      (cs, items) <- more p
      let contents = parseString . dropNLs . unlines $ c : cs
      return $ case items of
        Left x -> [(label, contents `docAppend` x)]
        Right i -> (label, contents) : i

-- | Drops all trailing newlines.
dropNLs :: String -> String
dropNLs = reverse . dropWhile (== '\n') . reverse

-- | Main worker for 'innerList' and 'definitionList'.
-- We need the 'Either' here to be able to tell in the respective functions
-- whether we're dealing with the next list or a nested paragraph.
more :: Monoid a => Parser a
     -> Parser ([String], Either (DocH mod Identifier) a)
more item = innerParagraphs <|> moreListItems item
            <|> moreContent item <|> pure ([], Right mempty)

-- | Used by 'innerList' and 'definitionList' to parse any nested paragraphs.
innerParagraphs :: Parser ([String], Either (DocH mod Identifier) a)
innerParagraphs = (,) [] . Left <$> ("\n" *> indentedParagraphs)

-- | Attempts to fetch the next list if possibly. Used by 'innerList' and
-- 'definitionList' to recursively grab lists that aren't separated by a whole
-- paragraph.
moreListItems :: Parser a
              -> Parser ([String], Either (DocH mod Identifier) a)
moreListItems item = (,) [] . Right <$> (skipSpace *> item)

-- | Helper for 'innerList' and 'definitionList' which simply takes
-- a line of text and attempts to parse more list content with 'more'.
moreContent :: Monoid a => Parser a
            -> Parser ([String], Either (DocH mod Identifier) a)
moreContent item = first . (:) <$> nonEmptyLine <*> more item

-- | Parses an indented paragraph.
-- The indentation is 4 spaces.
indentedParagraphs :: Parser (DocH mod Identifier)
indentedParagraphs = (concat <$> dropFrontOfPara "    ") >>= parseParagraphs

-- | Grab as many fully indented paragraphs as we can.
dropFrontOfPara :: Parser BS.ByteString -> Parser [String]
dropFrontOfPara sp = do
  currentParagraph <- some (sp *> takeNonEmptyLine)
  followingParagraphs <-
    skipHorizontalSpace *> nextPar -- we have more paragraphs to take
    <|> skipHorizontalSpace *> nlList -- end of the ride, remember the newline
    <|> endOfInput *> return [] -- nothing more to take at all
  return (currentParagraph ++ followingParagraphs)
  where
    nextPar = (++) <$> nlList <*> dropFrontOfPara sp
    nlList = "\n" *> return ["\n"]

nonSpace :: BS.ByteString -> Parser BS.ByteString
nonSpace xs
  | not $ any (not . isSpace) $ decodeUtf8 xs = fail "empty line"
  | otherwise = return xs

-- | Takes a non-empty, not fully whitespace line.
--
--  Doesn't discard the trailing newline.
takeNonEmptyLine :: Parser String
takeNonEmptyLine = do
    (++ "\n") . decodeUtf8 <$> (takeWhile1 (/= '\n') >>= nonSpace) <* "\n"

-- | Blocks of text of the form:
--
-- >> foo
-- >> bar
-- >> baz
--
birdtracks :: Parser (DocH mod a)
birdtracks = DocCodeBlock . DocString . intercalate "\n" . stripSpace <$> many1 line
  where
    line = skipHorizontalSpace *> ">" *> takeLine

stripSpace :: [String] -> [String]
stripSpace = fromMaybe <*> mapM strip'
  where
    strip' (' ':xs') = Just xs'
    strip' "" = Just ""
    strip' _  = Nothing

-- | Parses examples. Examples are a paragraph level entitity (separated by an empty line).
-- Consecutive examples are accepted.
examples :: Parser (DocH mod a)
examples = DocExamples <$> (many (skipHorizontalSpace *> "\n") *> go)
  where
    go :: Parser [Example]
    go = do
      prefix <- decodeUtf8 <$> takeHorizontalSpace <* ">>>"
      expr <- takeLine
      (rs, es) <- resultAndMoreExamples
      return (makeExample prefix expr rs : es)
      where
        resultAndMoreExamples :: Parser ([String], [Example])
        resultAndMoreExamples = moreExamples <|> result <|> pure ([], [])
          where
            moreExamples :: Parser ([String], [Example])
            moreExamples = (,) [] <$> go

            result :: Parser ([String], [Example])
            result = first . (:) <$> nonEmptyLine <*> resultAndMoreExamples

    makeExample :: String -> String -> [String] -> Example
    makeExample prefix expression res =
      Example (strip expression) result
      where
        result = map (substituteBlankLine . tryStripPrefix) res

        tryStripPrefix xs = fromMaybe xs (stripPrefix prefix xs)

        substituteBlankLine "<BLANKLINE>" = ""
        substituteBlankLine xs = xs

nonEmptyLine :: Parser String
nonEmptyLine = mfilter (any (not . isSpace)) takeLine

takeLine :: Parser String
takeLine = decodeUtf8 <$> takeWhile (/= '\n') <* endOfLine

endOfLine :: Parser ()
endOfLine = void "\n" <|> endOfInput

-- | Property parser.
--
-- >>> snd <$> parseOnly property "prop> hello world"
-- Right (DocProperty "hello world")
property :: Parser (DocH mod a)
property = DocProperty . strip . decodeUtf8 <$> ("prop>" *> takeWhile1 (/= '\n'))

-- |
-- Paragraph level codeblock. Anything between the two delimiting \@ is parsed
-- for markup.
codeblock :: Parser (DocH mod Identifier)
codeblock =
  DocCodeBlock . parseStringBS . dropSpaces
  <$> ("@" *> skipHorizontalSpace *> "\n" *> block' <* "@")
  where
    dropSpaces xs =
      let rs = decodeUtf8 xs
      in case splitByNl rs of
        [] -> xs
        ys -> case last ys of
          ' ':_ -> case mapM dropSpace ys of
            Nothing -> xs
            Just zs -> encodeUtf8 $ intercalate "\n" zs
          _ -> xs

    -- This is necessary because ‘lines’ swallows up a trailing newline
    -- and we lose information about whether the last line belongs to @ or to
    -- text which we need to decide whether we actually want to be dropping
    -- anything at all.
    splitByNl = unfoldr (\x -> case x of
                                 '\n':s -> Just (span (/= '\n') s)
                                 _      -> Nothing)
                . ('\n' :)

    dropSpace "" = Just ""
    dropSpace (' ':xs) = Just xs
    dropSpace _ = Nothing

    block' = scan False p
      where
        p isNewline c
          | isNewline && c == '@' = Nothing
          | isNewline && isSpace c = Just isNewline
          | otherwise = Just $ c == '\n'

hyperlink :: Parser (DocH mod a)
hyperlink = DocHyperlink . makeLabeled Hyperlink . decodeUtf8
              <$> disallowNewline ("<" *> takeUntil ">")
            <|> autoUrl
            <|> markdownLink

markdownLink :: Parser (DocH mod a)
markdownLink = DocHyperlink <$> linkParser

linkParser :: Parser Hyperlink
linkParser = flip Hyperlink <$> label <*> (whitespace *> url)
  where
    label :: Parser (Maybe String)
    label = Just . strip . decode <$> ("[" *> takeUntil "]")

    whitespace :: Parser ()
    whitespace = skipHorizontalSpace <* optional ("\n" *> skipHorizontalSpace)

    url :: Parser String
    url = rejectWhitespace (decode <$> ("(" *> takeUntil ")"))

    rejectWhitespace :: MonadPlus m => m String -> m String
    rejectWhitespace = mfilter (all (not . isSpace))

    decode :: BS.ByteString -> String
    decode = removeEscapes . decodeUtf8

-- | Looks for URL-like things to automatically hyperlink even if they
-- weren't marked as links.
autoUrl :: Parser (DocH mod a)
autoUrl = mkLink <$> url
  where
    url = mappend <$> ("http://" <|> "https://" <|> "ftp://") <*> takeWhile1 (not . isSpace)
    mkLink :: BS.ByteString -> DocH mod a
    mkLink s = case unsnoc s of
      Just (xs, x) | x `elem` (",.!?" :: String) -> DocHyperlink (Hyperlink (decodeUtf8 xs) Nothing) `docAppend` DocString [x]
      _ -> DocHyperlink (Hyperlink (decodeUtf8 s) Nothing)

-- | Parses strings between identifier delimiters. Consumes all input that it
-- deems to be valid in an identifier. Note that it simply blindly consumes
-- characters and does no actual validation itself.
parseValid :: Parser String
parseValid = p some
  where
    idChar = satisfy (`elem` ("_.!#$%&*+/<=>?@\\|-~:^"::String))
             <|> digit <|> letter_ascii
    p p' = do
      vs' <- p' $ utf8String "⋆" <|> return <$> idChar
      let vs = concat vs'
      c <- peekChar'
      case c of
        '`' -> return vs
        '\'' -> (\x -> vs ++ "'" ++ x) <$> ("'" *> p many') <|> return vs
        _ -> fail "outofvalid"

-- | Parses UTF8 strings from ByteString streams.
utf8String :: String -> Parser String
utf8String x = decodeUtf8 <$> string (encodeUtf8 x)

-- | Parses identifiers with help of 'parseValid'. Asks GHC for
-- 'String' from the string it deems valid.
identifier :: Parser (DocH mod Identifier)
identifier = do
  o <- idDelim
  vid <- parseValid
  e <- idDelim
  return $ DocIdentifier (o, vid, e)
  where
    idDelim = char '\'' <|> char '`'
