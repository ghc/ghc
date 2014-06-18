{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import           Control.Monad (void, mfilter)
import           Data.Attoparsec.ByteString.Char8 hiding (parse, take, endOfLine)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (chr, isAsciiUpper)
import           Data.List (stripPrefix, intercalate, unfoldr)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Documentation.Haddock.Doc
import           Documentation.Haddock.Parser.Util
import           Documentation.Haddock.Types
import           Documentation.Haddock.Utf8
import           Prelude hiding (takeWhile)

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

parse :: Parser a -> BS.ByteString -> a
parse p = either err id . parseOnly (p <* endOfInput)
  where
    err = error . ("Haddock.Parser.parse: " ++)

-- | Main entry point to the parser. Appends the newline character
-- to the input string.
parseParas :: String -- ^ String to parse
           -> DocH mod Identifier
parseParas = parse (p <* skipSpace) . encodeUtf8 . (++ "\n")
  where
    p :: Parser (DocH mod Identifier)
    p = mconcat <$> paragraph `sepBy` many (skipHorizontalSpace *> "\n")

-- | Parse a text paragraph. Actually just a wrapper over 'parseStringBS' which
-- drops leading whitespace and encodes the string to UTF8 first.
parseString :: String -> DocH mod Identifier
parseString = parseStringBS . encodeUtf8 . dropWhile isSpace

parseStringBS :: BS.ByteString -> DocH mod Identifier
parseStringBS = parse p
  where
    p :: Parser (DocH mod Identifier)
    p = mconcat <$> many (monospace <|> anchor <|> identifier <|> moduleName
                          <|> picture <|> hyperlink <|> autoUrl <|> bold
                          <|> emphasis <|> encodedChar <|> string'
                          <|> skipSpecialChar)

-- | Parses and processes
-- <https://en.wikipedia.org/wiki/Numeric_character_reference Numeric character references>
--
-- >>> parseOnly encodedChar "&#65;"
-- Right (DocString "A")
encodedChar :: Parser (DocH mod a)
encodedChar = "&#" *> c <* ";"
  where
    c = DocString . return . chr <$> num
    num = hex <|> decimal
    hex = ("x" <|> "X") *> hexadecimal

specialChar :: [Char]
specialChar = "_/<@\"&'`"

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
-- >>> parseOnly emphasis "/Hello world/"
-- Right (DocEmphasis (DocString "Hello world"))
emphasis :: Parser (DocH mod Identifier)
emphasis = DocEmphasis . parseStringBS <$>
  mfilter ('\n' `BS.notElem`) ("/" *> takeWhile1_ (/= '/') <* "/")

-- | Bold parser.
--
-- >>> parseOnly bold "__Hello world__"
-- Right (DocBold (DocString "Hello world"))
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
-- >>> parseOnly anchor "#Hello world#"
-- Right (DocAName "Hello world")
anchor :: Parser (DocH mod a)
anchor = DocAName . decodeUtf8 <$> ("#" *> takeWhile1 (`notElem` "#\n") <* "#")

-- | Monospaced strings.
--
-- >>> parseOnly monospace "@cruel@"
-- Right (DocMonospaced (DocString "cruel"))
monospace :: Parser (DocH mod Identifier)
monospace = DocMonospaced . parseStringBS <$> ("@" *> takeWhile1_ (/= '@') <* "@")

moduleName :: Parser (DocH mod a)
moduleName = DocModule <$> (char '"' *> modid <* char '"')
  where
    modid = intercalate "." <$> conid `sepBy1` "."
    conid = (:)
      <$> satisfy isAsciiUpper
      -- NOTE: According to Haskell 2010 we should actually only
      -- accept {small | large | digit | ' } here.  But as we can't
      -- match on unicode characters, this is currently not possible.
      <*> (decodeUtf8 <$> takeWhile (`notElem` " .&[{}(=*)+]!#|@/;,^?\"\n"))

-- | Picture parser, surrounded by \<\< and \>\>. It's possible to specify
-- a title for the picture.
--
-- >>> parseOnly picture "<<hello.png>>"
-- Right (DocPic (Picture {pictureUri = "hello.png", pictureTitle = Nothing}))
-- >>> parseOnly picture "<<hello.png world>>"
-- Right (DocPic (Picture {pictureUri = "hello.png", pictureTitle = Just "world"}))
picture :: Parser (DocH mod a)
picture = DocPic . makeLabeled Picture . decodeUtf8
          <$> disallowNewline ("<<" *> takeUntil ">>")

-- | Paragraph parser, called by 'parseParas'.
paragraph :: Parser (DocH mod Identifier)
paragraph = examples <|> skipSpace *> (list <|> birdtracks <|> codeblock
                                       <|> property <|> header
                                       <|> textParagraph)

-- | Headers inside the comment denoted with @=@ signs, up to 6 levels
-- deep.
header :: Parser (DocH mod Identifier)
header = do
  let psers = map (string . encodeUtf8 . concat . flip replicate "=") [6, 5 .. 1]
      pser = foldl1 (<|>) psers
  delim <- decodeUtf8 <$> pser
  line <- skipHorizontalSpace *> nonEmptyLine >>= return . parseString
  rest <- paragraph <|> return mempty
  return $ DocParagraph (DocHeader (Header (length delim) line)) <> rest

textParagraph :: Parser (DocH mod Identifier)
textParagraph = docParagraph . parseString . intercalate "\n" <$> many1 nonEmptyLine

-- | List parser, called by 'paragraph'.
list :: Parser (DocH mod Identifier)
list = DocUnorderedList <$> unorderedList
       <|> DocOrderedList <$> orderedList
       <|> DocDefList <$> definitionList

-- | Parses unordered (bullet) lists.
unorderedList :: Parser [DocH mod Identifier]
unorderedList = ("*" <|> "-") *> innerList unorderedList

-- | Parses ordered lists (numbered or dashed).
orderedList :: Parser [DocH mod Identifier]
orderedList = (paren <|> dot) *> innerList orderedList
  where
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
    Left p -> [contents `joinPara` p]
    Right i -> contents : i

-- | Parses definition lists.
definitionList :: Parser [(DocH mod Identifier, DocH mod Identifier)]
definitionList = do
  label <- "[" *> (parseStringBS <$> takeWhile1 (`notElem` "]\n")) <* "]"
  c <- takeLine
  (cs, items) <- more definitionList
  let contents = parseString . dropNLs . unlines $ c : cs
  return $ case items of
    Left p -> [(label, contents `joinPara` p)]
    Right i -> (label, contents) : i

-- | If possible, appends two 'Doc's under a 'DocParagraph' rather than
-- outside of it. This allows to get structures like
--
-- @DocParagraph (DocAppend … …)@
--
-- rather than
--
-- @DocAppend (DocParagraph …) …@
joinPara :: DocH mod id -> DocH mod id -> DocH mod id
joinPara (DocParagraph p) c = docParagraph $ p <> c
joinPara d p = d <> p

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

-- | Use by 'innerList' and 'definitionList' to parse any nested paragraphs.
innerParagraphs :: Parser ([String], Either (DocH mod Identifier) a)
innerParagraphs = (,) [] . Left <$> ("\n" *> indentedParagraphs)

-- | Attemps to fetch the next list if possibly. Used by 'innerList' and
-- 'definitionList' to recursivly grab lists that aren't separated by a whole
-- paragraph.
moreListItems :: Parser a
              -> Parser ([String], Either (DocH mod Identifier) a)
moreListItems item = (,) [] . Right <$> (skipSpace *> item)

-- | Helper for 'innerList' and 'definitionList' which simply takes
-- a line of text and attempts to parse more list content with 'more'.
moreContent :: Monoid a => Parser a
            -> Parser ([String], Either (DocH mod Identifier) a)
moreContent item = first . (:) <$> nonEmptyLine <*> more item

-- | Runs the 'parseParas' parser on an indented paragraph.
-- The indentation is 4 spaces.
indentedParagraphs :: Parser (DocH mod Identifier)
indentedParagraphs = parseParas . concat <$> dropFrontOfPara "    "

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
-- @
-- > foo
-- > bar
-- > baz
-- @
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
-- >>> parseOnly property "prop> hello world"
-- Right (DocProperty "hello world")
property :: Parser (DocH mod a)
property = DocProperty . strip . decodeUtf8 <$> ("prop>" *> takeWhile1 (/= '\n'))

-- |
-- Paragraph level codeblock. Anything between the two delimiting @ is parsed
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
    splitByNl = unfoldr (\case '\n':s -> Just (span (/= '\n') s)
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

-- | Parses links that were specifically marked as such.
hyperlink :: Parser (DocH mod a)
hyperlink = DocHyperlink . makeLabeled Hyperlink . decodeUtf8
              <$> disallowNewline ("<" *> takeUntil ">")
            <|> autoUrl

-- | Looks for URL-like things to automatically hyperlink even if they
-- weren't marked as links.
autoUrl :: Parser (DocH mod a)
autoUrl = mkLink <$> url
  where
    url = mappend <$> ("http://" <|> "https://" <|> "ftp://") <*> takeWhile1 (not . isSpace)
    mkLink :: BS.ByteString -> DocH mod a
    mkLink s = case unsnoc s of
      Just (xs, x) | x `elem` ",.!?" -> DocHyperlink (Hyperlink (decodeUtf8 xs) Nothing) <> DocString [x]
      _ -> DocHyperlink (Hyperlink (decodeUtf8 s) Nothing)

-- | Parses strings between identifier delimiters. Consumes all input that it
-- deems to be valid in an identifier. Note that it simply blindly consumes
-- characters and does no actual validation itself.
parseValid :: Parser String
parseValid = do
  vs' <- many' $ utf8String "⋆" <|> return <$> idChar
  let vs = concat vs'
  c <- peekChar
  case c of
    Just '`' -> return vs
    Just '\'' -> (\x -> vs ++ "'" ++ x) <$> ("'" *> parseValid)
                 <|> return vs
    _ -> fail "outofvalid"
  where
    idChar = satisfy (`elem` "_.!#$%&*+/<=>?@\\|-~:^")
             <|> digit <|> letter_ascii

-- | Parses UTF8 strings from ByteString streams.
utf8String :: String -> Parser String
utf8String x = decodeUtf8 <$> string (encodeUtf8 x)

-- | Parses identifiers with help of 'parseValid'. Asks GHC for 'String' from the
-- string it deems valid.
identifier :: Parser (DocH mod Identifier)
identifier = do
  o <- idDelim
  vid <- parseValid
  e <- idDelim
  return $ DocIdentifier (o, vid, e)
  where
    idDelim = char '\'' <|> char '`'
