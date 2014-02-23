{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving
             , FlexibleInstances, UndecidableInstances
             , IncoherentInstances #-}
-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable

module Haddock.Parser (parseString, parseParas, parseStringMaybe, parseParasMaybe) where

import           Prelude hiding (takeWhile)
import           Control.Arrow (first)
import           Control.Monad (void, mfilter)
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 hiding (parse, take, endOfLine)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (chr, isAsciiUpper)
import           Data.List (stripPrefix, intercalate)
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           DynFlags
import           FastString (mkFastString)
import           Haddock.Doc
import           Haddock.Types
import           Lexer (mkPState, unP, ParseResult(POk))
import           Parser (parseIdentifier)
import           RdrName
import           SrcLoc (mkRealSrcLoc, unLoc)
import           StringBuffer (stringToStringBuffer)
import           Haddock.Utf8
import           Haddock.Parser.Util

{-# DEPRECATED parseParasMaybe "use `parseParas` instead" #-}
parseParasMaybe :: DynFlags -> String -> Maybe (Doc RdrName)
parseParasMaybe d = Just . parseParas d

{-# DEPRECATED parseStringMaybe "use `parseString` instead" #-}
parseStringMaybe :: DynFlags -> String -> Maybe (Doc RdrName)
parseStringMaybe d = Just . parseString d

parse :: Parser a -> BS.ByteString -> a
parse p = either err id . parseOnly (p <* endOfInput)
  where
    err = error . ("Haddock.Parser.parse: " ++)

-- | Main entry point to the parser. Appends the newline character
-- to the input string.
parseParas :: DynFlags
              -> String -- ^ String to parse
              -> Doc RdrName
parseParas d = parse (p <* skipSpace) . encodeUtf8 . (++ "\n")
  where
    p :: Parser (Doc RdrName)
    p = mconcat <$> paragraph d `sepBy` many (skipHorizontalSpace *> "\n")

-- | Parse a text paragraph.
parseString :: DynFlags -> String -> Doc RdrName
parseString d = parseStringBS d . encodeUtf8 . dropWhile isSpace

parseStringBS :: DynFlags -> BS.ByteString -> Doc RdrName
parseStringBS d = parse p
  where
    p :: Parser (Doc RdrName)
    p = mconcat <$> many (monospace d <|> anchor <|> identifier d
                          <|> moduleName <|> picture <|> hyperlink <|> autoUrl <|> bold d
                          <|> emphasis d <|> encodedChar <|> string' <|> skipSpecialChar)

-- | Parses and processes
-- <https://en.wikipedia.org/wiki/Numeric_character_reference Numeric character references>
--
-- >>> parseOnly encodedChar "&#65;&#66;&#67;"
-- Right (DocString "ABC")
encodedChar :: Parser (Doc a)
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
string' :: Parser (Doc a)
string' = DocString . unescape . decodeUtf8 <$> takeWhile1_ (`notElem` specialChar)
  where
    unescape "" = ""
    unescape ('\\':x:xs) = x : unescape xs
    unescape (x:xs) = x : unescape xs

-- | Skips a single special character and treats it as a plain string.
-- This is done to skip over any special characters belonging to other
-- elements but which were not deemed meaningful at their positions.
skipSpecialChar :: Parser (Doc a)
skipSpecialChar = DocString . return <$> satisfy (`elem` specialChar)

-- | Emphasis parser.
--
-- >>> parseOnly emphasis "/Hello world/"
-- Right (DocEmphasis (DocString "Hello world"))
emphasis :: DynFlags -> Parser (Doc RdrName)
emphasis d = DocEmphasis . parseStringBS d <$>
  mfilter ('\n' `BS.notElem`) ("/" *> takeWhile1_ (/= '/') <* "/")

-- | Bold parser.
--
-- >>> parseOnly bold "__Hello world__"
-- Right (DocBold (DocString "Hello world"))
bold :: DynFlags -> Parser (Doc RdrName)
bold d = DocBold . parseStringBS d <$> disallowNewline ("__" *> takeUntil "__")

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
anchor :: Parser (Doc a)
anchor = DocAName . decodeUtf8 <$> ("#" *> takeWhile1 (`notElem` "#\n") <* "#")

-- | Monospaced strings.
--
-- >>> parseOnly (monospace dynflags) "@cruel@"
-- Right (DocMonospaced (DocString "cruel"))
monospace :: DynFlags -> Parser (Doc RdrName)
monospace d = DocMonospaced . parseStringBS d <$> ("@" *> takeWhile1_ (/= '@') <* "@")

moduleName :: Parser (Doc a)
moduleName = DocModule <$> (char '"' *> modid <* char '"')
  where
    modid = intercalate "." <$> conid `sepBy1` "."
    conid = (:)
      <$> satisfy isAsciiUpper
      -- NOTE: According to Haskell 2010 we shouldd actually only
      -- accept {small | large | digit | ' } here.  But as we can't
      -- match on unicode characters, this is currently not possible.
      <*> (decodeUtf8 <$> takeWhile (`notElem` " .&[{}(=*)+]!#|@/;,^?\"\n"))

-- | Picture parser, surrounded by \<\< and \>\>. It's possible to specify
-- a title for the picture.
--
-- >>> parseOnly picture "<<hello.png>>"
-- Right (DocPic (Picture "hello.png" Nothing))
-- >>> parseOnly picture "<<hello.png world>>"
-- Right (DocPic (Picture "hello.png" (Just "world")))
picture :: Parser (Doc a)
picture = DocPic . makeLabeled Picture . decodeUtf8
          <$> disallowNewline ("<<" *> takeUntil ">>")

-- | Paragraph parser, called by 'parseParas'.
paragraph :: DynFlags -> Parser (Doc RdrName)
paragraph d = examples <|> skipSpace *> (list d <|> birdtracks <|> codeblock d
                                         <|> property <|> header d
                                         <|> textParagraph d)

header :: DynFlags -> Parser (Doc RdrName)
header d = do
  let psers = map (string . encodeUtf8 . concat . flip replicate "=") [6, 5 .. 1]
      pser = foldl1 (<|>) psers
  delim <- decodeUtf8 <$> pser
  line <- skipHorizontalSpace *> nonEmptyLine >>= return . parseString d
  rest <- paragraph d <|> return mempty
  return $ docAppend (DocParagraph (DocHeader (Header (length delim) line))) rest

textParagraph :: DynFlags -> Parser (Doc RdrName)
textParagraph d = docParagraph . parseString d . intercalate "\n" <$> many1 nonEmptyLine

-- | List parser, called by 'paragraph'.
list :: DynFlags -> Parser (Doc RdrName)
list d = DocUnorderedList <$> unorderedList d
         <|> DocOrderedList <$> orderedList d
         <|> DocDefList <$> definitionList d

-- | Parses unordered (bullet) lists.
unorderedList :: DynFlags -> Parser [Doc RdrName]
unorderedList d = ("*" <|> "-") *> innerList (unorderedList d) d

-- | Parses ordered lists (numbered or dashed).
orderedList :: DynFlags -> Parser [Doc RdrName]
orderedList d = (paren <|> dot) *> innerList (orderedList d) d
  where
    dot = (decimal :: Parser Int) <* "."
    paren = "(" *> decimal <* ")"

-- | Generic function collecting any further lines belonging to the
-- list entry and recursively collecting any further lists in the
-- same paragraph. Usually used as
--
-- > someListFunction dynflags = listBeginning *> innerList someListFunction dynflags
innerList :: Parser [Doc RdrName] -> DynFlags -> Parser [Doc RdrName]
innerList item d = do
  c <- takeLine
  (cs, items) <- more item d
  let contents = docParagraph . parseString d . dropNLs . unlines $ c : cs
  return $ case items of
    Left p -> [contents `joinPara` p]
    Right i -> contents : i

-- | Parses definition lists.
definitionList :: DynFlags -> Parser [(Doc RdrName, Doc RdrName)]
definitionList d = do
  label <- "[" *> (parseStringBS d <$> takeWhile1 (`notElem` "]\n")) <* "]"
  c <- takeLine
  (cs, items) <- more (definitionList d) d
  let contents = parseString d . dropNLs . unlines $ c : cs
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
joinPara :: Doc id -> Doc id -> Doc id
joinPara (DocParagraph p) c = docParagraph $ docAppend p c
joinPara d p = docAppend d p

-- | Drops all trailing newlines.
dropNLs :: String -> String
dropNLs = reverse . dropWhile (== '\n') . reverse

-- | Main worker for 'innerList' and 'definitionList'.
-- We need the 'Either' here to be able to tell in the respective functions
-- whether we're dealing with the next list or a nested paragraph.
more :: Monoid a => Parser a -> DynFlags
     -> Parser ([String], Either (Doc RdrName) a)
more item d = innerParagraphs d <|> moreListItems item
              <|> moreContent item d <|> pure ([], Right mempty)

-- | Use by 'innerList' and 'definitionList' to parse any nested paragraphs.
innerParagraphs :: DynFlags
                -> Parser ([String], Either (Doc RdrName) a)
innerParagraphs d = (,) [] . Left <$> ("\n" *> indentedParagraphs d)

-- | Attemps to fetch the next list if possibly. Used by 'innerList' and
-- 'definitionList' to recursivly grab lists that aren't separated by a whole
-- paragraph.
moreListItems :: Parser a
              -> Parser ([String], Either (Doc RdrName) a)
moreListItems item = (,) [] . Right <$> (skipSpace *> item)

-- | Helper for 'innerList' and 'definitionList' which simply takes
-- a line of text and attempts to parse more list content with 'more'.
moreContent :: Monoid a => Parser a -> DynFlags
            -> Parser ([String], Either (Doc RdrName) a)
moreContent item d = first . (:) <$> nonEmptyLine <*> more item d

-- | Runs the 'parseParas' parser on an indented paragraph.
-- The indentation is 4 spaces.
indentedParagraphs :: DynFlags -> Parser (Doc RdrName)
indentedParagraphs d = parseParas d . concat <$> dropFrontOfPara "    "

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

birdtracks :: Parser (Doc a)
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
examples :: Parser (Doc a)
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
property :: Parser (Doc a)
property = DocProperty . strip . decodeUtf8 <$> ("prop>" *> takeWhile1 (/= '\n'))

-- |
-- Paragraph level codeblock. Anything between the two delimiting @ is parsed
-- for markup.
codeblock :: DynFlags -> Parser (Doc RdrName)
codeblock d =
  DocCodeBlock . parseStringBS d <$> ("@" *> skipHorizontalSpace *> "\n" *> block' <* "@")
  where
    block' = scan False p
      where
        p isNewline c
          | isNewline && c == '@' = Nothing
          | isNewline && isSpace c = Just isNewline
          | otherwise = Just $ c == '\n'

hyperlink :: Parser (Doc a)
hyperlink = DocHyperlink . makeLabeled Hyperlink . decodeUtf8
              <$> disallowNewline ("<" *> takeUntil ">")
            <|> autoUrl

autoUrl :: Parser (Doc a)
autoUrl = mkLink <$> url
  where
    url = mappend <$> ("http://" <|> "https://" <|> "ftp://") <*> takeWhile1 (not . isSpace)
    mkLink :: BS.ByteString -> Doc a
    mkLink s = case BS.unsnoc s of
      Just (xs, x) | x `elem` ",.!?" -> DocHyperlink (Hyperlink (decodeUtf8 xs) Nothing) <> DocString [x]
      _ -> DocHyperlink (Hyperlink (decodeUtf8 s) Nothing)

-- | Parses strings between identifier delimiters. Consumes all input that it
-- deems to be valid in an identifier. Note that it simply blindly consumes
-- characters and does no actual validation itself.
parseValid :: Parser String
parseValid = do
  vs <- many' $ satisfy (`elem` "_.!#$%&*+/<=>?@\\|-~:") <|> digit <|> letter_ascii
  c <- peekChar
  case c of
    Just '`' -> return vs
    Just '\'' -> (\x -> vs ++ "'" ++ x) <$> ("'" *> parseValid)
                 <|> return vs
    _ -> fail "outofvalid"

-- | Parses identifiers with help of 'parseValid'. Asks GHC for 'RdrName' from the
-- string it deems valid.
identifier :: DynFlags -> Parser (Doc RdrName)
identifier dflags = do
  o <- idDelim
  vid <- parseValid
  e <- idDelim
  return $ validIdentifier o vid e
  where
    idDelim = char '\'' <|> char '`'
    validIdentifier o ident e = case parseIdent ident of
      Just identName -> DocIdentifier identName
      Nothing -> DocString $ o : ident ++ [e]

    parseIdent :: String -> Maybe RdrName
    parseIdent str0 =
      let buffer = stringToStringBuffer str0
          realSrcLc = mkRealSrcLoc (mkFastString "<unknown file>") 0 0
          pstate = mkPState dflags buffer realSrcLc
      in case unP parseIdentifier pstate of
        POk _ name -> Just (unLoc name)
        _ -> Nothing

-- | Remove all leading and trailing whitespace
strip :: String -> String
strip = (\f -> f . f) $ dropWhile isSpace . reverse

skipHorizontalSpace :: Parser ()
skipHorizontalSpace = skipWhile (`elem` " \t\f\v\r")

takeHorizontalSpace :: Parser BS.ByteString
takeHorizontalSpace = takeWhile (`elem` " \t\f\v\r")

makeLabeled :: (String -> Maybe String -> a) -> String -> a
makeLabeled f input = case break isSpace $ removeEscapes $ strip input of
  (uri, "")    -> f uri Nothing
  (uri, label) -> f uri (Just $ dropWhile isSpace label)
  where
    -- As we don't parse these any further, we don't do any processing to the
    -- string so we at least remove escape character here. Perhaps we should
    -- actually be parsing the label at the very least?
    removeEscapes "" = ""
    removeEscapes ('\\':'\\':xs) = '\\' : removeEscapes xs
    removeEscapes ('\\':xs) = removeEscapes xs
    removeEscapes (x:xs) = x : removeEscapes xs
