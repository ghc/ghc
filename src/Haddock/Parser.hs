{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      :  Haddock.Parser
-- Copyright   :  (c) Mateusz Kowalczyk 2013,
--                    Simon Hengel      2013
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable

module Haddock.Parser (parseString, parseParas) where

import           Control.Applicative
import           Data.Attoparsec.ByteString   hiding (takeWhile1, take, inClass)
import qualified Data.Attoparsec.ByteString.Char8 as A8
import           Data.Attoparsec.ByteString.Char8 hiding (take, string)
import qualified Data.ByteString as BS
import           Data.Char (chr)
import           Data.List (stripPrefix)
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

default (Int)

-- | Main entry point to the parser. Appends the newline character
-- to the input string.
parseParas :: DynFlags
              -> String -- ^ String to parse
              -> Maybe (Doc RdrName)
parseParas d s = case parseOnly (p <* skipSpace) (encodeUtf8 $ s ++ "\n") of
  Right r -> Just $ combineStringNodes r
  _ -> Nothing
  where
    p :: Parser (Doc RdrName)
    -- make sure that we don't swallow up whitespace belonging to next paragraph
    p = mconcat <$> paragraph d `sepBy` some (optWs *> "\n")

-- | A parser that parsers separate lines of the comments. Eventually
-- called by 'parseParas'. Appends a newline character to the input string.
-- Drops any whitespace in front of the input string. It's dropped for the sake of
-- section headings.
parseString :: DynFlags -> String -> Maybe (Doc RdrName)
parseString d = parseString'' d . dropWhile isSpace

-- | A parser that parsers separate lines of the comments. Eventually
-- called by 'parseParas'. Appends a newline character to the input string.
-- Unlike 'parseString', doesn't drop the preceding whitespace. Internal use.
parseString'' :: DynFlags -> String -> Maybe (Doc RdrName)
parseString'' d = parseString' d . (++ "\n")

-- | An internal use function. Split from the 'parseString' is useful
-- as we can specify separately when we want the newline to be appended.
parseString' :: DynFlags -> String -> Maybe (Doc RdrName)
parseString' d s = case parseOnly p (encodeUtf8 s) of
  Right r -> Just $ combineStringNodes r
  _ -> Nothing
  where
    p :: Parser (Doc RdrName)
    p = mconcat <$> some (charEscape <|> monospace d <|> anchor <|> identifier d
                          <|> moduleName <|> picture <|> url
                          <|> emphasis d <|> encodedChar <|> string' <|> skipChar)

-- | Parses and processes
-- <https://en.wikipedia.org/wiki/Numeric_character_reference Numeric character references>
--
-- >>> parseOnly encodedChar "&#65;&#66;&#67;"
-- Right (DocString "ABC")
encodedChar :: Parser (Doc RdrName)
encodedChar = "&#" *> c <* ";"
  where
    c = DocString . return . chr <$> num
    num = hex <|> decimal
    hex = ("x" <|> "X") *> hexadecimal

-- | Plain, regular parser for text. Called as one of the last parsers
-- to ensure that we have already given a chance to more meaningful parsers
-- before capturing their characers.
string' :: Parser (Doc RdrName)
string' = DocString . decodeUtf8 <$> takeWhile1 (`notElem` "/<@\" &'`\\")

-- | Emphasis parser.
--
-- >>> parseOnly emphasis "/Hello world/"
-- Right (DocEmphasis (DocString "Hello world"))
emphasis :: DynFlags -> Parser (Doc RdrName)
emphasis d = stringBlock d id DocEmphasis "/" "/" "\n"

-- | Skips a single character and treats it as a plain string.
-- This is done to skip over any special characters belonging to other
-- elements but which were not deemed meaningful at their positions.
-- Note that this can only be used in places where we're absolutely certain
-- no unicode is present, such as to skip a 100% certain ASCII delimeter.
skipChar :: Parser (Doc RdrName)
skipChar = DocString . return <$> anyChar

-- | Treats the next character as a regular string, even if it's normally
-- used for markup.
charEscape :: Parser (Doc RdrName)
charEscape = "\\" *> (DocString . return <$> A8.satisfy (/= '\n'))

-- | Text anchors to allow for jumping around the generated documentation.
--
-- >>> parseOnly anchor "#Hello world#"
-- Right (DocAName "Hello world")
anchor :: Parser (Doc RdrName)
anchor = DocAName . decodeUtf8 <$> ("#" *> takeWhile1 (`notElem` "#\n") <* "#")

-- | Helper for markup structures surrounded with delimiters.
stringBlock
  :: DynFlags
     -> (String -> String) -- ^ Function used to transform parsed out text
                           -- before we send it to 'parseString''
     -> (Doc RdrName -> Doc RdrName) -- ^ 'Doc' to wrap around the result
     -> String -- ^ Opening delimiter
     -> String -- ^ Closing delimiter
     -> String -- ^ Additional characters to terminate parsing on
     -> Parser (Doc RdrName)
stringBlock d f doc op ed n = do
  inner <- block op ed n
  case parseString' d (f inner) of
    Just r -> return $ doc r
    _ -> fail $ "inner parse fail with op: ‘" ++ op ++ "’, ed: ‘" ++ ed ++ "’"

-- | Returns sections of text delimited by specified text.
block :: String -> String -> String -> Parser String
block op ed n = reverse . drop (length ed) . reverse <$> block' op ed
  where
    block' op' ed' = string (encodeUtf8 op') *> mid
      where
        mid :: Parser String
        mid = decodeUtf8 <$> string (encodeUtf8 ed')
              <|> do
                inner <- takeWithSkip (head ed') n
                more <- decodeUtf8 <$> string (encodeUtf8 $ tail ed')
                        <|> block' "" ed'  -- not full ending, take more
                return $ inner ++ more


-- | Takes all characters until the specified one. Unconditionally
-- takes a character if it's escaped. Fails if it doesn't find the character or
-- when the input string is empty.
takeWithSkip :: Char -> String -> Parser String
takeWithSkip s n = do
  content <- decodeUtf8 <$> A8.scan (False, False) p >>= gotSome
  if or (map (`elem` content) n) || last content /= s
    then fail "failed in takeWithSkip"
    else return content
  where
    gotSome [] = fail "EOF in takeWithSkip"
    gotSome xs = return xs
    -- Apparently ‘scan’ is so magical that it doesn't mangle unicode.
    p (escaped, terminate) c
      | terminate = Nothing -- swallows up that extra character
      | escaped = Just (False, False)
      | c == s = Just (False, True)
      | otherwise = Just (c == '\\', False)

-- | Monospaced strings.
--
-- >>> parseOnly (monospace dynflags) "@cruel@"
-- Right (DocMonospaced (DocString "cruel"))
monospace :: DynFlags -> Parser (Doc RdrName)
monospace d = stringBlock d id DocMonospaced "@" "@" ""

-- | Module name parser, surrounded by double quotes. This does a very primitive and
-- purely syntactic checking so that obviously invalid names are not treated as valid
-- and blindly hyperlinked (not starting with a capital letter or including spaces).
moduleName :: Parser (Doc RdrName)
moduleName = DocModule <$> ("\"" *> legalModule <* "\"")
  where legalModule = do
          n <- (:) <$> A8.satisfy (`elem` ['A' .. 'Z'])
               <*> (decodeUtf8 <$> A8.takeWhile (`notElem` "\"\n"))

          if any (`elem` n) " &[{}(=*)+]!#|@/;,^?"
            then fail "invalid characters in module name"
            else case n of
            [] -> return []
            _ -> if last n == '.' then fail "trailing dot in module name" else return n


-- | Picture parser, surrounded by \<\< and \>\>. It's possible to specify
-- a title for the picture.
--
-- >>> parseOnly picture "<<hello.png>>"
-- Right (DocPic (Picture "hello.png" Nothing))
-- >>> parseOnly picture "<<hello.png world>>"
-- Right (DocPic (Picture "hello.png" (Just "world")))
picture :: Parser (Doc RdrName)
picture = DocPic . makePicture . decodeUtf8 <$> ("<<" *> takeWhile1 (`notElem` ">\n") <* ">>")

-- | Paragraph parser, called by 'parseParas'.
paragraph :: DynFlags -> Parser (Doc RdrName)
paragraph d = examples <|> skipSpace *> (list d <|> birdtracks <|> codeblock d
                                         <|> property <|> textParagraph d)

-- | List parser, called by 'paragraph'.
list :: DynFlags -> Parser (Doc RdrName)
list d = DocUnorderedList <$> unorderedList d
         <|> DocOrderedList <$> orderedList d
         <|> DocDefList <$> definitionList d

-- | Parse given text with a provided parser, casting
-- Nothing to a failure
parseLine :: (String -> Maybe (Doc RdrName)) -- ^ Parser to use
             -> (Doc RdrName -> a) -- ^ Doc function to wrap around the result
             -> BS.ByteString -- ^ Text to parse
             -> Parser a
parseLine f doc str = maybe (fail "invalid string") (return . doc) (f $ decodeUtf8 str)

-- | Parses unordered (bullet) lists.
unorderedList :: DynFlags -> Parser [Doc RdrName]
unorderedList d = ("*" <|> "-") *> innerList unorderedList d

-- | Parses ordered lists (numbered or dashed).
orderedList :: DynFlags -> Parser [Doc RdrName]
orderedList d = skipSpace *> (paren <|> dot) *> innerList orderedList d
  where
    dot = decimal <* "."
    paren = "(" *> (decimal :: Parser Int) <* ")"

-- | Generic function collecting any further lines belonging to the
-- list entry and recursively collecting any further lists in the
-- same paragraph. Usually used as
--
-- > someListFunction dynflags = listBeginning *> innerList someListFunction dynflags
innerList :: (DynFlags -> Parser [Doc RdrName]) -- ^ parser calling this function
             -> DynFlags
             -> Parser [Doc RdrName]
innerList p d = do
  cl <- do
    content <- A8.takeWhile (/= '\n') <* "\n" -- allow empty
    parseLine (parseString'' d) id content
  ulcs <- many ulc
  let contents = docParagraph $ mconcat $ cl : [x | Right x <- ulcs]
      unLists = mconcat [x | Left x <- ulcs]
  return $ contents : unLists
  where
    ulc :: Parser (Either [Doc RdrName] (Doc RdrName))
    ulc = Left <$> (optWs *> p d)
          <|> Right <$> nonEmptyLine d

-- | Takes the remained of the line until the newline character
-- and calls 'parseLine' using 'parseString'. Fails if it's made
-- up strictly of whitespace.
nonEmptyLine :: DynFlags -> Parser (Doc RdrName)
nonEmptyLine d = do
  s <- (takeWhile1 (/= '\n') >>= nonSpace) <* "\n"
  parseLine (parseString'' d) id s
  where
    nonSpace xs
      | not (any (not . isSpace) (decodeUtf8 xs)) = fail "empty line"
      | otherwise = return xs

-- | Parses definition lists.
definitionList :: DynFlags -> Parser [(Doc RdrName, Doc RdrName)]
definitionList d = do
  _ <- "["
  inner <- parseLine (parseString' d) id =<< takeWhile1 (`notElem` "]\n")
  _ <- "]"
  outer <- parseLine (parseString'' d) id =<< (A8.takeWhile (/= '\n') <* "\n")
  ulcs <- many ulc
  let contents = mconcat $ outer : [x | Right x <- ulcs]
      unLists = map mconcat [x | Left x <- ulcs]
  return $ (inner, contents) : unLists
  where
    ulc :: Parser (Either [(Doc RdrName, Doc RdrName)] (Doc RdrName))
    ulc = Left <$> (optWs *> definitionList d)
          <|> Right <$> nonEmptyLine d

-- | Parses birdtracks. No further markup is parsed after the birdtrack.
-- Consecutive birdtracks are allowed.
birdtracks :: Parser (Doc RdrName)
birdtracks = DocCodeBlock . mconcat . map (DocString . (++ "\n") . decodeUtf8) <$> line `sepBy1` "\n"
  where
    line = optWs *> ">" *> A8.takeWhile (/= '\n')

-- | Parses examples. Examples are a paragraph level entitity (separated by an empty line).
-- Consecutive examples are accepted.
examples :: Parser (Doc RdrName)
examples = DocExamples <$> example

-- | Collects consecutive examples and their results.
example :: Parser [Example]
example = do
  ws <- optWs
  prompt <- decodeUtf8 <$> string ">>>"
  expr <- (++ "\n") . decodeUtf8 <$> (A8.takeWhile (/= '\n') <* "\n")
  results <- many result
  let exs = concat [ e | Left e <- results ]
      res = filter (not . null) [ r | Right r <- results ]
  return $ makeExample (decodeUtf8 ws ++ prompt) expr res : exs
  where
    result = Left <$> example
             <|> Right . decodeUtf8 <$> takeWhile1 (/= '\n') <* "\n"

-- | Propery parser.
--
-- >>> parseOnly property "prop> hello world"
-- Right (DocProperty "hello world")
property :: Parser (Doc RdrName)
property = do
    _ <- skipSpace
    s <- decodeUtf8 <$> (string "prop>" *> takeWhile1 (/= '\n'))
    return $ makeProperty ("prop>" ++ s)

-- | Paragraph level codeblock. Anything between the two delimiting @
-- is parsed for markup.
codeblock :: DynFlags -> Parser (Doc RdrName)
codeblock d = do
  -- Note that we don't need to use optWs here because in cases where
  -- we don't see a \n immediatelly after the opening @, this parser
  -- fails but we still have a chance to get a codeblock by getting
  -- a monospaced doc on its own in the paragraph. With that, the cases
  -- are covered. This should be updated if the implementation ever changes.
  s <- parseString' d . ('\n':) . decodeUtf8 <$> ("@\n" *> block' <* "@")
  maybe (fail "codeblock") (return . DocCodeBlock) s
  where
    block' = A8.scan False p
      where
        p isNewline c
          | isNewline && c == '@' = Nothing
          | otherwise = Just $ c == '\n'

-- | Calls 'parseString'' on each line of a paragraph
textParagraph :: DynFlags -> Parser (Doc RdrName)
textParagraph d = do
  s <- parseString' d . concatMap ((++ "\n") . decodeUtf8) <$> line `sepBy1` "\n"
  maybe (fail "textParagraph") (return . docParagraph) s
  where
    line = takeWhile1 (/= '\n')

-- | See 'picture' for adding a page title.
url :: Parser (Doc RdrName)
url = DocHyperlink . makeHyperlink . decodeUtf8 <$> ("<" *> takeWhile1 (`notElem` ">\n") <* ">")
      <|> autoUrl

-- | Naive implementation of auto-linking. Will link everything after
-- @http://@, @https://@, @ftp://@, @ssh://@, @gopher://@ until a space.
-- Single trailing punctuation character (.!?,) is split off.
autoUrl :: Parser (Doc RdrName)
autoUrl = do
  link <- decodeUtf8 <$> urlLone
  return $ formatLink link
  where
    urlLone = mappend <$> choice prefixes <*> takeWhile1 (not . isSpace)
    prefixes = [ "http://", "https://", "ftp://"
               , "ssh://", "gopher://" ]
    formatLink :: String -> Doc RdrName
    formatLink s = if last s `elem` ".!?,"
                   then docAppend (DocHyperlink $ Hyperlink (init s) Nothing) (DocString [last s])
                   else DocHyperlink $ Hyperlink s Nothing

-- | Parses strings between identifier delimiters. Consumes all input that it
-- deems to be valid in an identifier. Note that it simply blindly consumes
-- characters and does no actual validation itself.
parseValid :: Parser String
parseValid = do
  vs <- many' (A8.satisfy (`elem` "_.!#$%&*+/<=>?@\\?|-~:") <|> digit <|> letter_ascii)
  c <- peekChar
  case c of
    Just '`' -> return vs
    Just '\'' -> (do {c'' <- char '\''; y'' <- parseValid; return $ vs ++ [c''] ++ y''}) <|> return vs
    _ -> fail "outofvalid"

-- | Parses identifiers with help of 'parseValid'. Asks GHC for 'RdrName' from the
-- string it deems valid.
identifier :: DynFlags -> Parser (Doc RdrName)
identifier dflags = do
  o <- idDelim
  vid <- parseValid
  e <- idDelim
  return $ validIdentifier $ o : (vid ++ [e])
  where idDelim = char '\'' <|> char '`'
        validIdentifier str = case parseIdent (tail $ init str) of
          Just identName -> DocIdentifier identName
          Nothing -> DocString str
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

-- | Consumes whitespace, excluding a newline.
optWs :: Parser BS.ByteString
optWs = A8.takeWhile (`elem` " \t\f\v\r")

-- | Create an 'Example', stripping superfluous characters as appropriate.
-- Remembers the amount of indentation used for the prompt.
makeExample :: String -> String -> [String] -> Example
makeExample prompt expression res =
  Example (strip expression) result'       -- drop whitespace in expressions
  where (prefix, _) = span isSpace prompt
        result' = map substituteBlankLine $ filter (not . null) $ map (tryStripPrefix prefix) res
          where tryStripPrefix xs ys = fromMaybe ys $ stripPrefix xs ys
                substituteBlankLine "<BLANKLINE>" = ""
                substituteBlankLine line          = line

-- | Creates a 'Picture' with an optional title. Called by 'picture'.
makePicture :: String -> Picture
makePicture input = case break isSpace $ strip input of
  (uri, "")    -> Picture uri Nothing
  (uri, label) -> Picture uri (Just $ dropWhile isSpace label)

-- | Creates a 'Hyperlink' with an optional title. Called by 'example'.
makeHyperlink :: String -> Hyperlink
makeHyperlink input = case break isSpace $ strip input of
  (u, "")    -> Hyperlink u Nothing
  (u, label) -> Hyperlink u (Just $ dropWhile isSpace label)

-- | Makes a property that can be used by other programs for assertions.
-- Drops whitespace around the property. Called by 'property'
makeProperty :: String -> Doc RdrName
makeProperty s = case strip s of
  'p':'r':'o':'p':'>':xs ->
    DocProperty (dropWhile isSpace xs)
  xs ->
    error $ "makeProperty: invalid input " ++ show xs
