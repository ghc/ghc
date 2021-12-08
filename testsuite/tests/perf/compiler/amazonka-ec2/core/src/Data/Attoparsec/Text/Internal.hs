{-# LANGUAGE BangPatterns, FlexibleInstances, GADTs, OverloadedStrings,
    Rank2Types, RecordWildCards, TypeFamilies, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      :  Data.Attoparsec.Text.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'Text' strings, loosely
-- based on the Parsec library.

module Data.Attoparsec.Text.Internal (Parser, takeWhile1, char, satisfy, takeWhile, parseOnly, anyChar, takeText) where

import Control.Monad (when)
import Data.Attoparsec.Internal
import Data.Attoparsec.Internal.Types hiding (Parser, Failure, Success)
import qualified Data.Attoparsec.Text.Buffer as Buf
import Data.Attoparsec.Text.Buffer (Buffer, buffer)
import Data.List (intercalate)
import Data.String (IsString(..))
import Data.Text.Internal (Text(..))
import Prelude hiding (getChar, succ, take, takeWhile)
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.Text as T
import qualified Data.Text.Unsafe as T

type Parser = T.Parser Text
type Result = IResult Text
type Failure r = T.Failure Text Buffer r
type Success a r = T.Success Text Buffer a r

instance (a ~ Text) => IsString (Parser a) where
    fromString = string . T.pack

-- | The parser @satisfy p@ succeeds for any character for which the
-- predicate @p@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit c = c >= '0' && c <= '9'
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  (k,c) <- ensure 1
  let !h = T.unsafeHead c
  if p h
    then advance k >> return h
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | @string s@ parses a sequence of characters that identically match
-- @s@. Returns the parsed string (i.e. @s@).  This parser consumes no
-- input if it fails (even if a partial match).
--
-- /Note/: The behaviour of this parser is different to that of the
-- similarly-named parser in Parsec, as this one is all-or-nothing.
-- To illustrate the difference, the following parser will fail under
-- Parsec given an input of @\"for\"@:
--
-- >string "foo" <|> string "for"
--
-- The reason for its failure is that the first branch is a
-- partial match, and will consume the letters @\'f\'@ and @\'o\'@
-- before failing.  In attoparsec, the above parser will /succeed/ on
-- that input, because the failed first branch will consume nothing.
string :: Text -> Parser Text
string s = string_ (stringSuspended id) id s
{-# INLINE string #-}

string_ :: (forall r. Text -> Text -> Buffer -> Pos -> More
            -> Failure r -> Success Text r -> Result r)
        -> (Text -> Text)
        -> Text -> Parser Text
string_ suspended f s0 = T.Parser $ \t pos more lose succ ->
  let s  = f s0
      ft = f (Buf.unbufferAt (fromPos pos) t)
  in case T.commonPrefixes s ft of
       Nothing
         | T.null s          -> succ t pos more T.empty
         | T.null ft         -> suspended s s t pos more lose succ
         | otherwise         -> lose t pos more [] "string"
       Just (pfx,ssfx,tsfx)
         | T.null ssfx       -> let l = Pos (Buf.lengthCodeUnits pfx)
                                in succ t (pos + l) more (substring pos l t)
         | not (T.null tsfx) -> lose t pos more [] "string"
         | otherwise         -> suspended s ssfx t pos more lose succ
{-# INLINE string_ #-}

stringSuspended :: (Text -> Text)
                -> Text -> Text -> Buffer -> Pos -> More
                -> Failure r
                -> Success Text r
                -> Result r
stringSuspended f s000 s0 t0 pos0 more0 lose0 succ0 =
    runParser (demandInput_ >>= go) t0 pos0 more0 lose0 succ0
  where
    go s' = T.Parser $ \t pos more lose succ ->
      let s = f s'
      in case T.commonPrefixes s0 s of
        Nothing         -> lose t pos more [] "string"
        Just (_pfx,ssfx,tsfx)
          | T.null ssfx -> let l = Pos (Buf.lengthCodeUnits s000)
                           in succ t (pos + l) more (substring pos l t)
          | T.null tsfx -> stringSuspended f s000 ssfx t pos more lose succ
          | otherwise   -> lose t pos more [] "string"

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first character of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
takeWhile :: (Char -> Bool) -> Parser Text
takeWhile p = do
    h <- T.takeWhile p <$> get
    continue <- inputSpansChunks (size h)
    -- only use slow concat path if necessary
    if continue
      then takeWhileAcc p [h]
      else return h
{-# INLINE takeWhile #-}

takeWhileAcc :: (Char -> Bool) -> [Text] -> Parser Text
takeWhileAcc p = go
 where
  go acc = do
    h <- T.takeWhile p <$> get
    continue <- inputSpansChunks (size h)
    if continue
      then go (h:acc)
      else return $ concatReverse (h:acc)
{-# INLINE takeWhileAcc #-}

takeRest :: Parser [Text]
takeRest = go []
 where
  go acc = do
    input <- wantInput
    if input
      then do
        s <- get
        advance (size s)
        go (s:acc)
      else return (reverse acc)

-- | Consume all remaining input and return it as a single string.
takeText :: Parser Text
takeText = T.concat `fmap` takeRest

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one
-- character of input: it will fail if the predicate never returns
-- 'True' or if there is no input left.
takeWhile1 :: (Char -> Bool) -> Parser Text
takeWhile1 p = do
  (`when` demandInput) =<< endOfChunk
  h <- T.takeWhile p <$> get
  let size' = size h
  when (size' == 0) $ fail "takeWhile1"
  advance size'
  eoc <- endOfChunk
  if eoc
    then takeWhileAcc p [h]
    else return h
{-# INLINE takeWhile1 #-}

-- | Match any character.
anyChar :: Parser Char
anyChar = satisfy $ const True
{-# INLINE anyChar #-}

-- | Match a specific character.
char :: Char -> Parser Char
char c = satisfy (== c) <?> show c
{-# INLINE char #-}

-- | Terminal failure continuation.
failK :: Failure a
failK t (Pos pos) _more stack msg = Fail (Buf.dropCodeUnits pos t) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK t (Pos pos) _more a = Done (Buf.dropCodeUnits pos t) a
{-# INLINE successK #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
--
-- This function does not force a parser to consume all of its input.
-- Instead, any residual input will be discarded.  To force a parser
-- to consume all of its input, use something like this:
--
-- @
--'parseOnly' (myParser 'Control.Applicative.<*' 'endOfInput')
-- @
parseOnly :: Parser a -> Text -> Either String a
parseOnly m s = case runParser m (buffer s) 0 Complete failK successK of
                  Fail _ [] err   -> Left err
                  Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
                  Done _ a        -> Right a
                  _               -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

get :: Parser Text
get = T.Parser $ \t pos more _lose succ ->
  succ t pos more (Buf.dropCodeUnits (fromPos pos) t)
{-# INLINE get #-}

endOfChunk :: Parser Bool
endOfChunk = T.Parser $ \t pos more _lose succ ->
  succ t pos more (pos == lengthOf t)
{-# INLINE endOfChunk #-}

inputSpansChunks :: Pos -> Parser Bool
inputSpansChunks i = T.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + i
  in if pos < lengthOf t || more == Complete
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}

advance :: Pos -> Parser ()
advance n = T.Parser $ \t pos more _lose succ -> succ t (pos+n) more ()
{-# INLINE advance #-}

ensureSuspended :: Int -> Buffer -> Pos -> More
                -> Failure r -> Success (Pos, Text) r
                -> Result r
ensureSuspended n t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = T.Parser $ \t' pos' more' lose' succ' ->
          case lengthAtLeast pos' n t' of
            Just n' -> succ' t' pos' more' (n', substring pos n' t')
            Nothing -> runParser (demandInput >> go) t' pos' more' lose' succ'

-- | If at least @n@ elements of input are available, return the
-- current input, otherwise fail.
ensure :: Int -> Parser (Pos, Text)
ensure n = T.Parser $ \t pos more lose succ ->
    case lengthAtLeast pos n t of
      Just n' -> succ t pos more (n', substring pos n' t)
      -- The uncommon case is kept out-of-line to reduce code size:
      Nothing -> ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}

-- | Ensure that at least @n@ code points of input are available.
-- Returns the number of words consumed while traversing.
lengthAtLeast :: Pos -> Int -> Buffer -> Maybe Pos
lengthAtLeast pos n t = go 0 (fromPos pos)
  where go i !p
          | i == n    = Just (Pos p - pos)
          | p == len  = Nothing
          | otherwise = go (i+1) (p + Buf.iter_ t p)
        Pos len = lengthOf t
{-# INLINE lengthAtLeast #-}

substring :: Pos -> Pos -> Buffer -> Text
substring (Pos pos) (Pos n) = Buf.substring pos n
{-# INLINE substring #-}

lengthOf :: Buffer -> Pos
lengthOf = Pos . Buf.length

size :: Text -> Pos
size (Text _ _ l) = Pos l

-- | Name the parser, in case failure occurs.
(<?>) :: T.Parser i a
      -> String                 -- ^ the name to use if parsing fails
      -> T.Parser i a
p <?> msg0 = T.Parser $ \t pos more lose succ ->
             let lose' t' pos' more' strs msg = lose t' pos' more' (msg0:strs) msg
             in T.runParser p t pos more lose' succ
{-# INLINE (<?>) #-}
infix 0 <?>
