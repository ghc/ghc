{-# LANGUAGE BangPatterns, CPP, GADTs, OverloadedStrings, RankNTypes,
    RecordWildCards #-}
-- |
-- Module      :  Data.Attoparsec.ByteString.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.ByteString.Internal
    (
    -- * Parser types
      Parser
    , Result

    -- * Running parsers
    , parse
    , parseOnly

    -- * Combinators
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , satisfy
    , satisfyWith
    , anyWord8
    , skip
    , word8
    , notWord8

    -- ** Lookahead
    , peekWord8
    , peekWord8'

    -- ** Byte classes
    , inClass
    , notInClass

    -- * Parsing more complicated structures
    , storable

    -- * Efficient string handling
    , skipWhile
    , string
    , stringCI
    , take
    , scan
    , runScanner
    , takeWhile
    , takeWhile1
    , takeWhileIncluding
    , takeTill
    , getChunk

    -- ** Consume all remaining input
    , takeByteString
    , takeLazyByteString

    -- * Utilities
    , endOfLine
    , endOfInput
    , match
    , atEnd
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Attoparsec.ByteString.Buffer (Buffer, buffer)
import Data.Attoparsec.ByteString.FastSet (charClass, memberWord8)
import Data.Attoparsec.Combinator ((<?>))
import Data.Attoparsec.Internal
import Data.Attoparsec.Internal.Compat
import Data.Attoparsec.Internal.Fhthagn (inlinePerformIO)
import Data.Attoparsec.Internal.Types hiding (Parser, Failure, Success)
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Prelude hiding (getChar, succ, take, takeWhile)
import qualified Data.Attoparsec.ByteString.Buffer as Buf
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B

type Parser = T.Parser ByteString
type Result = IResult ByteString
type Failure r = T.Failure ByteString Buffer r
type Success a r = T.Success ByteString Buffer a r

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  h <- peekWord8'
  if p h
    then advance 1 >> return h
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any byte for which the predicate
-- @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit w = w >= 48 && w <= 57
skip :: (Word8 -> Bool) -> Parser ()
skip p = do
  h <- peekWord8'
  if p h
    then advance 1
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  h <- peekWord8'
  let c = f h
  if p c
    then advance 1 >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

storable :: Storable a => Parser a
storable = hack undefined
 where
  hack :: Storable b => b -> Parser b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmap` take (sizeOf dummy)
    return . inlinePerformIO . withForeignPtr fp $ \p ->
        peek (castPtr $ p `plusPtr` o)

-- | Consume exactly @n@ bytes of input.
take :: Int -> Parser ByteString
take n0 = do
  let n = max n0 0
  s <- ensure n
  advance n >> return s
{-# INLINE take #-}

-- | @string s@ parses a sequence of bytes that identically match
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
string :: ByteString -> Parser ByteString
string s = string_ (stringSuspended id) id s
{-# INLINE string #-}

-- ASCII-specific but fast, oh yes.
toLower :: Word8 -> Word8
toLower w | w >= 65 && w <= 90 = w + 32
          | otherwise          = w

-- | Satisfy a literal string, ignoring case.
stringCI :: ByteString -> Parser ByteString
stringCI s = string_ (stringSuspended lower) lower s
  where lower = B8.map toLower
{-# INLINE stringCI #-}

string_ :: (forall r. ByteString -> ByteString -> Buffer -> Pos -> More
            -> Failure r -> Success ByteString r -> Result r)
        -> (ByteString -> ByteString)
        -> ByteString -> Parser ByteString
string_ suspended f s0 = T.Parser $ \t pos more lose succ ->
  let n = B.length s
      s = f s0
  in if lengthAtLeast pos n t
     then let t' = substring pos (Pos n) t
          in if s == f t'
             then succ t (pos + Pos n) more t'
             else lose t pos more [] "string"
     else let t' = Buf.unsafeDrop (fromPos pos) t
          in if f t' `B.isPrefixOf` s
             then suspended s (B.drop (B.length t') s) t pos more lose succ
             else lose t pos more [] "string"
{-# INLINE string_ #-}

stringSuspended :: (ByteString -> ByteString)
                -> ByteString -> ByteString -> Buffer -> Pos -> More
                -> Failure r
                -> Success ByteString r
                -> Result r
stringSuspended f s0 s t pos more lose succ =
    runParser (demandInput_ >>= go) t pos more lose succ
  where go s'0   = T.Parser $ \t' pos' more' lose' succ' ->
          let m  = B.length s
              s' = f s'0
              n  = B.length s'
          in if n >= m
             then if B.unsafeTake m s' == s
                  then let o = Pos (B.length s0)
                       in succ' t' (pos' + o) more'
                          (substring pos' o t')
                  else lose' t' pos' more' [] "string"
             else if s' == B.unsafeTake n s
                  then stringSuspended f s0 (B.unsafeDrop n s)
                       t' pos' more' lose' succ'
                  else lose' t' pos' more' [] "string"

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = go
 where
  go = do
    t <- B8.takeWhile p <$> get
    continue <- inputSpansChunks (B.length t)
    when continue go
{-# INLINE skipWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
takeTill :: (Word8 -> Bool) -> Parser ByteString
takeTill p = takeWhile (not . p)
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
takeWhile :: (Word8 -> Bool) -> Parser ByteString
takeWhile p = do
    s <- B8.takeWhile p <$> get
    continue <- inputSpansChunks (B.length s)
    if continue
      then takeWhileAcc p [s]
      else return s
{-# INLINE takeWhile #-}

takeWhileAcc :: (Word8 -> Bool) -> [ByteString] -> Parser ByteString
takeWhileAcc p = go
 where
  go acc = do
    s <- B8.takeWhile p <$> get
    continue <- inputSpansChunks (B.length s)
    if continue
      then go (s:acc)
      else return $ concatReverse (s:acc)
{-# INLINE takeWhileAcc #-}

-- | Consume input until immediately after the predicate returns 'True', and return
-- the consumed input.
--
-- This parser will consume at least one 'Word8' or fail.
takeWhileIncluding :: (Word8 -> Bool) -> Parser B.ByteString
takeWhileIncluding p = do
  (s', t) <- B8.span p <$> get
  case B8.uncons t of
    -- Since we reached a break point and managed to get the next byte,
    -- input can not have been exhausted thus we succed and advance unconditionally.
    Just (h, _) -> do
      let s = s' `B8.snoc` h
      advance (B8.length s)
      return s
    -- The above isn't true so either we ran out of input or we need to process the next chunk.
    Nothing -> do
      continue <- inputSpansChunks (B8.length s')
      if continue
        then takeWhileIncAcc p [s']
        -- Our spec says that if we run out of input we fail.
        else fail "takeWhileIncluding reached end of input"
{-# INLINE takeWhileIncluding #-}

takeWhileIncAcc :: (Word8 -> Bool) -> [B.ByteString] -> Parser B.ByteString
takeWhileIncAcc p = go
 where
   go acc = do
     (s', t) <- B8.span p <$> get
     case B8.uncons t of
       Just (h, _) -> do
         let s = s' `B8.snoc` h
         advance (B8.length s)
         return (concatReverse $ s:acc)
       Nothing -> do
         continue <- inputSpansChunks (B8.length s')
         if continue
           then go (s':acc)
           else fail "takeWhileIncAcc reached end of input"
{-# INLINE takeWhileIncAcc #-}

takeRest :: Parser [ByteString]
takeRest = go []
 where
  go acc = do
    input <- wantInput
    if input
      then do
        s <- get
        advance (B.length s)
        go (s:acc)
      else return (reverse acc)

-- | Consume all remaining input and return it as a single string.
takeByteString :: Parser ByteString
takeByteString = B.concat `fmap` takeRest

-- | Consume all remaining input and return it as a single string.
takeLazyByteString :: Parser L.ByteString
takeLazyByteString = L.fromChunks `fmap` takeRest

-- | Return the rest of the current chunk without consuming anything.
--
-- If the current chunk is empty, then ask for more input.
-- If there is no more input, then return 'Nothing'
getChunk :: Parser (Maybe ByteString)
getChunk = do
  input <- wantInput
  if input
    then Just <$> get
    else return Nothing

data T s = T {-# UNPACK #-} !Int s

scan_ :: (s -> [ByteString] -> Parser r) -> s -> (s -> Word8 -> Maybe s)
         -> Parser r
scan_ f s0 p = go [] s0
 where
  go acc s1 = do
    let scanner bs = withPS bs $ \fp off len ->
          withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
                inner ptr !s
                  | ptr < end = do
                    w <- peek ptr
                    case p s w of
                      Just s' -> inner (ptr `plusPtr` 1) s'
                      _       -> done (ptr `minusPtr` start) s
                  | otherwise = done (ptr `minusPtr` start) s
                done !i !s = return (T i s)
            inner start s1
    bs <- get
    let T i s' = inlinePerformIO $ scanner bs
        !h = B.unsafeTake i bs
    continue <- inputSpansChunks i
    if continue
      then go (h:acc) s'
      else f s' (h:acc)
{-# INLINE scan_ #-}

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
scan :: s -> (s -> Word8 -> Maybe s) -> Parser ByteString
scan = scan_ $ \_ chunks -> return $! concatReverse chunks
{-# INLINE scan #-}

-- | Like 'scan', but generalized to return the final state of the
-- scanner.
runScanner :: s -> (s -> Word8 -> Maybe s) -> Parser (ByteString, s)
runScanner = scan_ $ \s xs -> let !sx = concatReverse xs in return (sx, s)
{-# INLINE runScanner #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: (Word8 -> Bool) -> Parser ByteString
takeWhile1 p = do
  (`when` demandInput) =<< endOfChunk
  s <- B8.takeWhile p <$> get
  let len = B.length s
  if len == 0
    then fail "takeWhile1"
    else do
      advance len
      eoc <- endOfChunk
      if eoc
        then takeWhileAcc p [s]
        else return s
{-# INLINE takeWhile1 #-}

-- | Match any byte in a set.
--
-- >vowel = inClass "aeiou"
--
-- Range notation is supported.
--
-- >halfAlphabet = inClass "a-nA-N"
--
-- To add a literal @\'-\'@ to a set, place it at the beginning or end
-- of the string.
inClass :: String -> Word8 -> Bool
inClass s = (`memberWord8` mySet)
    where mySet = charClass s
          {-# NOINLINE mySet #-}
{-# INLINE inClass #-}

-- | Match any byte not in a set.
notInClass :: String -> Word8 -> Bool
notInClass s = not . inClass s
{-# INLINE notInClass #-}

-- | Match any byte.
anyWord8 :: Parser Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Match a specific byte.
word8 :: Word8 -> Parser Word8
word8 c = satisfy (== c) <?> show c
{-# INLINE word8 #-}

-- | Match any byte except the given one.
notWord8 :: Word8 -> Parser Word8
notWord8 c = satisfy (/= c) <?> "not " ++ show c
{-# INLINE notWord8 #-}

-- | Match any byte, to perform lookahead. Returns 'Nothing' if end of
-- input has been reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'Control.Applicative.many', because such
-- parsers loop until a failure occurs.  Careless use will thus result
-- in an infinite loop.
peekWord8 :: Parser (Maybe Word8)
peekWord8 = T.Parser $ \t pos@(Pos pos_) more _lose succ ->
  case () of
    _| pos_ < Buf.length t ->
       let !w = Buf.unsafeIndex t pos_
       in succ t pos more (Just w)
     | more == Complete ->
       succ t pos more Nothing
     | otherwise ->
       let succ' t' pos' more' = let !w = Buf.unsafeIndex t' pos_
                                 in succ t' pos' more' (Just w)
           lose' t' pos' more' = succ t' pos' more' Nothing
       in prompt t pos more lose' succ'
{-# INLINE peekWord8 #-}

-- | Match any byte, to perform lookahead.  Does not consume any
-- input, but will fail if end of input has been reached.
peekWord8' :: Parser Word8
peekWord8' = T.Parser $ \t pos more lose succ ->
    if lengthAtLeast pos 1 t
    then succ t pos more (Buf.unsafeIndex t (fromPos pos))
    else let succ' t' pos' more' bs' = succ t' pos' more' $! B.unsafeHead bs'
         in ensureSuspended 1 t pos more lose succ'
{-# INLINE peekWord8' #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

-- | Terminal failure continuation.
failK :: Failure a
failK t (Pos pos) _more stack msg = Fail (Buf.unsafeDrop pos t) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK t (Pos pos) _more a = Done (Buf.unsafeDrop pos t) a
{-# INLINE successK #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Result a
parse m s = T.runParser m (buffer s) (Pos 0) Incomplete failK successK
{-# INLINE parse #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
--
-- This function does not force a parser to consume all of its input.
-- Instead, any residual input will be discarded.  To force a parser
-- to consume all of its input, use something like this:
--
-- @
--'parseOnly' (myParser 'Control.Applicative.<*' 'endOfInput')
-- @
parseOnly :: Parser a -> ByteString -> Either String a
parseOnly m s = case T.runParser m (buffer s) (Pos 0) Complete failK successK of
                  Fail _ [] err   -> Left err
                  Fail _ ctxs err -> Left (intercalate " > " ctxs ++ ": " ++ err)
                  Done _ a        -> Right a
                  _               -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

get :: Parser ByteString
get = T.Parser $ \t pos more _lose succ ->
  succ t pos more (Buf.unsafeDrop (fromPos pos) t)
{-# INLINE get #-}

endOfChunk :: Parser Bool
endOfChunk = T.Parser $ \t pos more _lose succ ->
  succ t pos more (fromPos pos == Buf.length t)
{-# INLINE endOfChunk #-}

inputSpansChunks :: Int -> Parser Bool
inputSpansChunks i = T.Parser $ \t pos_ more _lose succ ->
  let pos = pos_ + Pos i
  in if fromPos pos < Buf.length t || more == Complete
     then succ t pos more False
     else let lose' t' pos' more' = succ t' pos' more' False
              succ' t' pos' more' = succ t' pos' more' True
          in prompt t pos more lose' succ'
{-# INLINE inputSpansChunks #-}

advance :: Int -> Parser ()
advance n = T.Parser $ \t pos more _lose succ ->
  succ t (pos + Pos n) more ()
{-# INLINE advance #-}

ensureSuspended :: Int -> Buffer -> Pos -> More
                -> Failure r
                -> Success ByteString r
                -> Result r
ensureSuspended n t pos more lose succ =
    runParser (demandInput >> go) t pos more lose succ
  where go = T.Parser $ \t' pos' more' lose' succ' ->
          if lengthAtLeast pos' n t'
          then succ' t' pos' more' (substring pos (Pos n) t')
          else runParser (demandInput >> go) t' pos' more' lose' succ'

-- | If at least @n@ elements of input are available, return the
-- current input, otherwise fail.
ensure :: Int -> Parser ByteString
ensure n = T.Parser $ \t pos more lose succ ->
    if lengthAtLeast pos n t
    then succ t pos more (substring pos (Pos n) t)
    -- The uncommon case is kept out-of-line to reduce code size:
    else ensureSuspended n t pos more lose succ
{-# INLINE ensure #-}

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: Parser a -> Parser (ByteString, a)
match p = T.Parser $ \t pos more lose succ ->
  let succ' t' pos' more' a =
        succ t' pos' more' (substring pos (pos'-pos) t', a)
  in runParser p t pos more lose succ'

lengthAtLeast :: Pos -> Int -> Buffer -> Bool
lengthAtLeast (Pos pos) n bs = Buf.length bs >= pos + n
{-# INLINE lengthAtLeast #-}

substring :: Pos -> Pos -> Buffer -> ByteString
substring (Pos pos) (Pos n) = Buf.substring pos n
{-# INLINE substring #-}
