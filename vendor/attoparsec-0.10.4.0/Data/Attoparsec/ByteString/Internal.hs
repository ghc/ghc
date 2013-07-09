{-# LANGUAGE BangPatterns, CPP, Rank2Types, OverloadedStrings,
    RecordWildCards, MagicHash, UnboxedTuples #-}
-- |
-- Module      :  Data.Attoparsec.ByteString.Internal
-- Copyright   :  Bryan O'Sullivan 2007-2011
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators for 'B.ByteString' strings,
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
    , (<?>)
    , try
    , module Data.Attoparsec.Combinator

    -- * Parsing individual bytes
    , satisfy
    , satisfyWith
    , anyWord8
    , skip
    , word8
    , notWord8
    , peekWord8

    -- ** Byte classes
    , inClass
    , notInClass

    -- * Parsing more complicated structures
    , storable

    -- * Efficient string handling
    , skipWhile
    , string
    , stringTransform
    , take
    , scan
    , takeWhile
    , takeWhile1
    , takeTill

    -- ** Consume all remaining input
    , takeByteString
    , takeLazyByteString

    -- * State observation and manipulation functions
    , endOfInput
    , atEnd

    -- * Utilities
    , endOfLine
    ) where

import Control.Applicative ((<|>), (<$>))
import Control.Monad (when)
import Data.Attoparsec.ByteString.FastSet (charClass, memberWord8)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Internal.Types
    hiding (Parser, Input, Added, Failure, Success)
import Data.Monoid (Monoid(..))
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
import Foreign.Storable (Storable(peek, sizeOf))
import Prelude hiding (getChar, take, takeWhile)
import qualified Data.Attoparsec.Internal.Types as T
import qualified Data.ByteString as B8
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B

#if defined(__GLASGOW_HASKELL__)
import GHC.Base (realWorld#)
import GHC.IO (IO(IO))
#else
import System.IO.Unsafe (unsafePerformIO)
#endif

type Parser = T.Parser B.ByteString
type Result = IResult B.ByteString
type Input = T.Input B.ByteString
type Added = T.Added B.ByteString
type Failure r = T.Failure B.ByteString r
type Success a r = T.Success B.ByteString a r

ensure' :: Int -> Input -> Added -> More -> Failure r -> Success B.ByteString r
        -> IResult B.ByteString r
ensure' !n0 i0 a0 m0 kf0 ks0 =
    T.runParser (demandInput >> go n0) i0 a0 m0 kf0 ks0
  where
    go !n = T.Parser $ \i a m kf ks ->
        if B.length (unI i) >= n
        then ks i a m (unI i)
        else T.runParser (demandInput >> go n) i a m kf ks

-- | If at least @n@ bytes of input are available, return the current
-- input, otherwise fail.
ensure :: Int -> Parser B.ByteString
ensure !n = T.Parser $ \i0 a0 m0 kf ks ->
    if B.length (unI i0) >= n
    then ks i0 a0 m0 (unI i0)
    -- The uncommon case is kept out-of-line to reduce code size:
    else ensure' n i0 a0 m0 kf ks
-- Non-recursive so the bounds check can be inlined:
{-# INLINE ensure #-}

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: Input -> Added -> More
       -> (Input -> Added -> More -> Result r)
       -> (Input -> Added -> More -> Result r)
       -> Result r
prompt i0 a0 _m0 kf ks = Partial $ \s ->
    if B.null s
    then kf i0 a0 Complete
    else ks (i0 <> I s) (a0 <> A s) Incomplete

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Parser ()
demandInput = T.Parser $ \i0 a0 m0 kf ks ->
    if m0 == Complete
    then kf i0 a0 m0 ["demandInput"] "not enough bytes"
    else let kf' i a m = kf i a m ["demandInput"] "not enough bytes"
             ks' i a m = ks i a m ()
         in prompt i0 a0 m0 kf' ks'

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Parser Bool
wantInput = T.Parser $ \i0 a0 m0 _kf ks ->
  case () of
    _ | not (B.null (unI i0)) -> ks i0 a0 m0 True
      | m0 == Complete  -> ks i0 a0 m0 False
      | otherwise       -> let kf' i a m = ks i a m False
                               ks' i a m = ks i a m True
                           in prompt i0 a0 m0 kf' ks'

get :: Parser B.ByteString
get  = T.Parser $ \i0 a0 m0 _kf ks -> ks i0 a0 m0 (unI i0)

put :: B.ByteString -> Parser ()
put s = T.Parser $ \_i0 a0 m0 _kf ks -> ks (I s) a0 m0 ()

-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is provided for compatibility with Parsec.
-- Attoparsec parsers always backtrack on failure.
try :: Parser a -> Parser a
try p = p
{-# INLINE try #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  s <- ensure 1
  let !w = B.unsafeHead s
  if p w
    then put (B.unsafeTail s) >> return w
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any byte for which the predicate
-- @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit w = w >= 48 && w <= 57
skip :: (Word8 -> Bool) -> Parser ()
skip p = do
  s <- ensure 1
  if p (B.unsafeHead s)
    then put (B.unsafeTail s)
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! B.unsafeHead s
  if p c
    then let !t = B.unsafeTail s
         in put t >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

storable :: Storable a => Parser a
storable = hack undefined
 where
  hack :: Storable b => b -> Parser b
  hack dummy = do
    (fp,o,_) <- B.toForeignPtr `fmap` take (sizeOf dummy)
    return . B.inlinePerformIO . withForeignPtr fp $ \p ->
        peek (castPtr $ p `plusPtr` o)

-- | Consume @n@ bytes of input, but succeed only if the predicate
-- returns 'True'.
takeWith :: Int -> (B.ByteString -> Bool) -> Parser B.ByteString
takeWith n0 p = do
  let n = max n0 0
  s <- ensure n
  let h = B.unsafeTake n s
      t = B.unsafeDrop n s
  if p h
    then put t >> return h
    else fail "takeWith"

-- | Consume exactly @n@ bytes of input.
take :: Int -> Parser B.ByteString
take n = takeWith n (const True)
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
-- before failing.  In Attoparsec, the above parser will /succeed/ on
-- that input, because the failed first branch will consume nothing.
string :: B.ByteString -> Parser B.ByteString
string s = takeWith (B.length s) (==s)
{-# INLINE string #-}

stringTransform :: (B.ByteString -> B.ByteString) -> B.ByteString
                -> Parser B.ByteString
stringTransform f s = takeWith (B.length s) ((==f s) . f)
{-# INLINE stringTransform #-}

-- | Skip past input for as long as the predicate returns 'True'.
skipWhile :: (Word8 -> Bool) -> Parser ()
skipWhile p = go
 where
  go = do
    t <- B8.dropWhile p <$> get
    put t
    when (B.null t) $ do
      input <- wantInput
      when input go
{-# INLINE skipWhile #-}

-- | Consume input as long as the predicate returns 'False'
-- (i.e. until it returns 'True'), and return the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'True' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeTill :: (Word8 -> Bool) -> Parser B.ByteString
takeTill p = takeWhile (not . p)
{-# INLINE takeTill #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'False' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
takeWhile :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile p = (B.concat . reverse) `fmap` go []
 where
  go acc = do
    (h,t) <- B8.span p <$> get
    put t
    if B.null t
      then do
        input <- wantInput
        if input
          then go (h:acc)
          else return (h:acc)
      else return (h:acc)
{-# INLINE takeWhile #-}

takeRest :: Parser [B.ByteString]
takeRest = go []
 where
  go acc = do
    input <- wantInput
    if input
      then do
        s <- get
        put B.empty
        go (s:acc)
      else return (reverse acc)

-- | Consume all remaining input and return it as a single string.
takeByteString :: Parser B.ByteString
takeByteString = B.concat `fmap` takeRest

-- | Consume all remaining input and return it as a single string.
takeLazyByteString :: Parser L.ByteString
takeLazyByteString = L.fromChunks `fmap` takeRest

data T s = T {-# UNPACK #-} !Int s

-- | A stateful scanner.  The predicate consumes and transforms a
-- state argument, and each transformed state is passed to successive
-- invocations of the predicate on each byte of the input until one
-- returns 'Nothing' or the input ends.
--
-- This parser does not fail.  It will return an empty string if the
-- predicate returns 'Nothing' on the first byte of input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
scan :: s -> (s -> Word8 -> Maybe s) -> Parser B.ByteString
scan s0 p = do
  chunks <- go [] s0
  case chunks of
    [x] -> return x
    xs  -> return $! B.concat $ reverse xs
 where
  go acc s1 = do
    let scanner (B.PS fp off len) =
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
        !t = B.unsafeDrop i bs
    put t
    if B.null t
      then do
        input <- wantInput
        if input
          then go (h:acc) s'
          else return (h:acc)
      else return (h:acc)
{-# INLINE scan #-}

-- | Consume input as long as the predicate returns 'True', and return
-- the consumed input.
--
-- This parser requires the predicate to succeed on at least one byte
-- of input: it will fail if the predicate never returns 'True' or if
-- there is no input left.
takeWhile1 :: (Word8 -> Bool) -> Parser B.ByteString
takeWhile1 p = do
  (`when` demandInput) =<< B.null <$> get
  (h,t) <- B8.span p <$> get
  when (B.null h) $ fail "takeWhile1"
  put t
  if B.null t
    then (h<>) `fmap` takeWhile p
    else return h

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

-- | Match any byte. Returns 'Nothing' if end of input has been
-- reached. Does not consume any input.
--
-- /Note/: Because this parser does not fail, do not use it with
-- combinators such as 'many', because such parsers loop until a
-- failure occurs.  Careless use will thus result in an infinite loop.
peekWord8 :: Parser (Maybe Word8)
peekWord8 = T.Parser $ \i0 a0 m0 _kf ks ->
            if B.null (unI i0)
            then if m0 == Complete
                 then ks i0 a0 m0 Nothing
                 else let ks' i a m = let !w = B.unsafeHead (unI i)
                                      in ks i a m (Just w)
                          kf' i a m = ks i a m Nothing
                      in prompt i0 a0 m0 kf' ks'
            else let !w = B.unsafeHead (unI i0)
                 in ks i0 a0 m0 (Just w)
{-# INLINE peekWord8 #-}

-- | Match only if all input has been consumed.
endOfInput :: Parser ()
endOfInput = T.Parser $ \i0 a0 m0 kf ks ->
             if B.null (unI i0)
             then if m0 == Complete
                  then ks i0 a0 m0 ()
                  else let kf' i1 a1 m1 _ _ = addS i0 a0 m0 i1 a1 m1 $
                                              \ i2 a2 m2 -> ks i2 a2 m2 ()
                           ks' i1 a1 m1 _   = addS i0 a0 m0 i1 a1 m1 $
                                              \ i2 a2 m2 -> kf i2 a2 m2 []
                                                            "endOfInput"
                       in  T.runParser demandInput i0 a0 m0 kf' ks'
             else kf i0 a0 m0 [] "endOfInput"

-- | Return an indication of whether the end of input has been
-- reached.
atEnd :: Parser Bool
atEnd = not <$> wantInput
{-# INLINE atEnd #-}

-- | Match either a single newline character @\'\\n\'@, or a carriage
-- return followed by a newline character @\"\\r\\n\"@.
endOfLine :: Parser ()
endOfLine = (word8 10 >> return ()) <|> (string "\r\n" >> return ())

-- | Name the parser, in case failure occurs.
(<?>) :: Parser a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser a
p <?> msg0 = T.Parser $ \i0 a0 m0 kf ks ->
             let kf' i a m strs msg = kf i a m (msg0:strs) msg
             in T.runParser p i0 a0 m0 kf' ks
{-# INLINE (<?>) #-}
infix 0 <?>

-- | Terminal failure continuation.
failK :: Failure a
failK i0 _a0 _m0 stack msg = Fail (unI i0) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK i0 _a0 _m0 a = Done (unI i0) a
{-# INLINE successK #-}

-- | Run a parser.
parse :: Parser a -> B.ByteString -> Result a
parse m s = T.runParser m (I s) mempty Incomplete failK successK
{-# INLINE parse #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser a -> B.ByteString -> Either String a
parseOnly m s = case T.runParser m (I s) mempty Complete failK successK of
                  Fail _ _ err -> Left err
                  Done _ a     -> Right a
                  _            -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining. /Very unsafe/. In
-- particular, you should do no memory allocation inside an
-- 'inlinePerformIO' block. On Hugs this is just @unsafePerformIO@.
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif
{-# INLINE inlinePerformIO #-}
