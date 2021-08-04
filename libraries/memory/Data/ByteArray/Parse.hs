-- |
-- Module      : Data.ByteArray.Parse
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : portable
--
-- A very simple bytearray parser related to Parsec and Attoparsec
--
-- Simple example:
--
-- > > parse ((,,) <$> take 2 <*> byte 0x20 <*> (bytes "abc" *> anyByte)) "xx abctest"
-- > ParseOK "est" ("xx", 116)
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.ByteArray.Parse
    ( Parser
    , Result(..)
    -- * run the Parser
    , parse
    , parseFeed
    -- * Parser methods
    , hasMore
    , byte
    , anyByte
    , bytes
    , take
    , takeWhile
    , takeAll
    , skip
    , skipWhile
    , skipAll
    , takeStorable
    ) where

import           Control.Monad
import qualified Control.Monad.Fail as Fail
import           Foreign.Storable              (Storable, peek, sizeOf)
import           Data.Word

import           Data.Memory.Internal.Imports
import           Data.Memory.Internal.Compat
import           Data.ByteArray.Types          (ByteArrayAccess, ByteArray)
import qualified Data.ByteArray.Types     as B
import qualified Data.ByteArray.Methods   as B

import           Prelude hiding (take, takeWhile)

-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more input data
--
-- * success: the remaining unparsed data and the parser value
data Result byteArray a =
      ParseFail String
    | ParseMore (Maybe byteArray -> Result byteArray a)
    | ParseOK   byteArray a

instance (Show ba, Show a) => Show (Result ba a) where
    show (ParseFail err) = "ParseFailure: " ++ err
    show (ParseMore _)   = "ParseMore _"
    show (ParseOK b a)   = "ParseOK " ++ show a ++ " " ++ show b

-- | The continuation of the current buffer, and the error string
type Failure byteArray r = byteArray -> String -> Result byteArray r

-- | The continuation of the next buffer value, and the parsed value
type Success byteArray a r = byteArray -> a -> Result byteArray r

-- | Simple ByteString parser structure
newtype Parser byteArray a = Parser
    { runParser :: forall r . byteArray
                           -> Failure byteArray r
                           -> Success byteArray a r
                           -> Result byteArray r }

instance Functor (Parser byteArray) where
    fmap f p = Parser $ \buf err ok ->
        runParser p buf err (\b a -> ok b (f a))
instance Applicative (Parser byteArray) where
    pure      = return
    (<*>) d e = d >>= \b -> e >>= \a -> return (b a)
instance Monad (Parser byteArray) where
#if !(MIN_VERSION_base(4,13,0))
    fail          = Fail.fail
#endif
    return v      = Parser $ \buf _ ok -> ok buf v
    m >>= k       = Parser $ \buf err ok ->
         runParser m buf err (\buf' a -> runParser (k a) buf' err ok)
instance Fail.MonadFail (Parser byteArray) where
    fail errorMsg = Parser $ \buf err _ -> err buf ("Parser failed: " ++ errorMsg)
instance MonadPlus (Parser byteArray) where
    mzero = fail "MonadPlus.mzero"
    mplus f g = Parser $ \buf err ok ->
        -- rewrite the err callback of @f to call @g
        runParser f buf (\_ _ -> runParser g buf err ok) ok
instance Alternative (Parser byteArray) where
    empty = fail "Alternative.empty"
    (<|>) = mplus

-- | Run a parser on an @initial byteArray.
--
-- If the Parser need more data than available, the @feeder function
-- is automatically called and fed to the More continuation.
parseFeed :: (ByteArrayAccess byteArray, Monad m)
          => m (Maybe byteArray)
          -> Parser byteArray a
          -> byteArray
          -> m (Result byteArray a)
parseFeed feeder p initial = loop $ parse p initial
  where loop (ParseMore k) = feeder >>= (loop . k)
        loop r             = return r

-- | Run a Parser on a ByteString and return a 'Result'
parse :: ByteArrayAccess byteArray
      => Parser byteArray a -> byteArray -> Result byteArray a
parse p s = runParser p s (\_ msg -> ParseFail msg) (\b a -> ParseOK b a)

------------------------------------------------------------

-- When needing more data, getMore append the next data
-- to the current buffer. if no further data, then
-- the err callback is called.
getMore :: ByteArray byteArray => Parser byteArray ()
getMore = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    case nextChunk of
        Nothing -> err buf "EOL: need more data"
        Just nc
            | B.null nc -> runParser getMore buf err ok
            | otherwise -> ok (B.append buf nc) ()

-- Only used by takeAll, which accumulate all the remaining data
-- until ParseMore is fed a Nothing value.
--
-- getAll cannot fail.
getAll :: ByteArray byteArray => Parser byteArray ()
getAll = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    case nextChunk of
        Nothing -> ok buf ()
        Just nc -> runParser getAll (B.append buf nc) err ok

-- Only used by skipAll, which flush all the remaining data
-- until ParseMore is fed a Nothing value.
--
-- flushAll cannot fail.
flushAll :: ByteArray byteArray => Parser byteArray ()
flushAll = Parser $ \buf err ok -> ParseMore $ \nextChunk ->
    case nextChunk of
        Nothing -> ok buf ()
        Just _  -> runParser flushAll B.empty err ok

------------------------------------------------------------
hasMore :: ByteArray byteArray => Parser byteArray Bool
hasMore = Parser $ \buf err ok ->
    if B.null buf
        then ParseMore $ \nextChunk ->
                case nextChunk of
                    Nothing -> ok buf False
                    Just nc -> runParser hasMore nc err ok
        else ok buf True

-- | Get the next byte from the parser
anyByte :: ByteArray byteArray => Parser byteArray Word8
anyByte = Parser $ \buf err ok ->
    case B.uncons buf of
        Nothing      -> runParser (getMore >> anyByte) buf err ok
        Just (c1,b2) -> ok b2 c1

-- | Parse a specific byte at current position
--
-- if the byte is different than the expected on,
-- this parser will raise a failure.
byte :: ByteArray byteArray => Word8 -> Parser byteArray ()
byte w = Parser $ \buf err ok ->
    case B.uncons buf of
        Nothing      -> runParser (getMore >> byte w) buf err ok
        Just (c1,b2) | c1 == w   -> ok b2 ()
                     | otherwise -> err buf ("byte " ++ show w ++ " : failed : got " ++ show c1)

-- | Parse a sequence of bytes from current position
--
-- if the following bytes don't match the expected
-- bytestring completely, the parser will raise a failure
bytes :: (Show ba, Eq ba, ByteArray ba) => ba -> Parser ba ()
bytes allExpected = consumeEq allExpected
  where errMsg = "bytes " ++ show allExpected ++ " : failed"

        -- partially consume as much as possible or raise an error.
        consumeEq expected = Parser $ \actual err ok ->
            let eLen = B.length expected in
            if B.length actual >= eLen
                then    -- enough data for doing a full match
                        let (aMatch,aRem) = B.splitAt eLen actual
                         in if aMatch == expected
                                then ok aRem ()
                                else err actual errMsg
                else    -- not enough data, match as much as we have, and then recurse.
                        let (eMatch, eRem) = B.splitAt (B.length actual) expected
                         in if actual == eMatch
                                then runParser (getMore >> consumeEq eRem) B.empty err ok
                                else err actual errMsg

------------------------------------------------------------

-- | Take a storable from the current position in the stream
takeStorable :: (ByteArray byteArray, Storable d)
             => Parser byteArray d
takeStorable = anyStorable undefined
  where
    anyStorable :: ByteArray byteArray => Storable d => d -> Parser byteArray d
    anyStorable a = do
        buf <- take (sizeOf a)
        return $ unsafeDoIO $ B.withByteArray buf $ \ptr -> peek ptr

-- | Take @n bytes from the current position in the stream
take :: ByteArray byteArray => Int -> Parser byteArray byteArray
take n = Parser $ \buf err ok ->
    if B.length buf >= n
        then let (b1,b2) = B.splitAt n buf in ok b2 b1
        else runParser (getMore >> take n) buf err ok

-- | Take bytes while the @predicate hold from the current position in the stream
takeWhile :: ByteArray byteArray => (Word8 -> Bool) -> Parser byteArray byteArray
takeWhile predicate = Parser $ \buf err ok ->
    let (b1, b2) = B.span predicate buf
     in if B.null b2
            then runParser (getMore >> takeWhile predicate) buf err ok
            else ok b2 b1

-- | Take the remaining bytes from the current position in the stream
takeAll :: ByteArray byteArray => Parser byteArray byteArray
takeAll = Parser $ \buf err ok ->
    runParser (getAll >> returnBuffer) buf err ok
  where
    returnBuffer = Parser $ \buf _ ok -> ok B.empty buf

-- | Skip @n bytes from the current position in the stream
skip :: ByteArray byteArray => Int -> Parser byteArray ()
skip n = Parser $ \buf err ok ->
    if B.length buf >= n
        then ok (B.drop n buf) ()
        else runParser (getMore >> skip (n - B.length buf)) B.empty err ok

-- | Skip bytes while the @predicate hold from the current position in the stream
skipWhile :: ByteArray byteArray => (Word8 -> Bool) -> Parser byteArray ()
skipWhile p = Parser $ \buf err ok ->
    let (_, b2) = B.span p buf 
     in if B.null b2
            then runParser (getMore >> skipWhile p) B.empty err ok
            else ok b2 ()

-- | Skip all the remaining bytes from the current position in the stream
skipAll :: ByteArray byteArray => Parser byteArray ()
skipAll = Parser $ \buf err ok -> runParser flushAll buf err ok
