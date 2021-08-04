{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- |
-- Module      :  Data.Attoparsec.ByteString
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient combinator parsing for 'B.ByteString' strings,
-- loosely based on the Parsec library.

module Data.Attoparsec.ByteString
    (
    -- * Differences from Parsec
    -- $parsec

    -- * Incremental input
    -- $incremental

    -- * Performance considerations
    -- $performance

    -- * Parser types
      I.Parser
    , Result
    , T.IResult(..)
    , I.compareResults

    -- * Running parsers
    , parse
    , feed
    , I.parseOnly
    , parseWith
    , parseTest

    -- ** Result conversion
    , maybeResult
    , eitherResult

    -- * Parsing individual bytes
    , I.word8
    , I.anyWord8
    , I.notWord8
    , I.satisfy
    , I.satisfyWith
    , I.skip

    -- ** Lookahead
    , I.peekWord8
    , I.peekWord8'

    -- ** Byte classes
    , I.inClass
    , I.notInClass

    -- * Efficient string handling
    , I.string
    , I.skipWhile
    , I.take
    , I.scan
    , I.runScanner
    , I.takeWhile
    , I.takeWhile1
    , I.takeWhileIncluding
    , I.takeTill
    , I.getChunk

    -- ** Consume all remaining input
    , I.takeByteString
    , I.takeLazyByteString

    -- * Combinators
    , try
    , (<?>)
    , choice
    , count
    , option
    , many'
    , many1
    , many1'
    , manyTill
    , manyTill'
    , sepBy
    , sepBy'
    , sepBy1
    , sepBy1'
    , skipMany
    , skipMany1
    , eitherP
    , I.match
    -- * State observation and manipulation functions
    , I.endOfInput
    , I.atEnd
    ) where

import Data.Attoparsec.Combinator
import Data.List (intercalate)
import qualified Data.Attoparsec.ByteString.Internal as I
import qualified Data.Attoparsec.Internal as I
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Internal (Result, parse)
import qualified Data.Attoparsec.Internal.Types as T

-- $parsec
--
-- Compared to Parsec 3, attoparsec makes several tradeoffs.  It is
-- not intended for, or ideal for, all possible uses.
--
-- * While attoparsec can consume input incrementally, Parsec cannot.
--   Incremental input is a huge deal for efficient and secure network
--   and system programming, since it gives much more control to users
--   of the library over matters such as resource usage and the I/O
--   model to use.
--
-- * Much of the performance advantage of attoparsec is gained via
--   high-performance parsers such as 'I.takeWhile' and 'I.string'.
--   If you use complicated combinators that return lists of bytes or
--   characters, there is less performance difference between the two
--   libraries.
--
-- * Unlike Parsec 3, attoparsec does not support being used as a
--   monad transformer.
--
-- * attoparsec is specialised to deal only with strict 'B.ByteString'
--   input.  Efficiency concerns rule out both lists and lazy
--   bytestrings.  The usual use for lazy bytestrings would be to
--   allow consumption of very large input without a large footprint.
--   For this need, attoparsec's incremental input provides an
--   excellent substitute, with much more control over when input
--   takes place.  If you must use lazy bytestrings, see the
--   "Data.Attoparsec.ByteString.Lazy" module, which feeds lazy chunks
--   to a regular parser.
--
-- * Parsec parsers can produce more helpful error messages than
--   attoparsec parsers.  This is a matter of focus: attoparsec avoids
--   the extra book-keeping in favour of higher performance.

-- $incremental
--
-- attoparsec supports incremental input, meaning that you can feed it
-- a bytestring that represents only part of the expected total amount
-- of data to parse. If your parser reaches the end of a fragment of
-- input and could consume more input, it will suspend parsing and
-- return a 'T.Partial' continuation.
--
-- Supplying the 'T.Partial' continuation with a bytestring will
-- resume parsing at the point where it was suspended, with the
-- bytestring you supplied used as new input at the end of the
-- existing input. You must be prepared for the result of the resumed
-- parse to be another 'T.Partial' continuation.
--
-- To indicate that you have no more input, supply the 'T.Partial'
-- continuation with an empty bytestring.
--
-- Remember that some parsing combinators will not return a result
-- until they reach the end of input.  They may thus cause 'T.Partial'
-- results to be returned.
--
-- If you do not need support for incremental input, consider using
-- the 'I.parseOnly' function to run your parser.  It will never
-- prompt for more input.
--
-- /Note/: incremental input does /not/ imply that attoparsec will
-- release portions of its internal state for garbage collection as it
-- proceeds.  Its internal representation is equivalent to a single
-- 'ByteString': if you feed incremental input to a parser, it will
-- require memory proportional to the amount of input you supply.
-- (This is necessary to support arbitrary backtracking.)

-- $performance
--
-- If you write an attoparsec-based parser carefully, it can be
-- realistic to expect it to perform similarly to a hand-rolled C
-- parser (measuring megabytes parsed per second).
--
-- To actually achieve high performance, there are a few guidelines
-- that it is useful to follow.
--
-- Use the 'B.ByteString'-oriented parsers whenever possible,
-- e.g. 'I.takeWhile1' instead of 'many1' 'I.anyWord8'.  There is
-- about a factor of 100 difference in performance between the two
-- kinds of parser.
--
-- For very simple byte-testing predicates, write them by hand instead
-- of using 'I.inClass' or 'I.notInClass'.  For instance, both of
-- these predicates test for an end-of-line byte, but the first is
-- much faster than the second:
--
-- >endOfLine_fast w = w == 13 || w == 10
-- >endOfLine_slow   = inClass "\r\n"
--
-- Make active use of benchmarking and profiling tools to measure,
-- find the problems with, and improve the performance of your parser.

-- | Run a parser and print its result to standard output.
parseTest :: (Show a) => I.Parser a -> B.ByteString -> IO ()
parseTest p s = print (parse p s)

-- | Run a parser with an initial input string, and a monadic action
-- that can supply more input if needed.
parseWith :: Monad m =>
             (m B.ByteString)
          -- ^ An action that will be executed to provide the parser
          -- with more input, if necessary.  The action must return an
          -- 'B.empty' string when there is no more input available.
          -> I.Parser a
          -> B.ByteString
          -- ^ Initial input for the parser.
          -> m (Result a)
parseWith refill p s = step $ parse p s
  where step (T.Partial k) = (step . k) =<< refill
        step r             = return r
{-# INLINE parseWith #-}

-- | Convert a 'Result' value to a 'Maybe' value. A 'T.Partial' result
-- is treated as failure.
maybeResult :: Result r -> Maybe r
maybeResult (T.Done _ r) = Just r
maybeResult _            = Nothing

-- | Convert a 'Result' value to an 'Either' value. A 'T.Partial'
-- result is treated as failure.
eitherResult :: Result r -> Either String r
eitherResult (T.Done _ r)        = Right r
eitherResult (T.Fail _ [] msg)   = Left msg
eitherResult (T.Fail _ ctxs msg) = Left (intercalate " > " ctxs ++ ": " ++ msg)
eitherResult _                   = Left "Result: incomplete input"
