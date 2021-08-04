{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Data.ByteString.Unsafe
#endif
{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Data.Attoparsec.Zepto
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- A tiny, highly specialized combinator parser for 'B.ByteString'
-- strings.
--
-- While the main attoparsec module generally performs well, this
-- module is particularly fast for simple non-recursive loops that
-- should not normally result in failed parses.
--
-- /Warning/: on more complex inputs involving recursion or failure,
-- parsers based on this module may be as much as /ten times slower/
-- than regular attoparsec! You should /only/ use this module when you
-- have benchmarks that prove that its use speeds your code up.
module Data.Attoparsec.Zepto
    (
      Parser
    , ZeptoT
    , parse
    , parseT
    , atEnd
    , string
    , take
    , takeWhile
    ) where

import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import Data.Functor.Identity (Identity(runIdentity))
import Data.Monoid as Mon (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Word (Word8)
import Prelude hiding (take, takeWhile)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

newtype S = S {
      input :: ByteString
    }

data Result a = Fail String
              | OK !a S

-- | A simple parser.
--
-- This monad is strict in its state, and the monadic bind operator
-- ('>>=') evaluates each result to weak head normal form before
-- passing it along.
newtype ZeptoT m a = Parser {
      runParser :: S -> m (Result a)
    }

type Parser a = ZeptoT Identity a

instance Monad m => Functor (ZeptoT m) where
    fmap f m = Parser $ \s -> do
      result <- runParser m s
      case result of
        OK a s'  -> return (OK (f a) s')
        Fail err -> return (Fail err)
    {-# INLINE fmap #-}

instance MonadIO m => MonadIO (ZeptoT m) where
  liftIO act = Parser $ \s -> do
    result <- liftIO act
    return (OK result s)
  {-# INLINE liftIO #-}

instance Monad m => Monad (ZeptoT m) where
    return = pure
    {-# INLINE return #-}

    m >>= k   = Parser $ \s -> do
      result <- runParser m s
      case result of
        OK a s'  -> runParser (k a) s'
        Fail err -> return (Fail err)
    {-# INLINE (>>=) #-}

#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

instance Monad m => Fail.MonadFail (ZeptoT m) where
    fail msg = Parser $ \_ -> return (Fail msg)
    {-# INLINE fail #-}

instance Monad m => MonadPlus (ZeptoT m) where
    mzero = fail "mzero"
    {-# INLINE mzero #-}

    mplus a b = Parser $ \s -> do
      result <- runParser a s
      case result of
        ok@(OK _ _) -> return ok
        _           -> runParser b s
    {-# INLINE mplus #-}

instance (Monad m) => Applicative (ZeptoT m) where
    pure a = Parser $ \s -> return (OK a s)
    {-# INLINE pure #-}
    (<*>)  = ap
    {-# INLINE (<*>) #-}

gets :: Monad m => (S -> a) -> ZeptoT m a
gets f = Parser $ \s -> return (OK (f s) s)
{-# INLINE gets #-}

put :: Monad m => S -> ZeptoT m ()
put s = Parser $ \_ -> return (OK () s)
{-# INLINE put #-}

-- | Run a parser.
parse :: Parser a -> ByteString -> Either String a
parse p bs = case runIdentity (runParser p (S bs)) of
               (OK a _)   -> Right a
               (Fail err) -> Left err
{-# INLINE parse #-}

-- | Run a parser on top of the given base monad.
parseT :: Monad m => ZeptoT m a -> ByteString -> m (Either String a)
parseT p bs = do
  result <- runParser p (S bs)
  case result of
    OK a _   -> return (Right a)
    Fail err -> return (Left err)
{-# INLINE parseT #-}

instance Monad m => Semigroup (ZeptoT m a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monad m => Mon.Monoid (ZeptoT m a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Monad m => Alternative (ZeptoT m) where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

-- | Consume input while the predicate returns 'True'.
takeWhile :: Monad m => (Word8 -> Bool) -> ZeptoT m ByteString
takeWhile p = do
  (h,t) <- gets (B.span p . input)
  put (S t)
  return h
{-# INLINE takeWhile #-}

-- | Consume @n@ bytes of input.
take :: Monad m => Int -> ZeptoT m ByteString
take !n = do
  s <- gets input
  if B.length s >= n
    then put (S (B.unsafeDrop n s)) >> return (B.unsafeTake n s)
    else fail "insufficient input"
{-# INLINE take #-}

-- | Match a string exactly.
string :: Monad m => ByteString -> ZeptoT m ()
string s = do
  i <- gets input
  if s `B.isPrefixOf` i
    then put (S (B.unsafeDrop (B.length s) i)) >> return ()
    else fail "string"
{-# INLINE string #-}

-- | Indicate whether the end of the input has been reached.
atEnd :: Monad m => ZeptoT m Bool
atEnd = do
  i <- gets input
  return $! B.null i
{-# INLINE atEnd #-}
