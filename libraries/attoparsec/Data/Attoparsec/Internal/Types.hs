{-# LANGUAGE CPP, BangPatterns, GeneralizedNewtypeDeriving, OverloadedStrings,
    Rank2Types, RecordWildCards, TypeFamilies #-}
-- |
-- Module      :  Data.Attoparsec.Internal.Types
-- Copyright   :  Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  unknown
--
-- Simple, efficient parser combinators, loosely based on the Parsec
-- library.

module Data.Attoparsec.Internal.Types
    (
      Parser(..)
    , State
    , Failure
    , Success
    , Pos(..)
    , IResult(..)
    , More(..)
    , (<>)
    , Chunk(..)
    ) where

import Control.Applicative as App (Applicative(..), (<$>))
import Control.Applicative (Alternative(..))
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(..))
import qualified Control.Monad.Fail as Fail (MonadFail(..))
import Data.Monoid as Mon (Monoid(..))
import Data.Semigroup  (Semigroup(..))
import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (w2c)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Unsafe (Iter(..))
import Prelude hiding (succ)
import qualified Data.Attoparsec.ByteString.Buffer as B
import qualified Data.Attoparsec.Text.Buffer as T

newtype Pos = Pos { fromPos :: Int }
            deriving (Eq, Ord, Show, Num)

-- | The result of a parse.  This is parameterised over the type @i@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult i r =
    Fail i [String] String
    -- ^ The parse failed.  The @i@ parameter is the input that had
    -- not yet been consumed when the failure occurred.  The
    -- @[@'String'@]@ is a list of contexts in which the error
    -- occurred.  The 'String' is the message describing the error, if
    -- any.
  | Partial (i -> IResult i r)
    -- ^ Supply this continuation with more input so that the parser
    -- can resume.  To indicate that no more input is available, pass
    -- an empty string to the continuation.
    --
    -- __Note__: if you get a 'Partial' result, do not call its
    -- continuation more than once.
  | Done i r
    -- ^ The parse succeeded.  The @i@ parameter is the input that had
    -- not yet been consumed (if any) when the parse succeeded.

instance (Show i, Show r) => Show (IResult i r) where
    showsPrec d ir = showParen (d > 10) $
      case ir of
        (Fail t stk msg) -> showString "Fail" . f t . f stk . f msg
        (Partial _)      -> showString "Partial _"
        (Done t r)       -> showString "Done" . f t . f r
      where f :: Show a => a -> ShowS
            f x = showChar ' ' . showsPrec 11 x

instance (NFData i, NFData r) => NFData (IResult i r) where
    rnf (Fail t stk msg) = rnf t `seq` rnf stk `seq` rnf msg
    rnf (Partial _)  = ()
    rnf (Done t r)   = rnf t `seq` rnf r
    {-# INLINE rnf #-}

instance Functor (IResult i) where
    fmap _ (Fail t stk msg) = Fail t stk msg
    fmap f (Partial k)      = Partial (fmap f . k)
    fmap f (Done t r)   = Done t (f r)

-- | The core parser type.  This is parameterised over the type @i@
-- of string being processed.
--
-- This type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception (i.e. fails) with an
--   error message.
--
-- * 'Functor' and 'Applicative', which follow the usual definitions.
--
-- * 'MonadPlus', where 'mzero' fails (with no error message) and
--   'mplus' executes the right-hand parser if the left-hand one
--   fails.  When the parser on the right executes, the input is reset
--   to the same state as the parser on the left started with. (In
--   other words, attoparsec is a backtracking parser that supports
--   arbitrary lookahead.)
--
-- * 'Alternative', which follows 'MonadPlus'.
newtype Parser i a = Parser {
      runParser :: forall r.
                   State i -> Pos -> More
                -> Failure i (State i)   r
                -> Success i (State i) a r
                -> IResult i r
    }

type family State i
type instance State ByteString = B.Buffer
type instance State Text = T.Buffer

type Failure i t   r = t -> Pos -> More -> [String] -> String
                       -> IResult i r
type Success i t a r = t -> Pos -> More -> a -> IResult i r

-- | Have we read all available input?
data More = Complete | Incomplete
            deriving (Eq, Show)

instance Semigroup More where
    c@Complete <> _ = c
    _          <> m = m

instance Mon.Monoid More where
    mappend = (<>)
    mempty  = Incomplete

instance Monad (Parser i) where
#if !(MIN_VERSION_base(4,13,0))
    fail = Fail.fail
    {-# INLINE fail #-}
#endif

    return = App.pure
    {-# INLINE return #-}

    m >>= k = Parser $ \t !pos more lose succ ->
        let succ' t' !pos' more' a = runParser (k a) t' pos' more' lose succ
        in runParser m t pos more lose succ'
    {-# INLINE (>>=) #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}


instance Fail.MonadFail (Parser i) where
    fail err = Parser $ \t pos more lose _succ -> lose t pos more [] msg
      where msg = "Failed reading: " ++ err
    {-# INLINE fail #-}

plus :: Parser i a -> Parser i a -> Parser i a
plus f g = Parser $ \t pos more lose succ ->
  let lose' t' _pos' more' _ctx _msg = runParser g t' pos more' lose succ
  in runParser f t pos more lose' succ

instance MonadPlus (Parser i) where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus = plus

instance Functor (Parser i) where
    fmap f p = Parser $ \t pos more lose succ ->
      let succ' t' pos' more' a = succ t' pos' more' (f a)
      in runParser p t pos more lose succ'
    {-# INLINE fmap #-}

apP :: Parser i (a -> b) -> Parser i a -> Parser i b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative (Parser i) where
    pure v = Parser $ \t !pos more _lose succ -> succ t pos more v
    {-# INLINE pure #-}
    (<*>)  = apP
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}
    x <* y = x >>= \a -> y >> pure a
    {-# INLINE (<*) #-}

instance Semigroup (Parser i a) where
    (<>) = plus
    {-# INLINE (<>) #-}

instance Monoid (Parser i a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}

instance Alternative (Parser i) where
    empty = fail "empty"
    {-# INLINE empty #-}

    (<|>) = plus
    {-# INLINE (<|>) #-}

    many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE many #-}

    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE some #-}

-- | A common interface for input chunks.
class Monoid c => Chunk c where
  type ChunkElem c
  -- | Test if the chunk is empty.
  nullChunk :: c -> Bool
  -- | Append chunk to a buffer.
  pappendChunk :: State c -> c -> State c
  -- | Position at the end of a buffer. The first argument is ignored.
  atBufferEnd :: c -> State c -> Pos
  -- | Return the buffer element at the given position along with its length.
  bufferElemAt :: c -> Pos -> State c -> Maybe (ChunkElem c, Int)
  -- | Map an element to the corresponding character.
  --   The first argument is ignored.
  chunkElemToChar :: c -> ChunkElem c -> Char

instance Chunk ByteString where
  type ChunkElem ByteString = Word8
  nullChunk = BS.null
  {-# INLINE nullChunk #-}
  pappendChunk = B.pappend
  {-# INLINE pappendChunk #-}
  atBufferEnd _ = Pos . B.length
  {-# INLINE atBufferEnd #-}
  bufferElemAt _ (Pos i) buf
    | i < B.length buf = Just (B.unsafeIndex buf i, 1)
    | otherwise = Nothing
  {-# INLINE bufferElemAt #-}
  chunkElemToChar _ = w2c
  {-# INLINE chunkElemToChar #-}

instance Chunk Text where
  type ChunkElem Text = Char
  nullChunk = Text.null
  {-# INLINE nullChunk #-}
  pappendChunk = T.pappend
  {-# INLINE pappendChunk #-}
  atBufferEnd _ = Pos . T.length
  {-# INLINE atBufferEnd #-}
  bufferElemAt _ (Pos i) buf
    | i < T.length buf = let Iter c l = T.iter buf i in Just (c, l)
    | otherwise = Nothing
  {-# INLINE bufferElemAt #-}
  chunkElemToChar _ = id
  {-# INLINE chunkElemToChar #-}
