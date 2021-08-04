{-# LANGUAGE BangPatterns, CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-} -- Imports internal modules
#endif
-- |
-- Module      :  Data.Attoparsec.Combinator
-- Copyright   :  Daan Leijen 1999-2001, Bryan O'Sullivan 2007-2015
-- License     :  BSD3
--
-- Maintainer  :  bos@serpentine.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Useful parser combinators, similar to those provided by Parsec.
module Data.Attoparsec.Combinator
    (
    -- * Combinators
      try
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
    , feed
    , satisfyElem
    , endOfInput
    , atEnd
    , lookAhead
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..), (<$>))
import Data.Monoid (Monoid(mappend))
#endif
import Control.Applicative (Alternative(..), liftA2, many, (<|>))
import Control.Monad (MonadPlus(..))
import Data.Attoparsec.Internal.Types (Parser(..), IResult(..))
import Data.Attoparsec.Internal (endOfInput, atEnd, satisfyElem)
import Data.ByteString (ByteString)
import Data.Foldable (asum)
import Data.Text (Text)
import qualified Data.Attoparsec.Zepto as Z
import Prelude hiding (succ)

-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is provided for compatibility with Parsec.
-- attoparsec parsers always backtrack on failure.
try :: Parser i a -> Parser i a
try = id
{-# INLINE try #-}

-- | Name the parser, in case failure occurs.
(<?>) :: Parser i a
      -> String                 -- ^ the name to use if parsing fails
      -> Parser i a
p <?> msg0 = Parser $ \t pos more lose succ ->
             let lose' t' pos' more' strs msg = lose t' pos' more' (msg0:strs) msg
             in runParser p t pos more lose' succ
{-# INLINE (<?>) #-}
infix 0 <?>

-- | @choice ps@ tries to apply the actions in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding
-- action.
choice :: Alternative f => [f a] -> f a
choice = asum
{-# SPECIALIZE choice :: [Parser ByteString a]
                      -> Parser ByteString a #-}
{-# SPECIALIZE choice :: [Parser Text a] -> Parser Text a #-}
{-# SPECIALIZE choice :: [Z.Parser a] -> Z.Parser a #-}

-- | @option x p@ tries to apply action @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- > priority  = option 0 (digitToInt <$> digit)
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x
{-# SPECIALIZE option :: a -> Parser ByteString a -> Parser ByteString a #-}
{-# SPECIALIZE option :: a -> Parser Text a -> Parser Text a #-}
{-# SPECIALIZE option :: a -> Z.Parser a -> Z.Parser a #-}

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | @many' p@ applies the action @p@ /zero/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many' letter
many' :: (MonadPlus m) => m a -> m [a]
many' p = many_p
  where many_p = some_p `mplus` return []
        some_p = liftM2' (:) p many_p
{-# INLINE many' #-}

-- | @many1 p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@.
--
-- >  word  = many1 letter
many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)
{-# INLINE many1 #-}

-- | @many1' p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many1' letter
many1' :: (MonadPlus m) => m a -> m [a]
many1' p = liftM2' (:) p (many' p)
{-# INLINE many1' #-}

-- | @sepBy p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy` (char ',')
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
{-# SPECIALIZE sepBy :: Parser ByteString a -> Parser ByteString s
                     -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}

-- | @sepBy' p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy'` (char ',')
sepBy' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy' p s = scan `mplus` return []
  where scan = liftM2' (:) p ((s >> sepBy1' p s) `mplus` return [])
{-# SPECIALIZE sepBy' :: Parser ByteString a -> Parser ByteString s
                      -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy' :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy' :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}

-- | @sepBy1 p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy1` (char ',')
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan
    where scan = liftA2 (:) p ((s *> scan) <|> pure [])
{-# SPECIALIZE sepBy1 :: Parser ByteString a -> Parser ByteString s
                      -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy1 :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy1 :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}

-- | @sepBy1' p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy1'` (char ',')
sepBy1' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy1' p s = scan
    where scan = liftM2' (:) p ((s >> scan) `mplus` return [])
{-# SPECIALIZE sepBy1' :: Parser ByteString a -> Parser ByteString s
                       -> Parser ByteString [a] #-}
{-# SPECIALIZE sepBy1' :: Parser Text a -> Parser Text s -> Parser Text [a] #-}
{-# SPECIALIZE sepBy1' :: Z.Parser a -> Z.Parser s -> Z.Parser [a] #-}

-- | @manyTill p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill anyChar (string "-->")
--
-- (Note the overlapping parsers @anyChar@ and @string \"-->\"@.
-- While this will work, it is not very efficient, as it will cause a
-- lot of backtracking.)
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = scan
    where scan = (end *> pure []) <|> liftA2 (:) p scan
{-# SPECIALIZE manyTill :: Parser ByteString a -> Parser ByteString b
                        -> Parser ByteString [a] #-}
{-# SPECIALIZE manyTill :: Parser Text a -> Parser Text b -> Parser Text [a] #-}
{-# SPECIALIZE manyTill :: Z.Parser a -> Z.Parser b -> Z.Parser [a] #-}

-- | @manyTill' p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill' anyChar (string "-->")
--
-- (Note the overlapping parsers @anyChar@ and @string \"-->\"@.
-- While this will work, it is not very efficient, as it will cause a
-- lot of backtracking.)
--
-- The value returned by @p@ is forced to WHNF.
manyTill' :: (MonadPlus m) => m a -> m b -> m [a]
manyTill' p end = scan
    where scan = (end >> return []) `mplus` liftM2' (:) p scan
{-# SPECIALIZE manyTill' :: Parser ByteString a -> Parser ByteString b
                         -> Parser ByteString [a] #-}
{-# SPECIALIZE manyTill' :: Parser Text a -> Parser Text b -> Parser Text [a] #-}
{-# SPECIALIZE manyTill' :: Z.Parser a -> Z.Parser b -> Z.Parser [a] #-}

-- | Skip zero or more instances of an action.
skipMany :: Alternative f => f a -> f ()
skipMany p = scan
    where scan = (p *> scan) <|> pure ()
{-# SPECIALIZE skipMany :: Parser ByteString a -> Parser ByteString () #-}
{-# SPECIALIZE skipMany :: Parser Text a -> Parser Text () #-}
{-# SPECIALIZE skipMany :: Z.Parser a -> Z.Parser () #-}

-- | Skip one or more instances of an action.
skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p
{-# SPECIALIZE skipMany1 :: Parser ByteString a -> Parser ByteString () #-}
{-# SPECIALIZE skipMany1 :: Parser Text a -> Parser Text () #-}
{-# SPECIALIZE skipMany1 :: Z.Parser a -> Z.Parser () #-}

-- | Apply the given action repeatedly, returning every result.
count :: Monad m => Int -> m a -> m [a]
count n p = sequence (replicate n p)
{-# INLINE count #-}

-- | Combine two alternatives.
eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}

-- | If a parser has returned a 'T.Partial' result, supply it with more
-- input.
feed :: Monoid i => IResult i r -> i -> IResult i r
feed (Fail t ctxs msg) d = Fail (mappend t d) ctxs msg
feed (Partial k) d    = k d
feed (Done t r) d     = Done (mappend t d) r
{-# INLINE feed #-}

-- | Apply a parser without consuming any input.
lookAhead :: Parser i a -> Parser i a
lookAhead p = Parser $ \t pos more lose succ ->
  let succ' t' _pos' more' = succ t' pos more'
  in runParser p t pos more lose succ'
{-# INLINE lookAhead #-}
