module Options.Applicative.Help.Chunk
  ( Chunk(..)
  , chunked
  , listToChunk
  , (<<+>>)
  , (<</>>)
  , vcatChunks
  , vsepChunks
  , isEmpty
  , stringChunk
  , paragraph
  , extractChunk
  , tabulate
  ) where

import Control.Applicative
import Control.Monad
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Semigroup
import Prelude

import Options.Applicative.Help.Pretty

-- | The free monoid on a semigroup 'a'.
newtype Chunk a = Chunk
  { unChunk :: Maybe a }
  deriving (Eq, Show)

instance Functor Chunk where
  fmap f = Chunk . fmap f . unChunk

instance Applicative Chunk where
  pure = Chunk . pure
  Chunk f <*> Chunk x = Chunk (f <*> x)

instance Alternative Chunk where
  empty = Chunk Control.Applicative.empty
  a <|> b = Chunk $ unChunk a <|> unChunk b

instance Monad Chunk where
  return = pure
  m >>= f = Chunk $ unChunk m >>= unChunk . f

instance Semigroup a => Semigroup (Chunk a) where
  (<>) = chunked (<>)

instance Semigroup a => Monoid (Chunk a) where
  mempty = Chunk Nothing
  mappend = (<>)

instance MonadPlus Chunk where
  mzero = Chunk mzero
  mplus m1 m2 = Chunk $ mplus (unChunk m1) (unChunk m2)

-- | Given a semigroup structure on 'a', return a monoid structure on 'Chunk a'.
--
-- Note that this is /not/ the same as 'liftA2'.
chunked :: (a -> a -> a)
        -> Chunk a -> Chunk a -> Chunk a
chunked _ (Chunk Nothing) y = y
chunked _ x (Chunk Nothing) = x
chunked f (Chunk (Just x)) (Chunk (Just y)) = Chunk (Just (f x y))

-- | Concatenate a list into a Chunk.  'listToChunk' satisfies:
--
-- > isEmpty . listToChunk = null
-- > listToChunk = mconcat . fmap pure
listToChunk :: Semigroup a => [a] -> Chunk a
listToChunk [] = mempty
listToChunk (x:xs) = pure (sconcat (x :| xs))

-- | Part of a constrained comonad instance.
--
-- This is the counit of the adjunction between 'Chunk' and the forgetful
-- functor from monoids to semigroups.  It satisfies:
--
-- > extractChunk . pure = id
-- > extractChunk . fmap pure = id
extractChunk :: Monoid a => Chunk a -> a
extractChunk = fromMaybe mempty . unChunk
-- we could also define:
-- duplicate :: Monoid a => Chunk a -> Chunk (Chunk a)
-- duplicate = fmap pure

-- | Concatenate two 'Chunk's with a space in between.  If one is empty, this
-- just returns the other one.
--
-- Unlike '<+>' for 'Doc', this operation has a unit element, namely the empty
-- 'Chunk'.
(<<+>>) :: Chunk Doc -> Chunk Doc -> Chunk Doc
(<<+>>) = chunked (<+>)

-- | Concatenate two 'Chunk's with a softline in between.  This is exactly like
-- '<<+>>', but uses a softline instead of a space.
(<</>>) :: Chunk Doc -> Chunk Doc -> Chunk Doc
(<</>>) = chunked (</>)

-- | Concatenate 'Chunk's vertically.
vcatChunks :: [Chunk Doc] -> Chunk Doc
vcatChunks = foldr (chunked (.$.)) mempty

-- | Concatenate 'Chunk's vertically separated by empty lines.
vsepChunks :: [Chunk Doc] -> Chunk Doc
vsepChunks = foldr (chunked (\x y -> x .$. mempty .$. y)) mempty

-- | Whether a 'Chunk' is empty.  Note that something like 'pure mempty' is not
-- considered an empty chunk, even though the underlying 'Doc' is empty.
isEmpty :: Chunk a -> Bool
isEmpty = isNothing . unChunk

-- | Convert a 'String' into a 'Chunk'.  This satisfies:
--
-- > isEmpty . stringChunk = null
-- > extractChunk . stringChunk = string
stringChunk :: String -> Chunk Doc
stringChunk "" = mempty
stringChunk s = pure (string s)

-- | Convert a paragraph into a 'Chunk'.  The resulting chunk is composed by the
-- words of the original paragraph separated by softlines, so it will be
-- automatically word-wrapped when rendering the underlying document.
--
-- This satisfies:
--
-- > isEmpty . paragraph = null . words
paragraph :: String -> Chunk Doc
paragraph = foldr (chunked (</>) . stringChunk) mempty
          . words

tabulate' :: Int -> [(Doc, Doc)] -> Chunk Doc
tabulate' _ [] = mempty
tabulate' size table = pure $ vcat
  [ indent 2 (fillBreak size key <+> value)
  | (key, value) <- table ]

-- | Display pairs of strings in a table.
tabulate :: [(Doc, Doc)] -> Chunk Doc
tabulate = tabulate' 24
