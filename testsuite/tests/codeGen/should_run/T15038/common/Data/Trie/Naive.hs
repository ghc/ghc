{-# LANGUAGE DeriveFunctor #-}

module Data.Trie.Naive
  ( Trie
  , singleton
  , singletonString
  , lookup
  , parser
  , fromList
  , fromListAppend
  , fromStringList
  ) where

import Prelude hiding (lookup)

import Data.Semigroup (Semigroup)
import Data.Word (Word8)
import Data.Map (Map)
import Data.Bifunctor (second)
import Packed.Bytes (Bytes)
import qualified Data.Char
import qualified GHC.OldList as L
import qualified Packed.Bytes.Parser as P
import qualified Packed.Bytes as B
import qualified Data.Semigroup as SG
import qualified Data.Map.Strict as M

data Trie a = Trie (Maybe a) (Map Word8 (Trie a))
  deriving (Functor)

instance Semigroup a => Semigroup (Trie a) where
  (<>) = append

instance Semigroup a => Monoid (Trie a) where
  mempty = Trie Nothing M.empty
  mappend = (SG.<>)

append :: Semigroup a => Trie a -> Trie a -> Trie a
append (Trie v1 m1) (Trie v2 m2) = Trie
  (SG.getOption (SG.Option v1 SG.<> SG.Option v2))
  (M.unionWith append m1 m2)

singleton :: Bytes -> a -> Trie a
singleton k v = B.foldr (\b r -> Trie Nothing (M.singleton b r)) (Trie (Just v) M.empty) k

singletonString :: String -> a -> Trie a
singletonString k v = L.foldr (\c r -> Trie Nothing (M.singleton (c2w c) r)) (Trie (Just v) M.empty) k

lookup :: Bytes -> Trie a -> Maybe a
lookup k t0 = case B.foldr lookupStep (Just t0) k of
  Nothing -> Nothing
  Just (Trie v _) -> v

lookupStep :: Word8 -> Maybe (Trie a) -> Maybe (Trie a)
lookupStep w Nothing = Nothing
lookupStep w (Just (Trie _ m)) = M.lookup w m

parser :: Trie (P.Parser a) -> P.Parser a
parser (Trie mp m) = case mp of
  Just p -> p
  Nothing -> do
    w <- P.any
    case M.lookup w m of
      Nothing -> P.failure
      Just t -> parser t

fromList :: [(Bytes,a)] -> Trie a
fromList = fmap SG.getFirst . fromListAppend . map (second SG.First)

fromListAppend :: Semigroup a => [(Bytes,a)] -> Trie a
fromListAppend = foldMap (uncurry singleton)

fromStringList :: [(String,a)] -> Trie a
fromStringList = fmap SG.getFirst . fromStringListAppend . map (second SG.First)

fromStringListAppend :: Semigroup a => [(String,a)] -> Trie a
fromStringListAppend = foldMap (uncurry singletonString)

c2w :: Char -> Word8
c2w = fromIntegral . Data.Char.ord
