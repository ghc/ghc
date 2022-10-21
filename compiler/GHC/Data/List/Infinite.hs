{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module GHC.Data.List.Infinite
  ( Infinite (..)
  , head, tail
  , filter
  , (++)
  , unfoldr
  , (!!)
  , groupBy
  , dropList
  , iterate
  , concatMap
  , allListsOf
  , toList
  , repeat
  , enumFrom
  ) where

import Prelude ((-), Applicative (..), Bool (..), Enum (succ), Foldable, Functor (..), Int, Maybe (..), Monad (..), Traversable (..), (<$>), flip, otherwise)
import Control.Category (Category (..))
import Control.Monad (guard)
import qualified Data.Foldable as F
import Data.List.NonEmpty (NonEmpty (..))
import qualified GHC.Base as List (build)

infixr 5 `Inf`

data Infinite a = Inf a (Infinite a)
  deriving (Foldable, Functor, Traversable)

head :: Infinite a -> a
head (Inf a _) = a
{-# NOINLINE [1] head #-}

tail :: Infinite a -> Infinite a
tail (Inf _ as) = as
{-# NOINLINE [1] tail #-}

{-# RULES
"head/build" forall (g :: forall b . (a -> b -> b) -> b) . head (build g) = g \ x _ -> x
  #-}

instance Applicative Infinite where
    pure = repeat
    Inf f fs <*> Inf a as = Inf (f a) (fs <*> as)

instance Monad Infinite where
    x >>= f = join (f <$> x)
      where
        join (Inf a as) = head a `Inf` join (tail <$> as)

mapMaybe :: (a -> Maybe b) -> Infinite a -> Infinite b
mapMaybe f = go
  where
    go (Inf a as) = let bs = go as in case f a of
        Nothing -> bs
        Just b -> Inf b bs
{-# NOINLINE [1] mapMaybe #-}

{-# RULES
"mapMaybe" [~1] forall f as . mapMaybe f as = build \ c -> foldr (mapMaybeFB c f) as
"mapMaybeList" [1] forall f . foldr (mapMaybeFB Inf f) = mapMaybe f
  #-}

{-# INLINE [0] mapMaybeFB #-}
mapMaybeFB :: (b -> r -> r) -> (a -> Maybe b) -> a -> r -> r
mapMaybeFB cons f a bs = case f a of
    Nothing -> bs
    Just r -> cons r bs

filter :: (a -> Bool) -> Infinite a -> Infinite a
filter f = mapMaybe (\ a -> a <$ guard (f a))
{-# INLINE filter #-}

infixr 5 ++
(++) :: Foldable f => f a -> Infinite a -> Infinite a
(++) = flip (F.foldr Inf)

unfoldr :: (b -> (a, b)) -> b -> Infinite a
unfoldr f b = build \ c -> let go b = case f b of (a, b') -> a `c` go b' in go b
{-# INLINE unfoldr #-}

(!!) :: Infinite a -> Int -> a
Inf a _ !! 0 = a
Inf _ as !! n = as !! (n-1)

groupBy :: (a -> a -> Bool) -> Infinite a -> Infinite (NonEmpty a)
groupBy eq = go
  where
    go (Inf a as) = Inf (a:|bs) (go cs)
      where (bs, cs) = span (eq a) as

span :: (a -> Bool) -> Infinite a -> ([a], Infinite a)
span p = spanJust (\ a -> a <$ guard (p a))
{-# INLINE span #-}

spanJust :: (a -> Maybe b) -> Infinite a -> ([b], Infinite a)
spanJust p = go
  where
    go as@(Inf a as')
      | Just b <- p a = let (bs, cs) = go as' in (b:bs, cs)
      | otherwise = ([], as)

iterate :: (a -> a) -> a -> Infinite a
iterate f = go where go a = Inf a (go (f a))
{-# NOINLINE [1] iterate #-}

{-# RULES
"iterate" [~1] forall f a . iterate f a = build (\ c -> iterateFB c f a)
"iterateFB" [1] iterateFB Inf = iterate
  #-}

iterateFB :: (a -> b -> b) -> (a -> a) -> a -> b
iterateFB c f a = go a
  where go a = a `c` go (f a)
{-# INLINE [0] iterateFB #-}

concatMap :: Foldable f => (a -> f b) -> Infinite a -> Infinite b
concatMap f = go where go (Inf a as) = f a ++ go as
{-# NOINLINE [1] concatMap #-}

{-# RULES "concatMap" forall f as . concatMap f as = build \ c -> foldr (\ x b -> F.foldr c b (f x)) as #-}

{-# SPECIALIZE concatMap :: (a -> [b]) -> Infinite a -> Infinite b #-}

foldr :: (a -> b -> b) -> Infinite a -> b
foldr f = go where go (Inf a as) = f a (go as)
{-# INLINE [0] foldr #-}

build :: (forall b . (a -> b -> b) -> b) -> Infinite a
build g = g Inf
{-# INLINE [1] build #-}

-- Analogous to 'foldr'/'build' fusion for '[]'
{-# RULES
"foldr/build" forall f (g :: forall b . (a -> b -> b) -> b) . foldr f (build g) = g f
"foldr/id" foldr Inf = id

"foldr/cons/build" forall f a (g :: forall b . (a -> b -> b) -> b) . foldr f (Inf a (build g)) = f a (g f)
  #-}

{-# RULES
"map" [~1] forall f (as :: Infinite a) . fmap f as = build \ c -> foldr (mapFB c f) as
"mapFB" forall c f g . mapFB (mapFB c f) g = mapFB c (f . g)
"mapFB/id" forall c . mapFB c (\ x -> x) = c
  #-}

mapFB :: (b -> c -> c) -> (a -> b) -> a -> c -> c
mapFB c f = \ x ys -> c (f x) ys
{-# INLINE [0] mapFB #-}

dropList :: [a] -> Infinite b -> Infinite b
dropList [] bs = bs
dropList (_:as) (Inf _ bs) = dropList as bs

-- | Compute all lists of the given alphabet.
-- For example: @'allListsOf' "ab" = ["a", "b", "aa", "ba", "ab", "bb", "aaa", "baa", "aba", ...]@
allListsOf :: [a] -> Infinite [a]
allListsOf as = concatMap (\ bs -> [a:bs | a <- as]) ([] `Inf` allListsOf as)

-- See Note [Fusion for `Infinite` lists].
toList :: Infinite a -> [a]
toList = \ as -> List.build (\ c _ -> foldr c as)
{-# INLINE toList #-}

repeat :: a -> Infinite a
repeat a = as where as = Inf a as
{-# INLINE [0] repeat #-}

repeatFB :: (a -> b -> b) -> a -> b
repeatFB c x = xs where xs = c x xs
{-# INLINE [0] repeatFB #-}

{-# RULES
"repeat" [~1] forall a . repeat a = build \ c -> repeatFB c a
"repeatFB" [1] repeatFB Inf = repeat
  #-}

enumFrom :: Enum a => a -> Infinite a
enumFrom = iterate succ
{-# INLINE enumFrom #-}

{-
Note [Fusion for `Infinite` lists]
~~~~~~~~~~~~~~~~~~~~
We use RULES to support foldr/build fusion for Infinite lists, analogously to the RULES in
GHC.Base to support fusion for regular lists. In particular, we define the following:
• `build :: (forall b . (a -> b -> b) -> b) -> Infinite a`
• `foldr :: (a -> b -> b) -> Infinite a -> b`
• A RULE `foldr f (build g) = g f`
• `Infinite`-producing functions in terms of `build`, and `Infinite`-consuming functions in
  terms of `foldr`

This can work across data types. For example, consider `toList :: Infinite a -> [a]`.
We want 'toList' to be both a good consumer (of 'Infinite' lists) and a good producer (of '[]').
Ergo, we define it in terms of 'Infinite.foldr' and `List.build`.

For a bigger example, consider `List.map f (toList (Infinite.map g as))`

We want to fuse away the intermediate `Infinite` structure between `Infnite.map` and `toList`,
and the list structure between `toList` and `List.map`. And indeed we do: see test
"InfiniteListFusion".
-}
