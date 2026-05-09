
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.Generics
import Data.Coerce
import Data.Semigroup
import Data.List.NonEmpty qualified as NE
import Control.Exception

main :: IO ()
main = do
  let l1 = L [1]
      l2 = L [2]
  -- check append functions; this is a regression test for later down the line
  print ("sappend", l1 <> l2)
  print ("mappend", l1 `mappend` l2)
  print ("gappend", l1 `gappend` l2)
  -- when `mappend` is removed from `Monoid`, `mappend`'s definition will be
  -- `mappend = (<>)` at the top level, removing the above `mappend = gappend` issue

  -- check concat functions
  -- We have the defined method functions, the generic variants, and the
  -- default implementations written out again
  let ls = [l1, l2]
      lsNE = l1 NE.:| [l2]
  print ("sconcat", sconcat lsNE)
  print ("mconcat", mconcat ls)
  print ("gsconcat", gsconcat lsNE)
  print ("gmconcat", gmconcat ls)
  print ("dsconcat", dsconcat lsNE)
  print ("dmconcatMappend", dmconcatMappend ls) -- uses `mappend` which is incorrect, see above
  print ("dmconcatSappend", dmconcatSappend ls)
  print ("dmconcatUsingSconcat", dmconcatUsingSconcat ls)

  let undefinedList = l1 : undefined
      fOn s f = print ("strictness " <> s, f undefinedList)
  (fOn "mconcat" mconcat `finally` -- derived instance is fine, shows
    fOn "gmconcat" gmconcat `finally` -- this is too strict - using mappend on lists needs all lists
    fOn "dmconcatMappend" dmconcatMappend `finally` -- also too strict for the above
    fOn "dmconcatSappend" dmconcatSappend `finally` -- correct strictness, shows
    fOn "dmconcatUsingSconcat" dmconcatUsingSconcat -- sconcat is too strict as well
    ) `catch` \(_ :: SomeException) -> pure ()

newtype L a = L [a]
  deriving (Generic, Show, Eq)
  deriving Monoid via (Generically (L a))

-- semigroup instance not derived with Generically, so it could be mis-aligned
-- with generic monoid definition
instance Semigroup (L a) where
  L [] <> l = l
  l <> _ = l

-- generic (<>)
gappend :: forall a . (Generic a, Semigroup (Rep a ())) => a -> a -> a
gappend a b = to (from a <> from b :: Rep a ())

-- generic sconcat
gsconcat :: forall a . (Generic a, Semigroup (Rep a ())) => NE.NonEmpty a -> a
gsconcat = to . sconcat @(Rep a ()) . fmap from

-- generic mconcat
gmconcat :: forall a . (Generic a, Monoid (Rep a ())) => [a] -> a
gmconcat = to . mconcat @(Rep a ()) . fmap from

-- default sconcat
dsconcat :: forall a . Semigroup a => NE.NonEmpty a -> a
dsconcat (a NE.:| as) = go a as where
  go b (c:cs) = b <> go c cs
  go b []     = b

-- default mconcat using mappend
dmconcatMappend :: Monoid a => [a] -> a
dmconcatMappend = foldr mappend mempty

-- default mconcat using (<>), also the new generically impl
dmconcatSappend :: Monoid a => [a] -> a
dmconcatSappend = foldr (<>) mempty

-- incorrect impl, too strict in the spine
dmconcatUsingSconcat :: Monoid a => [a] -> a
dmconcatUsingSconcat as = case as of
  [] -> mempty
  x : xs -> sconcat (x NE.:| xs)
