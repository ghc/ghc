{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Biapplicative (
  -- * Biapplicative bifunctors
    Biapplicative(..)
  , (<<$>>)
  , (<<**>>)
  , biliftA3
  , traverseBia
  , sequenceBia
  , traverseBiaWith
  , module Data.Bifunctor
  ) where

import Control.Applicative
import Data.Bifunctor
import Data.Functor.Identity
import GHC.Exts (inline)

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
import Data.Traversable (Traversable (traverse))
#endif

import Data.Semigroup (Arg(..))

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

infixl 4 <<$>>, <<*>>, <<*, *>>, <<**>>
(<<$>>) :: (a -> b) -> a -> b
(<<$>>) = id
{-# INLINE (<<$>>) #-}

class Bifunctor p => Biapplicative p where
#if __GLASGOW_HASKELL__ >= 708
  {-# MINIMAL bipure, ((<<*>>) | biliftA2 ) #-}
#endif
  bipure :: a -> b -> p a b

  (<<*>>) :: p (a -> b) (c -> d) -> p a c -> p b d
  (<<*>>) = biliftA2 id id
  {-# INLINE (<<*>>) #-}

  -- | Lift binary functions
  biliftA2 :: (a -> b -> c) -> (d -> e -> f) -> p a d -> p b e -> p c f
  biliftA2 f g a b = bimap f g <<$>> a <<*>> b
  {-# INLINE biliftA2 #-}

  -- |
  -- @
  -- a '*>>' b ≡ 'bimap' ('const' 'id') ('const' 'id') '<<$>>' a '<<*>>' b
  -- @
  (*>>) :: p a b -> p c d -> p c d
  a *>> b = biliftA2 (const id) (const id) a b
  {-# INLINE (*>>) #-}

  -- |
  -- @
  -- a '<<*' b ≡ 'bimap' 'const' 'const' '<<$>>' a '<<*>>' b
  -- @
  (<<*) :: p a b -> p c d -> p a b
  a <<* b = biliftA2 const const a b
  {-# INLINE (<<*) #-}

(<<**>>) :: Biapplicative p => p a c -> p (a -> b) (c -> d) -> p b d
(<<**>>) = biliftA2 (flip id) (flip id)
{-# INLINE (<<**>>) #-}


-- | Lift ternary functions
biliftA3 :: Biapplicative w => (a -> b -> c -> d) -> (e -> f -> g -> h) -> w a e -> w b f -> w c g -> w d h
biliftA3 f g a b c = biliftA2 f g a b <<*>> c
{-# INLINE biliftA3 #-}

-- | Traverse a 'Traversable' container in a 'Biapplicative'.
--
-- 'traverseBia' satisfies the following properties:
--
-- [/Pairing/]
--
--     @'traverseBia' (,) t = (t, t)@
--
-- [/Composition/]
--
--     @'traverseBia' ('Data.Bifunctor.Biff.Biff' . 'bimap' g h . f) = 'Data.Bifunctor.Biff.Biff' . 'bimap' ('traverse' g) ('traverse' h) . 'traverseBia' f@
--
--     @'traverseBia' ('Data.Bifunctor.Tannen.Tannen' . 'fmap' f . g) = 'Data.Bifunctor.Tannen.Tannen' . 'fmap' ('traverseBia' f) . 'traverse' g@
--
-- [/Naturality/]
--
--     @ t . 'traverseBia' f = 'traverseBia' (t . f) @
--
--     for every biapplicative transformation @t@.
--
--     A /biapplicative transformation/ from a 'Biapplicative' @P@ to a 'Biapplicative' @Q@
--     is a function
--
--     @t :: P a b -> Q a b@
--
--     preserving the 'Biapplicative' operations. That is,
--
--     * @t ('bipure' x y) = 'bipure' x y@
--
--     * @t (x '<<*>>' y) = t x '<<*>>' t y@
--
-- === Performance note
--
-- 'traverseBia' is fairly efficient, and uses compiler rewrite rules
-- to be even more efficient for a few important types like @[]@. However,
-- if performance is critical, you might consider writing a container-specific
-- implementation.
traverseBia :: (Traversable t, Biapplicative p)
            => (a -> p b c) -> t a -> p (t b) (t c)
traverseBia = inline (traverseBiaWith traverse)
-- We explicitly inline traverseBiaWith because it seems likely to help
-- specialization. I'm not much of an expert at the inlining business,
-- so I won't mind if someone else decides to do this differently.

-- We use a staged INLINABLE so we can rewrite traverseBia to specialized
-- versions for a few important types.
{-# INLINABLE [1] traverseBia #-}

-- | Perform all the 'Biappicative' actions in a 'Traversable' container
-- and produce a container with all the results.
--
-- @
-- sequenceBia = 'traverseBia' id
-- @
sequenceBia :: (Traversable t, Biapplicative p)
            => t (p b c) -> p (t b) (t c)
sequenceBia = inline (traverseBia id)
{-# INLINABLE sequenceBia #-}

-- | A version of 'traverseBia' that doesn't care how the traversal is
-- done.
--
-- @
-- 'traverseBia' = traverseBiaWith traverse
-- @
traverseBiaWith :: forall p a b c s t. Biapplicative p
  => (forall f x. Applicative f => (a -> f x) -> s -> f (t x))
  -> (a -> p b c) -> s -> p (t b) (t c)
traverseBiaWith trav p s = smash p (trav One s)
{-# INLINABLE traverseBiaWith #-}

smash :: forall p t a b c. Biapplicative p
      => (a -> p b c)
      -> (forall x. Mag a x (t x))
      -> p (t b) (t c)
smash p m = go m m
  where
    go :: forall x y. Mag a b x -> Mag a c y -> p x y
    go (Pure t) (Pure u) = bipure t u
    go (Map f x) (Map g y) = bimap f g (go x y)
    go (Ap fs xs) (Ap gs ys) = go fs gs <<*>> go xs ys
#if MIN_VERSION_base(4,10,0)
    go (LiftA2 f xs ys) (LiftA2 g zs ws) = biliftA2 f g (go xs zs) (go ys ws)
#endif
    go (One x) (One _) = p x
    go _ _ = impossibleError
{-# INLINABLE smash #-}

-- Let's not end up with a bunch of CallStack junk in the smash
-- unfolding.
impossibleError :: a
impossibleError = error "Impossible: the arguments are always the same."

-- This is used to reify a traversal for 'traverseBia'. It's a somewhat
-- bogus 'Functor' and 'Applicative' closely related to 'Magma' from the
-- @lens@ package. Valid traversals don't use (<$), (<*), or (*>), so
-- we leave them out. We offer all the rest of the Functor and Applicative
-- operations to improve performance: we generally want to keep the structure
-- as small as possible. We might even consider using RULES to widen lifts
-- when we can:
--
--   liftA2 f x y <*> z ==> liftA3 f x y z,
--
-- etc., up to the pointer tagging limit. But we do need to be careful. I don't
-- *think* GHC will ever inline the traversal into the go function (because that
-- would duplicate work), but if it did, and if different RULES fired for the
-- two copies, everything would break horribly.
--
-- Note: if it's necessary for some reason, we *could* relax GADTs to
-- ExistentialQuantification by changing the type of One to
--
--   One :: (b -> c) -> a -> Mag a b c
--
-- where the function will always end up being id. But we allocate a *lot*
-- of One constructors, so this would definitely be bad for performance.
data Mag a b t where
  Pure :: t -> Mag a b t
  Map :: (x -> t) -> Mag a b x -> Mag a b t
  Ap :: Mag a b (t -> u) -> Mag a b t -> Mag a b u
#if MIN_VERSION_base(4,10,0)
  LiftA2 :: (t -> u -> v) -> Mag a b t -> Mag a b u -> Mag a b v
#endif
  One :: a -> Mag a b b

instance Functor (Mag a b) where
  fmap = Map

instance Applicative (Mag a b) where
  pure = Pure
  (<*>) = Ap
#if MIN_VERSION_base(4,10,0)
  liftA2 = LiftA2
#endif

-- Rewrite rules for traversing a few important types. These avoid the overhead
-- of allocating and matching on a Mag.
{-# RULES
"traverseBia/list" forall f t. traverseBia f t = traverseBiaList f t
"traverseBia/Maybe" forall f t. traverseBia f t = traverseBiaMaybe f t
"traverseBia/Either" forall f t. traverseBia f t = traverseBiaEither f t
"traverseBia/Identity" forall f t. traverseBia f t = traverseBiaIdentity f t
"traverseBia/Const" forall f t. traverseBia f t = traverseBiaConst f t
"traverseBia/Pair" forall f t. traverseBia f t = traverseBiaPair f t
 #-}

traverseBiaList :: Biapplicative p => (a -> p b c) -> [a] -> p [b] [c]
traverseBiaList f = foldr go (bipure [] [])
  where
    go x r = biliftA2 (:) (:) (f x) r

traverseBiaMaybe :: Biapplicative p => (a -> p b c) -> Maybe a -> p (Maybe b) (Maybe c)
traverseBiaMaybe _f Nothing = bipure Nothing Nothing
traverseBiaMaybe f (Just x) = bimap Just Just (f x)

traverseBiaEither :: Biapplicative p => (a -> p b c) -> Either e a -> p (Either e b) (Either e c)
traverseBiaEither f (Right x) = bimap Right Right (f x)
traverseBiaEither _f (Left (e :: e)) = bipure m m
  where
    m :: Either e x
    m = Left e

traverseBiaIdentity :: Biapplicative p => (a -> p b c) -> Identity a -> p (Identity b) (Identity c)
traverseBiaIdentity f (Identity x) = bimap Identity Identity (f x)

traverseBiaConst :: Biapplicative p => (a -> p b c) -> Const x a -> p (Const x b) (Const x c)
traverseBiaConst _f (Const x) = bipure (Const x) (Const x)

traverseBiaPair :: Biapplicative p => (a -> p b c) -> (e, a) -> p (e, b) (e, c)
traverseBiaPair f (x,y) = bimap ((,) x) ((,) x) (f y)

----------------------------------------------
--
-- Instances

instance Biapplicative (,) where
  bipure = (,)
  {-# INLINE bipure #-}
  (f, g) <<*>> (a, b) = (f a, g b)
  {-# INLINE (<<*>>) #-}
  biliftA2 f g (x, y) (a, b) = (f x a, g y b)
  {-# INLINE biliftA2 #-}

instance Biapplicative Arg where
  bipure = Arg
  {-# INLINE bipure #-}
  Arg f g <<*>> Arg a b = Arg (f a) (g b)
  {-# INLINE (<<*>>) #-}
  biliftA2 f g (Arg x y) (Arg a b) = Arg (f x a) (g y b)
  {-# INLINE biliftA2 #-}

instance Monoid x => Biapplicative ((,,) x) where
  bipure = (,,) mempty
  {-# INLINE bipure #-}
  (x, f, g) <<*>> (x', a, b) = (mappend x x', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y) => Biapplicative ((,,,) x y) where
  bipure = (,,,) mempty mempty
  {-# INLINE bipure #-}
  (x, y, f, g) <<*>> (x', y', a, b) = (mappend x x', mappend y y', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y, Monoid z) => Biapplicative ((,,,,) x y z) where
  bipure = (,,,,) mempty mempty mempty
  {-# INLINE bipure #-}
  (x, y, z, f, g) <<*>> (x', y', z', a, b) = (mappend x x', mappend y y', mappend z z', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y, Monoid z, Monoid w) => Biapplicative ((,,,,,) x y z w) where
  bipure = (,,,,,) mempty mempty mempty mempty
  {-# INLINE bipure #-}
  (x, y, z, w, f, g) <<*>> (x', y', z', w', a, b) = (mappend x x', mappend y y', mappend z z', mappend w w', f a, g b)
  {-# INLINE (<<*>>) #-}

instance (Monoid x, Monoid y, Monoid z, Monoid w, Monoid v) => Biapplicative ((,,,,,,) x y z w v) where
  bipure = (,,,,,,) mempty mempty mempty mempty mempty
  {-# INLINE bipure #-}
  (x, y, z, w, v, f, g) <<*>> (x', y', z', w', v', a, b) = (mappend x x', mappend y y', mappend z z', mappend w w', mappend v v', f a, g b)
  {-# INLINE (<<*>>) #-}

#ifdef MIN_VERSION_tagged
instance Biapplicative Tagged where
  bipure _ b = Tagged b
  {-# INLINE bipure #-}

  Tagged f <<*>> Tagged x = Tagged (f x)
  {-# INLINE (<<*>>) #-}
#endif

instance Biapplicative Const where
  bipure a _ = Const a
  {-# INLINE bipure #-}
  Const f <<*>> Const x = Const (f x)
  {-# INLINE (<<*>>) #-}
