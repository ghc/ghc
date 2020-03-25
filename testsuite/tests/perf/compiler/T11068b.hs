{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module T11068b (Lens', field) where

import Data.Kind
import GHC.Generics
import GHC.TypeLits

----------------------------------------
-- Profunctors

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  lmap  :: (a -> b)             -> p b c -> p a c
  rmap  ::             (c -> d) -> p b c -> p b d

class Profunctor p => Strong p where
  first'  :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)

  linear :: LensVL s t a b -> p a b -> p s t
  linear f = dimap
    ((\(Context bt a) -> (a, bt)) . f (Context id))
    (\(b, bt) -> bt b)
    . first'
  {-# INLINE linear #-}

data Store a b s t = Store (s -> a) (s -> b -> t)

instance Profunctor (Store a b) where
  dimap f g (Store get set) = Store (get . f) (\s -> g . set (f s))
  lmap  f   (Store get set) = Store (get . f) (\s -> set (f s))
  rmap    g (Store get set) = Store get       (\s -> g . set s)

instance Strong (Store a b) where
  first'  (Store get set) = Store (get . fst) (\(s, c) b -> (set s b, c))
  second' (Store get set) = Store (get . snd) (\(c, s) b -> (c, set s b))

----------------------------------------
-- Lens

type LensVL  s t a b = forall f. Functor f => (a -> f b) -> s -> f t
type LensVL' s   a   = LensVL s s a a

newtype Lens  s t a b = Lens (forall p. Strong p => p a b -> p s t)
type    Lens' s   a   = Lens s s a a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set = Lens $ dimap (\s -> (get s, s))
                            (\(b, s) -> set s b)
                    . first'

lensVL :: LensVL s t a b -> Lens s t a b
lensVL l = Lens (linear l)

withLens :: Lens s t a b -> ((s -> a) -> (s -> b -> t) -> r) -> r
withLens (Lens l) k = case l $ Store id (\_ -> id) of
  Store get set -> k get set

----------------------------------------
-- Field

field :: forall name s a. (Generic s, GField name (Rep s) a) => Lens' s a
field = withLens (lensVL (\f s -> to <$> gfield @name f (from s))) lens
{-# INLINE field #-}

class
  ( HasFieldPred name f ~ 'Just a
  ) => GField (name :: Symbol) f a | name f -> a where
  gfield :: LensVL' (f p) a

instance
  ( GField name f a
  , HasFieldPred name f ~ 'Just a
  ) => GField name (M1 C c f) a where
  gfield f (M1 x)  = M1 <$> gfield @name f x

instance
  ( GField name f a
  , HasFieldPred name f ~ 'Just a
  ) => GField name (M1 D c f) a where
  gfield f (M1 x) = M1 <$> gfield @name f x

instance
  ( c ~ 'MetaSel ('Just name) u s l
  , f ~ Rec0 a
  ) => GField name (M1 S c f) a where
  gfield f (M1 (K1 x)) = M1 . K1 <$> f x

instance
  ( HasFieldPred name (f :+: g) ~ 'Just a
  , GField name f a
  , GField name g a
  ) => GField name (f :+: g) a where
  gfield f (L1 x) = L1 <$> gfield @name f x
  gfield f (R1 y) = R1 <$> gfield @name f y

instance
  ( GFieldChoice name f g (HasFieldPred name f) a
  ) => GField name (f :*: g) a where
  gfield = gfieldChoice @name @f @g @(HasFieldPred name f)

class
  ( HasFieldPred name (f :*: g) ~ 'Just a
  ) => GFieldChoice (name :: Symbol) f g (res :: Maybe Type) a where
  gfieldChoice :: LensVL' ((f :*: g) p) a

instance
  ( a ~ a'
  , GField name f a'
  , HasFieldPred name (f :*: g) ~ 'Just a
  ) => GFieldChoice name f g ('Just a') a where
  gfieldChoice f (x :*: y) = (:*: y) <$> gfield @name f x

instance
  ( a ~ a'
  , GField name g a'
  , HasFieldPred name (f :*: g) ~ 'Just a
  ) => GFieldChoice name f g 'Nothing a where
  gfieldChoice f (x :*: y) = (x :*:) <$> gfield @name f y

----------------------------------------
-- Helpers

data Context a b t = Context (b -> t) a
  deriving Functor

type family Both (m1 :: Maybe Type) (m2 :: Maybe Type) :: Maybe Type where
  Both ('Just a) ('Just a) = 'Just a

type family Alt (m1 :: Maybe Type) (m2 :: Maybe Type) :: Maybe Type where
  Alt ('Just a) _ = 'Just a
  Alt _ b = b

type family HasFieldPred (field :: Symbol) f :: Maybe Type where
  HasFieldPred field (S1 ('MetaSel ('Just field) _ _ _) (Rec0 t)) = 'Just t
  HasFieldPred field (S1 _ _)  = 'Nothing
  HasFieldPred field (l :*: r) = Alt (HasFieldPred field l) (HasFieldPred field r)
  HasFieldPred field (l :+: r) = Both (HasFieldPred field l) (HasFieldPred field r)
  HasFieldPred field (C1 _ f)  = HasFieldPred field f
  HasFieldPred field (D1 _ f)  = HasFieldPred field f
  HasFieldPred field (K1 _ _)  = 'Nothing
  HasFieldPred field U1        = 'Nothing
  HasFieldPred field V1        = 'Nothing
