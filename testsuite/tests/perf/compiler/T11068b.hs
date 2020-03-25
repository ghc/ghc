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
module T11068b (Lens', GField(..)) where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
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

class GField (name :: Symbol) s a | name s -> a where
  gfield :: Lens' s a

instance
  ( Generic s
  , GFieldImpl name s (Rep s) a
  ) => GField name s a where
  gfield = withLens
    (lensVL (\f s -> to <$> gfieldImpl @name @s f (from s)))
    (\get set -> lensVL $ \f s -> set s <$> f (get s))
  {-# INLINE gfield #-}

data Void0
instance a ~ Void0 => GField name Void0 a where
  gfield = lensVL id

class GFieldImpl (name :: Symbol) s g a | name g -> a where
  gfieldImpl :: LensVL' (g x) a

instance
  ( GFieldImpl name s g a
  ) => GFieldImpl name s (M1 D m g) a where
  gfieldImpl f (M1 x) = M1 <$> gfieldImpl @name @s f x

instance
  ( GFieldImpl name s g a
  ) => GFieldImpl name s (M1 C m g) a where
  gfieldImpl f (M1 x) = M1 <$> gfieldImpl @name @s f x

instance
  ( g ~ Rec0 a
  , If (name == fname)
       (() :: Constraint)
       (TypeError
         ('Text "Type " ':<>: Quoted ('ShowType s) ':<>:
          'Text " doesn't have a field named " ':<>: Quoted ('Text name) ':$$:
          'Text " in some of its data constructors"))
  ) => GFieldImpl name s (M1 S ('MetaSel ('Just fname) su ss ds) g) a where
  gfieldImpl f (M1 (K1 x)) = M1 . K1 <$> f x

instance
  ( GFieldImpl name s g1 a
  , GFieldImpl name s g2 a
  ) => GFieldImpl name s (g1 :+: g2) a where
  gfieldImpl f (L1 x) = L1 <$> gfieldImpl @name @s f x
  gfieldImpl f (R1 y) = R1 <$> gfieldImpl @name @s f y
  {-# INLINE gfieldImpl #-}

instance
  ( GFieldProd name (HasFieldPred name g1) s g1 g2 a
  ) => GFieldImpl name s (g1 :*: g2) a where
  gfieldImpl = gfieldProd @name @(HasFieldPred name g1) @s

class GFieldProd (name :: Symbol) (res :: Maybe Type) s g1 g2 a | name g1 g2 -> a where
  gfieldProd :: LensVL' ((g1 :*: g2) x) a

instance
  ( a ~ b
  , GFieldImpl name s g1 a
  ) => GFieldProd name ('Just b) s g1 g2 a where
  gfieldProd f (x :*: y) = (:*: y) <$> gfieldImpl @name @s f x

instance
  ( GFieldImpl name s g2 a
  ) => GFieldProd name 'Nothing s g1 g2 a where
  gfieldProd f (x :*: y) = (x :*:) <$> gfieldImpl @name @s f y


----------------------------------------
-- Helpers

data Context a b t = Context (b -> t) a
  deriving Functor

type family Quoted (s :: ErrorMessage) :: ErrorMessage where
  Quoted s = 'Text "‘" ':<>: s ':<>: 'Text "’"

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
