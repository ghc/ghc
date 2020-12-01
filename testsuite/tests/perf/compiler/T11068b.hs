{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
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

-- Code taken from the optics / generic-lens-lite library.

----------------------------------------
-- Profunctors

data Context a b t = Context (b -> t) a
  deriving Functor

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
  , path ~ GetPathTree name (Rep s)
  , GFieldSum path (Rep s) a
  ) => GField name s a where
  gfield = withLens
    (lensVL (\f s -> to <$> gfieldSum @path f (from s)))
    (\get set -> lensVL $ \f s -> set s <$> f (get s))
  {-# INLINE gfield #-}

data Void0
-- | Hidden instance.
instance {-# DYSFUNCTIONAL #-} a ~ Void0 => GField name Void0 a where
  gfield = lensVL id

class GFieldSum (path :: PathTree) (g :: Type -> Type) a | path g -> a where
  gfieldSum :: LensVL' (g x) a

instance {-# DYSFUNCTIONAL #-}
  ( TypeError ('Text "Type has no data constructors")
  ) => GFieldSum path V1 a where
  gfieldSum = error "unreachable"

instance
  ( GFieldSum path g a
  ) => GFieldSum path (M1 D m g) a where
  gfieldSum f (M1 x) = M1 <$> gfieldSum @path f x

instance
  ( GFieldSum path1 g1 a
  , GFieldSum path2 g2 a
  ) => GFieldSum ('PathTree path1 path2) (g1 :+: g2) a where
  gfieldSum f (L1 x) = L1 <$> gfieldSum @path1 f x
  gfieldSum f (R1 y) = R1 <$> gfieldSum @path2 f y
  {-# INLINE gfieldSum #-}

instance
  ( path ~ FromMaybe (TypeError ('Text "No such field")) mpath
  , GFieldProd path g a
  ) => GFieldSum ('PathLeaf mpath) (M1 C m g) a where
  gfieldSum f (M1 x) = M1 <$> gfieldProd @path f x

class GFieldProd (path :: [Path]) g a | path g -> a where
  gfieldProd :: LensVL' (g x) a

instance
  ( GFieldProd path g1 a
  ) => GFieldProd ('PathLeft : path) (g1 :*: g2) a where
  gfieldProd f (x :*: y) = (:*: y) <$> gfieldProd @path f x

instance
  ( GFieldProd path g2 a
  ) => GFieldProd ('PathRight : path) (g1 :*: g2) a where
  gfieldProd f (x :*: y) = (x :*:) <$> gfieldProd @path f y

instance
  ( a ~ b -- for better error message if types don't match
  ) => GFieldProd '[] (M1 S ('MetaSel ('Just name) su ss ds) (Rec0 b)) a where
  gfieldProd f (M1 (K1 x)) = M1 . K1 <$> f x

----------------------------------------
-- Helpers

data PathTree
  = PathTree PathTree PathTree
  | PathLeaf (Maybe [Path])
  | NoPath

data Path = PathLeft | PathRight

-- | Compute paths to a field for a generic representation of a data type.
type family GetPathTree (name :: Symbol) g :: PathTree where
  GetPathTree name (M1 D _ g)  = GetPathTree name g
  GetPathTree name V1          = 'NoPath
  GetPathTree name (g1 :+: g2) = 'PathTree (GetPathTree name g1)
                                           (GetPathTree name g2)
  GetPathTree name (M1 C _ g)  = 'PathLeaf (GetPath name g '[])

-- | Compute path to a constructor in a sum or a field in a product.
type family GetPath (name :: Symbol) g (acc :: [Path]) :: Maybe [Path] where
  GetPath name (M1 D _ g) acc = GetPath name g acc

  -- Find path to a constructor in a sum type
  GetPath name (M1 C ('MetaCons name _ _) _) acc = 'Just (Reverse acc '[])
  GetPath name (g1 :+: g2) acc = Alt (GetPath name g1 ('PathLeft  : acc))
                                     (GetPath name g2 ('PathRight : acc))

  -- Find path to a field in a product type
  GetPath name (M1 S ('MetaSel ('Just name) _ _ _) _) acc = 'Just (Reverse acc '[])
  GetPath name (g1 :*: g2) acc = Alt (GetPath name g1 ('PathLeft  : acc))
                                     (GetPath name g2 ('PathRight : acc))

  GetPath _ _ _ = 'Nothing

-- | Reverse a type-level list.
type family Reverse (xs :: [k]) (acc :: [k]) :: [k] where
  Reverse '[]      acc = acc
  Reverse (x : xs) acc = Reverse xs (x : acc)

type family FromMaybe (def :: a) (m :: Maybe a) :: a where
  FromMaybe _   ('Just a) = a
  FromMaybe def 'Nothing  = def

-- | Type-level mplus for 'Maybe'.
type family Alt (m1 :: Maybe a) (m2 :: Maybe a) :: Maybe a where
  Alt ('Just a) _ = 'Just a
  Alt         _ b = b
