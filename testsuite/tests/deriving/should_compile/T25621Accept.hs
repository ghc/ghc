{-# language DerivingVia #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}

module T25621Accept where

import Data.Functor.Identity
import Data.Kind
import GHC.Generics


type Table :: Type -> Constraint
class Table a where
  type Columns a :: (Type -> Type) -> Type
  type Context a :: Type -> Type

  toColumns :: a -> Columns a (Context a)
  fromColumns :: Columns a (Context a) -> a


type HLift :: Type -> (Type -> Type) -> Type
newtype HLift a f = HLift (f a)


instance Table Bool where
  type Columns Bool = HLift Bool
  type Context Bool = Identity

  toColumns = HLift . Identity
  fromColumns (HLift (Identity a)) = a


type HProduct :: ((Type -> Type) -> Type) -> ((Type -> Type) -> Type) -> (Type -> Type) -> Type
data HProduct h i f = HProduct (h f) (i f)


type GColumns :: (Type -> Type) -> (Type -> Type) -> Type
type family GColumns rep where
  GColumns (M1 _i _c rep) = GColumns rep
  GColumns (rep1 :*: rep2) = HProduct (GColumns rep1) (GColumns rep2)
  GColumns (K1 _i a) = Columns a


type GTable :: (Type -> Type) -> (Type -> Type) -> Constraint
class GTable f rep where
  gtoColumns :: rep x -> GColumns rep f
  gfromColumns :: GColumns rep f -> rep x


instance GTable f rep => GTable f (M1 i c rep) where
  gtoColumns (M1 a) = gtoColumns a
  gfromColumns = M1 . gfromColumns


instance (GTable f rep1, GTable f rep2) => GTable f (rep1 :*: rep2) where
  gtoColumns (a :*: b) = HProduct (gtoColumns a) (gtoColumns b)
  gfromColumns (HProduct a b) = gfromColumns a :*: gfromColumns b


instance (Table a, Context a ~ f) => GTable f (K1 i a) where
  gtoColumns (K1 a) = toColumns a
  gfromColumns = K1 . fromColumns


type GContext :: (Type -> Type) -> Type -> Type
type family GContext rep where
  GContext (M1 _i _c rep) = GContext rep
  GContext (rep :*: _) = GContext rep
  GContext (K1 _i a) = Context a


instance (Generic a, GTable (GContext (Rep a)) (Rep a)) => Table (Generically a) where
  type Columns (Generically a) = GColumns (Rep a)
  type Context (Generically a) = GContext (Rep a)

  toColumns (Generically a) = gtoColumns (from a)
  fromColumns = Generically . to . gfromColumns


data Foo = Foo Bool Bool Bool
  deriving Generic
  deriving Table via Generically Foo
  -- works, the inferred instance is:
  --
  --   instance Table Foo


data Bar a = Bar a a a
  deriving Generic
  deriving Table via Generically (Bar a)
  -- works, the inferred instance is:
  --
  --   instance Table a => Table (Bar a)


data Baz a = Baz Bool a
  deriving Generic
  deriving Table via Generically (Baz a)
  -- works, the inferred instance is:
  --
  --   instance (Context a ~ Identity, Table a) => Table (Baz a)
  --
  -- Critically, this is only accepted because the quality constraint
  -- `Context a ~ Identity` has a type family application (`Context a`) on one
  -- side of the equality.
