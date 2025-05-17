{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE InstanceSigs           #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module T16979 (strs) where

import Control.Applicative
import Data.Coerce
import Data.Kind
import Data.Monoid
import GHC.Generics
import GHC.TypeLits

data Poly a b
  = PNil
  | PCons a (Poly b a)
  deriving (Show, Generic)

poly :: Poly Int String
poly = PCons 10 (PCons "hello" (PCons 20 (PCons "world" PNil)))

strs :: [String]
strs = toListOf (param @0) poly

toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f = getConst #. l (Const #. f)
{-# INLINE foldMapOf #-}

type Getting r s a = (a -> Const r a) -> s -> Const r s

class Profunctor p where
  dimap :: (a -> b) -> (c -> d) -> p b c -> p a d
  (#.) :: forall a b c q. Coercible c b => q b c -> p a b -> p a c

instance Profunctor (->) where
  dimap ab cd bc = cd . bc . ab
  {-# INLINE dimap #-}
  (#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
  {-# INLINE (#.) #-}

class HasParam (p :: Nat) s t a b | p t a -> s, p s b -> t, p s -> a, p t -> b where
  param :: Applicative g => (a -> g b) -> s -> g t

instance
  ( GenericN s
  , GenericN t
  , s ~ Infer t (P n b 'PTag) a
  , t ~ Infer s (P n a 'PTag) b
  , a ~ ArgAt s n
  , b ~ ArgAt t n
  , GHasParam n (RepN s) (RepN t) a b
  ) => HasParam n s t a b where

  param = confusing (\f s -> toN <$> gparam @n f (fromN s))
  {-# INLINE param #-}

confusing :: Applicative f => Traversal s t a b -> (a -> f b) -> s -> f t
confusing t = \f -> lowerYoneda . lowerCurried . t (liftCurriedYoneda . f)
{-# INLINE confusing #-}

newtype Yoneda f a = Yoneda { runYoneda :: forall b. (a -> b) -> f b }

instance Functor (Yoneda f) where
  fmap f m = Yoneda (\k -> runYoneda m (k . f))

instance Applicative f => Applicative (Yoneda f) where
  pure a = Yoneda (\f -> pure (f a))
  Yoneda m <*> Yoneda n = Yoneda (\f -> m (f .) <*> n id)

newtype Curried f a =
  Curried { runCurried :: forall r. f (a -> r) -> f r }

instance Functor f => Functor (Curried f) where
  fmap f (Curried g) = Curried (g . fmap (.f))
  {-# INLINE fmap #-}

instance (Functor f) => Applicative (Curried f) where
  pure a = Curried (fmap ($ a))
  {-# INLINE pure #-}
  Curried mf <*> Curried ma = Curried (ma . mf . fmap (.))
  {-# INLINE (<*>) #-}

lowerYoneda :: Yoneda f a -> f a
lowerYoneda (Yoneda f) = f id

lowerCurried :: Applicative f => Curried f a -> f a
lowerCurried (Curried f) = f (pure id)

liftCurriedYoneda :: Applicative f => f a -> Curried (Yoneda f) a
liftCurriedYoneda fa = Curried (`yap` fa)
{-# INLINE liftCurriedYoneda #-}

yap :: Applicative f => Yoneda f (a -> b) -> f a -> Yoneda f b
yap (Yoneda k) fa = Yoneda (\ab_r -> k (ab_r .) <*> fa)
{-# INLINE yap #-}

type Traversal s t a b
  = forall f. Applicative f => (a -> f b) -> s -> f t

class GHasParam (p :: Nat) s t a b where
  gparam :: forall g (x :: Type).  Applicative g => (a -> g b) -> s x -> g (t x)

instance (GHasParam p l l' a b, GHasParam p r r' a b) => GHasParam p (l :*: r) (l' :*: r') a b where
  gparam f (l :*: r) = (:*:) <$> gparam @p f l <*> gparam @p f r

instance (GHasParam p l l' a b, GHasParam p r r' a b) => GHasParam p (l :+: r) (l' :+: r') a b where
  gparam f (L1 l) = L1 <$> gparam @p f l
  gparam f (R1 r) = R1 <$> gparam @p f r

instance GHasParam p U1 U1 a b where
  gparam _ _ = pure U1

instance GHasParam p s t a b => GHasParam p (M1 m meta s) (M1 m meta t) a b where
  gparam f (M1 x) = M1 <$> gparam @p f x

instance GHasParam p (Rec (param p) a) (Rec (param p) b) a b where
  gparam = recIso

instance {-# OVERLAPPABLE #-}
  ( GHasParamRec (LookupParam si p) s t a b
  ) => GHasParam p (Rec si s) (Rec ti t) a b where
  gparam f (Rec (K1 x)) = Rec . K1 <$> gparamRec @(LookupParam si p) f x

class GHasParamRec (param :: Maybe Nat) s t a b | param t a b -> s, param s a b -> t where
  gparamRec :: forall g.  Applicative g => (a -> g b) -> s -> g t

instance GHasParamRec 'Nothing a a c d where
  gparamRec _ = pure

instance (HasParam n s t a b) => GHasParamRec ('Just n) s t a b where
  gparamRec = param @n

recIso :: Iso (Rec r a p) (Rec r b p) a b
recIso = iso (unK1 . unRec) (Rec . K1)

type Iso s t a b
  = forall p f. (Profunctor p, Functor f) => p a (f b) -> p s (f t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)
{-# INLINE iso #-}

type LookupParam :: k -> Nat -> Maybe Nat
type family LookupParam a p where
  LookupParam (param (n :: Nat)) m = 'Nothing
  LookupParam (a (_ (m :: Nat))) n = IfEq m n ('Just 0) (MaybeAdd (LookupParam a n) 1)
  LookupParam (a _) n = MaybeAdd (LookupParam a n) 1
  LookupParam a _ = 'Nothing

type family MaybeAdd (a :: Maybe Nat) (b :: Nat) :: Maybe Nat where
  MaybeAdd 'Nothing _  = 'Nothing
  MaybeAdd ('Just a) b = 'Just (a + b)

type family IfEq (a :: k) (b :: k) (t :: l) (f :: l) :: l where
  IfEq a a t _ = t
  IfEq _ _ _ f = f

data Sub where
  Sub :: Nat -> k -> Sub

type ReplaceArg :: k -> Nat -> j -> k
type family ReplaceArg t pos to where
  ReplaceArg @_ @j (t (a :: j)) 0 to = t to
  ReplaceArg (t a) pos to = ReplaceArg t (pos - 1) to a
  ReplaceArg t _ _ = t

type ReplaceArgs :: k -> [Sub] -> k
type family ReplaceArgs t subs where
  ReplaceArgs t '[] = t
  ReplaceArgs t ('Sub n arg ': ss) = ReplaceArgs (ReplaceArg t n arg) ss

type ArgAt :: k -> Nat -> j
type family ArgAt t n where
  ArgAt @_ @j (t (a :: j)) 0 = a
  ArgAt (t a) n = ArgAt t (n - 1)

type Unify :: k -> k -> [Sub]
type family Unify a b where
  Unify (p (n :: Nat) _ 'PTag) a' = '[ 'Sub n a']
  Unify (a (x :: k)) (b (y :: k)) = Unify x y ++ Unify a b
  Unify a a = '[]

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Infer (s :: Type) (a' :: Type) (b :: Type) :: Type where
  Infer (s a) a' b
    = ReplaceArgs (s a) (Unify a' b)
  Infer s _ _ = s

data PTag = PTag
type family P :: Nat -> k -> PTag -> k
type family Param :: Nat -> k where

type Indexed :: k -> Nat -> k
type family Indexed t i where
  Indexed (t a) i = Indexed t (i + 1) (Param i)
  Indexed t _     = t

newtype Rec (p :: Type) a x = Rec { unRec :: K1 R a x }

type family Zip (a :: Type -> Type) (b :: Type -> Type) :: Type -> Type where
  Zip (M1 mt m s) (M1 mt m t)
    = M1 mt m (Zip s t)
  Zip (l :+: r) (l' :+: r')
    = Zip l l' :+: Zip r r'
  Zip (l :*: r) (l' :*: r')
    = Zip l l' :*: Zip r r'
  Zip (Rec0 p) (Rec0 a)
    = Rec p a
  Zip U1 U1
    = U1

class
  ( Coercible (Rep a) (RepN a)
  , Generic a
  ) => GenericN (a :: Type) where
  type family RepN (a :: Type) :: Type -> Type
  type instance RepN a = Zip (Rep (Indexed a 0)) (Rep a)
  toN :: RepN a x -> a
  fromN :: a -> RepN a x
instance
  ( Coercible (Rep a) (RepN a)
  , Generic a
  ) => GenericN a where
  toN :: forall x. RepN a x -> a
  toN   = coerce (to :: Rep a x -> a)
  {-# INLINE toN #-}

  fromN :: forall x. a -> RepN a x
  fromN = coerce (from :: a -> Rep a x)
  {-# INLINE fromN #-}
