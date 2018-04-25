{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE DataKinds                  #-}

module Main where


-- Type-level peano naturals (value-level too, but we don't use those)
data Nat = Ze | Su Nat

type T0 = Ze
type T1 = Su T0
type T2 = Su T1

-- (!) at the type level
type family El (n :: Nat) (l :: [*]) :: *
type instance El Ze     (h ': t) = h
type instance El (Su n) (h ': t) = El n t

{-
-- The following might be useful, but are not used at the moment
-- ($) at the type level (well, not quite ($), in fact...)
class Apply (fs :: [*]) (es :: [*]) where
  type ApplyT (fs :: [*]) (es :: [*]) :: [*]
  apply :: ListV fs -> ListV es -> ListV (ApplyT fs es)

instance Apply '[] '[] where
  type ApplyT '[] '[] = '[]
  apply NilV NilV = NilV

instance (Apply fs es) => Apply ((e1 -> e2) ': fs) (e1 ': es) where
  type ApplyT ((e1 -> e2) ': fs) (e1 ': es) = e2 ': ApplyT fs es
  apply (ConsV f fs) (ConsV e es) = ConsV (f e) (apply fs es)
-}

-- Value mirror for the list kind
data ListV :: [*] -> * where
  NilV  :: ListV '[]
  ConsV :: a -> ListV t -> ListV (a ': t)
  
data ListV2 :: [[*]] -> * where
  NilV2  :: ListV2 '[]
  ConsV2 :: ListV a -> ListV2 t -> ListV2 (a ': t)
  
listv1 :: ListV (Int ': '[])
listv1 = ConsV 3 NilV

listv2 :: ListV2 ((Int ': '[]) ': '[])
listv2 = ConsV2 listv1 NilV2

--data ListVX :: Maybe -> * where

data TripleV :: (*, * -> *, *) -> * where
  TripleV :: a -> c -> TripleV '(a, [], c)

-- Value mirror for the Nat kind
data NatV :: Nat -> * where
  ZeW :: NatV Ze
  SuW :: NatV n -> NatV (Su n)

-- Generic universe
data MultiP x = UNIT
              | KK x -- wish I could just write * instead of x
              | SUM  (MultiP x) (MultiP x)
              | PROD (MultiP x) (MultiP x)
              | PAR Nat
              | REC

-- Universe interpretation
data Interprt :: MultiP * -> [*] -> * -> * where
  Unit  :: Interprt UNIT lp r
  K     :: x -> Interprt (KK x) lp r
  L     :: Interprt a lp r -> Interprt (SUM a b) lp r
  R     :: Interprt b lp r -> Interprt (SUM a b) lp r
  Prod  :: Interprt a lp r -> Interprt b lp r -> Interprt (PROD a b) lp r
  Par   :: NatV n -> El n lp -> Interprt (PAR n) lp r
  Rec   :: r -> Interprt REC lp r

-- Embedding values into the universe
class Generic a where
  type Rep a :: MultiP *
  type Es a  :: [*]
  from :: a -> Interprt (Rep a) (Es a) a
  to   :: Interprt (Rep a) (Es a) a -> a

-- Parameter map over the universe
class PMap (rep :: MultiP *) where
  pmap :: (forall n. NatV n -> El n lp1 -> El n lp2)
       -> (r -> s) -> Interprt rep lp1 r -> Interprt rep lp2 s

instance PMap UNIT where
  pmap _ _ Unit = Unit

instance PMap (KK x) where
  pmap _ _ (K x) = K x

instance (PMap a, PMap b) => PMap (SUM a b) where
  pmap f g (L x) = L (pmap f g x)
  pmap f g (R x) = R (pmap f g x)

instance (PMap a, PMap b) => PMap (PROD a b) where
  pmap f g (Prod x y) = Prod (pmap f g x) (pmap f g y)

instance PMap (PAR p) where
  pmap f _ (Par n p) = Par n (f n p)

instance PMap REC where
  pmap _ g (Rec p) = Rec (g p)

-- User-facing function
pmapu :: (Generic a, Generic b, PMap (Rep a), Rep a ~ Rep b)
      => (forall n. NatV n -> El n (Es a)-> El n (Es b)) -> a -> b
pmapu f = to . pmap f (pmapu f). from


-- Example: lists
instance Generic [a] where
  type Rep [a] = SUM UNIT (PROD (PAR T0) REC)
  type Es  [a] = a ': '[]
  from [] = L Unit
  from (h:t) = R (Prod (Par ZeW h) (Rec t))
  to (L Unit) = []
  to (R (Prod (Par ZeW h) (Rec t))) = h:t
  
-- Map on lists: we can define an auxiliary function with the usual type...
pmapList :: forall a b. (a -> b) -> [a] -> [b]
pmapList f l = pmapu g l where
  g :: forall n. NatV n -> El n (Es [a]) -> El n (Es [b])
  g ZeW x = f x

-- ... or use pmapu directly
pmapExample1 :: [String]
pmapExample1 = pmapu f [1..10::Int] where
  f :: forall n. NatV n -> El n (Es [Int]) -> El n (Es [String])
  f ZeW = show

-- Example: Either
instance Generic (Either a b) where
  type Rep (Either a b) = SUM (PAR T0) (PAR T1)
  type Es  (Either a b) = a ': b ': '[]
  from (Left  a) = L (Par ZeW a)
  from (Right b) = R (Par (SuW ZeW) b)
  to (L (Par ZeW a)) = Left a
  to (R (Par (SuW ZeW) b)) = Right b

pmapEither :: forall a1 a2 b1 b2.
              (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
pmapEither f g = pmapu h where
  h :: forall n. NatV n -> El n (Es (Either a1 b1)) -> El n (Es (Either a2 b2))
  h ZeW = f
  h (SuW ZeW) = g


main = print pmapExample1
