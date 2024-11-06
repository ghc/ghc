{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, NoListTuplePuns #-}

module T23501a where

import Prelude.Experimental (List, Unit)
import Data.Kind (Type, Constraint)

----------------------------
--   Class declarations   --
----------------------------

class C1 (_ :: k) _   -- no SAKS
  where f1 :: k -> Unit

f1' :: C1 @k a b => k -> Unit
f1' @_ @a @b = f1 @_ @a @b

type C2 :: k1 -> k2 -> Constraint
class C2 (_ :: k) _ where
  f2 :: k -> Unit

f2' :: C2 @k1 @k2 a b => k1 -> Unit
f2' @_ @_ @a @b = f2 @_ @_ @a @b

type C3 :: k1 -> k2 -> Constraint
class C3 @k @_ _ _ where
  f3 :: k -> Unit

f3' :: C3 @k1 @k2 a b => k1 -> Unit
f3' @_ @_ @a @b = f3 @_ @_ @a @b

---------------------------
--   Data declarations   --
---------------------------

data D1 k (_ :: k) _  -- no SAKS
  where MkD1 :: k -> D1 k a b

mkD1 :: k -> D1 k a b
mkD1 = MkD1

type D2 :: forall (k1 :: Type) -> k1 -> k2 -> Type
data D2 k (_ :: k) _ where
  MkD2 :: k -> D2 k a b

mkD2 :: k -> D2 k a b
mkD2 = MkD2

type D3 :: k1 -> k2 -> Type
data D3 @k @_ _ _ = MkD3 k

data MProxy (_ :: Type) = MPrx
data CProxy (_ :: k -> Constraint) = CPrx

type Rec :: (k -> Type) -> List k -> Type
data Rec _ _ where
  RNil :: Rec f []
  (:&) :: f x -> Rec f xs -> Rec f (x:xs)

------------------
--   Newtypes   --
------------------

newtype N1 k (_ :: k) _   -- no SAKS
  = MkN1 k

mkN1 :: k -> N1 k a b
mkN1 = MkN1

type N2 :: forall (k1 :: Type) -> k1 -> k2 -> Type
newtype N2 k (_ :: k) _ = MkN2 k

mkN2 :: k -> N2 k a b
mkN2 = MkN2

----------------------------
--   Open type families   --
----------------------------

type family OTF1 (_ :: Type -> Type) _    -- no SAKS

type instance OTF1 f x = f x

otf1 :: OTF1 Maybe Int -> Int
otf1 Nothing = 0
otf1 (Just x) = x

type OTF2 :: (Type -> Type) -> Type -> Type
type family OTF2 (_ :: Type -> Type) _

type instance OTF2 f x = f x

otf2 :: OTF2 Maybe Int -> Int
otf2 Nothing = 0
otf2 (Just x) = x

------------------------------
--   Closed type families   --
------------------------------

type family CTF1 (_ :: Type -> Type) _  -- no SAKS
  where CTF1 f x = f x

ctf1 :: CTF1 Maybe Int -> Int
ctf1 Nothing = 0
ctf1 (Just x) = x

type CTF2 :: (Type -> Type) -> Type -> Type
type family CTF2 (_ :: Type -> Type) _ where
  CTF2 f x = f x

ctf2 :: CTF2 Maybe Int -> Int
ctf2 Nothing = 0
ctf2 (Just x) = x

type CTF3 :: k1 -> k2 -> Type
type family CTF3 @_ @k _ _
  where CTF3 @_ @k _ _ = k

ctf3 :: CTF3 a True -> Bool
ctf3 = id

-----------------------
--   Type synonyms   --
-----------------------

type T1 (_ :: Type -> Type) _ = ()   -- no SAKS

type T2 :: (Type -> Type) -> k -> Type
type T2 (_ :: Type -> Type) _ = Unit

type T3 :: k1 -> k2 -> Type
type T3 @_ @k _ _ = k

type FConst _ = ()
