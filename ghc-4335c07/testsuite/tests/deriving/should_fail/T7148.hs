{-# LANGUAGE GeneralizedNewtypeDeriving, GADTs #-}

module T7148 where

data SameType a b where
  Refl :: SameType a a
 
coerce :: SameType a b -> a -> b
coerce Refl = id
 
trans :: SameType a b -> SameType b c -> SameType a c
trans Refl Refl = Refl
 
sameUnit :: SameType () ()
sameUnit = Refl


class IsoUnit a where
  iso1 :: SameType () b -> SameType a b
  iso2 :: SameType b () -> SameType b a
 
instance IsoUnit () where
  iso1 = id
  iso2 = id


newtype Tagged a b = Tagged b deriving IsoUnit

sameTagged :: SameType (Tagged a b) (Tagged a' b') -> SameType a a'
sameTagged Refl = Refl
 
unsafe' :: SameType (Tagged a ()) (Tagged a' ())
unsafe' = (iso1 sameUnit) `trans` (iso2 sameUnit)
 
unsafe :: SameType a b
unsafe = sameTagged unsafe'
 
--once again inferred type is a -> b
unsafeCoerce = coerce unsafe