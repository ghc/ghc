{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module PatternSyns2
  ( pattern P1, pattern P2, pattern P3, pattern P4, pattern P5
  , pattern P
  )
  where

import Data.Kind
import Data.Proxy
import GHC.Exts

type D :: ( Type -> Constraint ) -> Type -> Type
data D c a where
  MkD :: c a => a -> D c a

pattern P1 :: () => Num a => a -> D Num a
pattern P1 a = MkD a

pattern P2 :: Num a => () => a -> a
pattern P2 a = a

type RCIR :: TYPE IntRep -> Constraint
class RCIR a where

type PCIR :: TYPE IntRep -> Constraint
class PCIR a where

type PCDR :: TYPE DoubleRep -> Constraint
class PCDR a where

type Q :: TYPE IntRep -> Type
data Q a where
  MkQ :: forall ( a :: TYPE IntRep ) ( e :: TYPE DoubleRep )
      .  ( PCIR a, PCDR e )
      => a -> e -> Q a

pattern P3 :: forall (a :: TYPE IntRep). () => forall (e :: TYPE DoubleRep). (PCIR a, PCDR e) => a -> e -> Q a
pattern P3 a e = MkQ a e

pattern P4 :: forall (a :: TYPE IntRep). (RCIR a) => forall (e :: TYPE DoubleRep). (PCIR a, PCDR e) => a -> e -> Q a
pattern P4 a e = MkQ a e

pattern P5 :: forall (a :: TYPE IntRep). (RCIR a) => forall (e :: TYPE DoubleRep). () => a -> e -> Q a
pattern P5 a e <- MkQ a e


type A :: Type
data A where
  MkA :: forall k (a ::k) b. ( Show b ) => Proxy a -> b -> A

pattern P :: forall . () => forall k (a :: k) b. ( Show b ) => Proxy a -> b -> A
pattern P a b = MkA a b
