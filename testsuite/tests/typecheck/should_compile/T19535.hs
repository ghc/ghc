{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module T19535 where

import GHC.TypeNats (natVal)
import GHC.TypeLits hiding (natVal)
import Data.Type.Equality
import Data.Proxy

e1 :: CharToNat 'a' :~: 97
e1 = Refl

e2 :: NatToChar 120 :~: 'x'
e2 = Refl

ntc :: forall {n} c. (KnownNat n, NatToChar n ~ c) => Natural
ntc = natVal (Proxy @n)

ctn :: forall {c} n. (KnownChar c, CharToNat c ~ n) => Char
ctn = charVal (Proxy @c)

n1 :: Natural
n1 = ntc @'z'

c1 :: Char
c1 = ctn @122

ntc_ntc :: forall {n} m. (KnownNat n, NatToChar n ~ NatToChar (m + 1)) => Natural
ntc_ntc = natVal (Proxy @n)

ctn_ctn :: forall {c} d. (KnownChar c, CharToNat c ~ (CharToNat d + 1)) => Char
ctn_ctn = charVal (Proxy @c)

n2 :: Natural
n2 = ntc_ntc @119

c2 :: Char
c2 = ctn_ctn @'w'
