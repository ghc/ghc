{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module T10604_Authenticated where

import GHC.Generics

data AuthParam = Prover | Verifier

class MapAuth f where
    mapAuth :: f 'Prover -> f 'Verifier
    default mapAuth :: (GMapAuth (Rep1 f), Generic1 f)
                    => f 'Prover -> f 'Verifier
    mapAuth = to1 . gmapAuth . from1

class GMapAuth f where
    gmapAuth :: f 'Prover -> f 'Verifier

instance GMapAuth V1 where
    gmapAuth = undefined

instance GMapAuth U1 where
    gmapAuth U1 = U1

instance (GMapAuth l, GMapAuth r) => GMapAuth (l :+: r) where
    gmapAuth (L1 x) = L1 (gmapAuth x)
    gmapAuth (R1 x) = R1 (gmapAuth x)

instance (GMapAuth l, GMapAuth r) => GMapAuth (l :*: r) where
    gmapAuth (x :*: y) = gmapAuth x :*: gmapAuth y

instance GMapAuth (K1 i c) where
    gmapAuth (K1 c) = K1 c

instance (GMapAuth f) => GMapAuth (M1 i t f) where
    gmapAuth (M1 x) = M1 (gmapAuth x)

instance (MapAuth f) => GMapAuth (Rec1 f) where
    gmapAuth (Rec1 x) = Rec1 (mapAuth x)
