{-# LANGUAGE KindSignatures, PolyKinds, DataKinds, RankNTypes #-}

module T6081 where

data KProxy (a :: *) = KP

class KindClass (kp :: KProxy k)
instance KindClass (KP :: KProxy [k])


