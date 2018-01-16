{-# LANGUAGE PolyKinds, DataKinds #-}
module T11056 where
import Data.Typeable

data T = A | B Int

bar :: TypeRep
bar = typeRep (Proxy :: Proxy '[True])

baz :: TypeRep
baz = typeRep (Proxy :: Proxy 'A)

quux :: TypeRep
quux = typeRep (Proxy :: Proxy 'B)
