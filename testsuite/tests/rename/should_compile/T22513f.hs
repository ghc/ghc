module T22513f where

import Data.Proxy

p :: () -> forall (a :: id). Proxy a
p () = Proxy