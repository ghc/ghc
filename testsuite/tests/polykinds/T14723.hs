{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module T14723 () where

import Data.Coerce( coerce )
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.String (fromString)
import Data.Int (Int64)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (Nat, Symbol)

data JType = Iface Symbol

data J (a :: JType)

newIterator
  :: IO (J ('Iface "java.util.Iterator"))
newIterator = do
    let tblPtr :: Int64
        tblPtr = undefined
    iterator <-
         (qqMarker (Proxy :: Proxy "wuggle")
                   (Proxy :: Proxy "waggle")
                   (Proxy :: Proxy "tblPtr")
                   (Proxy :: Proxy 106)
                   (tblPtr, ())
                   Proxy
                   (undefined :: IO Int))
    undefined

class Coercible (a :: Type) where
  type Ty a :: JType

instance Coercible Int64 where
  type Ty Int64 = Iface "Int64"
instance Coercible Int where
  type Ty Int = Iface "Int"

class Coercibles xs (tys :: k) | xs -> tys
instance Coercibles () ()
instance (ty ~ Ty x, Coercible x, Coercibles xs tys) => Coercibles (x, xs) '(ty, tys)

qqMarker
  :: forall
     -- k                -- the kind variable shows up in Core
     (args_tys :: k)     -- JType's of arguments
     tyres               -- JType of result
     (input :: Symbol)   -- input string of the quasiquoter
     (mname :: Symbol)   -- name of the method to generate
     (antiqs :: Symbol)  -- antiquoted variables as a comma-separated list
     (line :: Nat)       -- line number of the quasiquotation
     args_tuple          -- uncoerced argument types
     b.                  -- uncoerced result type
     (tyres ~ Ty b, Coercibles args_tuple args_tys, Coercible b, HasCallStack)
  => Proxy input
  -> Proxy mname
  -> Proxy antiqs
  -> Proxy line
  -> args_tuple
  -> Proxy args_tys
  -> IO b
  -> IO b
qqMarker = undefined
