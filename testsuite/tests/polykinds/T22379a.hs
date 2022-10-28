{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Bug where

import Data.Kind
import Data.Proxy (Proxy)

data Delayed (env :: Type) (c :: Type)
data Handler (a :: Type)
data Router (a :: Type)

-- class decl, then type decl

class HasServer api where
  type ServerT api (m :: Type -> Type) :: Type

  route ::
       Proxy api
    -> Delayed env (Server api)
    -> Router env

  hoistServerWithContext
      :: Proxy api
      -> (forall x. m x -> n x)
      -> ServerT api m
      -> ServerT api n

type Server aqi = ServerT aqi Handler

