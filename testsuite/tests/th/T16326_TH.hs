{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T16326_TH where

import Control.Monad.IO.Class
import Data.Kind
import Data.Proxy
import Language.Haskell.TH hiding (Type)
import System.IO

data Foo :: forall a -> a -> Type
type family Foo2 :: forall a -> a -> Type where
  Foo2 = Foo

$(do info <- reify ''Foo2
     liftIO $ hPutStrLn stderr $ pprint info

     dec <- [d| data Nested :: forall a. forall b -> forall c.
                               forall d -> forall e.
                               Proxy '[a,b,c,d,e] -> Type |]
     liftIO $ hPutStrLn stderr $ pprint dec
     pure dec)
