{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StarIsType #-}
module T15360b where

import Data.Kind
import Data.Proxy

x :: Proxy $([t| * Double |])
x = Proxy

y :: Proxy $([t| 1 Int |])
y = Proxy

z :: Proxy $([t| Constraint Bool |])
z = Proxy

w :: Proxy $([t| '[] Int |])
w = Proxy
