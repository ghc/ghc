module T23329 where

import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))

import T23329_M

foo :: ()
foo = myMethod @Type @MyMaybe @() () Proxy Proxy
