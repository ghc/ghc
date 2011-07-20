module Foo where

import qualified Prelude
import Prelude hiding( Maybe )

data Maybe a = Foo

f :: Prelude.Maybe a -> Int
f Foo = 3
