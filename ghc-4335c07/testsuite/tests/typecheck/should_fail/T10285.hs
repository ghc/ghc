module T10285 where

import T10285a
import Data.Type.Coercion
import Data.Coerce

oops :: Coercion (N a) (N b) -> a -> b
oops Coercion = coerce

unsafeCoerce :: a -> b
unsafeCoerce = oops coercion
