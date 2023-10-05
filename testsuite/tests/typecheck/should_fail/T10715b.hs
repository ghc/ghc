module T10715b where

-- test error message: should complain about an occurs check

import Data.Coerce

foo = coerce `asTypeOf` head
