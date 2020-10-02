module T16745A where

import T16745B hiding (field)
import T16745D hiding (foo)

wrong = foo -- should not be in scope
