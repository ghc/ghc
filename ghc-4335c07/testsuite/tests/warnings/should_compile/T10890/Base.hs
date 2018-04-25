module Base (AClass(..), BClass()) where

import Extends (BClass ())

class AClass a where
  has :: a
