module B(module A) where

import A hiding (Bar)
import A(Bar)
