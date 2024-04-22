module Prelude
  (storefront, module Reexport) where

import Data.Semigroup as Reexport ((<>))
import System.IO as Reexport (putStrLn)

storefront = "A project-local Prelude"

backyard = "unexported local definition -- shall be visible only after *-import"
