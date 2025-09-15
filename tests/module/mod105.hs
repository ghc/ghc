-- !!! Re-defining Prelude entities
module Ring where
import qualified Prelude
import Data.List ( nub )

l1 + l2 = l1 Prelude.++ l2
l1 * l2 = nub (l1 + l2)

succ = (Prelude.+ 1)
