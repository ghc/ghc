module T23176 where

import GHC.Exts

f = outOfScope :: (_ :: TYPE (r s))
(g :: _) = outOfScope :: (_ :: TYPE (r s))
