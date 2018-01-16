module S where
-- NB: package p is hidden!
import qualified QP     -- q (reexport p:P)
import qualified RP     -- r (reexport p:P)
import qualified Q      -- q (exposed), r (reexport q:Q)
import qualified R      -- r (exposed)
import qualified RR     -- r (reexport r:R)
import qualified RP     -- r (reexport p:P)
import qualified RQP    -- r (reexport p:P)
import qualified RQP2   -- r (reexport p:P)
import qualified PMerge  -- q (reexport p:P), r (reexport p:P)
import qualified PMerge2 -- q (reexport p:P2), r (reexport p:P2)
import qualified QMerge  -- q (reexport q:Q), r (reexport q:Q)

x :: QP.P
x = RP.P

s = QP.p || Q.q || R.r
