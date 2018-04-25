module R where
import P  -- p (exposed), q (reexport p:P)
import P2 -- q (reexport p:P)
import Q  -- q (exposed)
import qualified QP -- q (reexport p:P)
import qualified QQ -- q (reexport q:Q)
import qualified PMerge -- q (reexport p:P)
import qualified PMerge2 -- q (reexport p:P2)
import qualified QMerge -- q (reexport q:Q)
data R = R
r = p && q
