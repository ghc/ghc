-- Like T27628h, but the constructor is in scope only qualified (M.Bin),
-- so the ambient qual policy qualifies it in the warning.  The warning's
-- own module prefix must not stack on top: expect ‘T27628h_M.Bin’, not
-- ‘T27628h_M.M.Bin’.
module T27628m where

import qualified T27628h_M as M

g :: Int -> M.T Int -> M.T Int
g x t = M.f x (M.f x t)
