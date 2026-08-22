-- Specialising the span-less copy of T27628h_M.merge must warn with the
-- "inlined from another module" wording and a module-qualified constructor.
module T27628h where

import T27628h_M

g :: Int -> T Int -> T Int
g x t = f x (f x t)
