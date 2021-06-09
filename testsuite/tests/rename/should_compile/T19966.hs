module T19966 where

-- I is out of scope, so out-of-scope is a warning.
x = I

-- IO is out of scope, so out-of-scope should be a warning, but fails.
y = IO
