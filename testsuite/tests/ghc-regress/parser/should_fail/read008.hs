module ShouldFail where

-- strictness annotations on the argument to a newtype constructor
-- are not allowed.
newtype N a = T ![a]
