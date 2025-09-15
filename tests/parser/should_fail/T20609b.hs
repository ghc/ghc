module T20609b where

-- Declarations in this module used to be accepted by GHC
-- before `forall` became a keyword (#23719).

class MyClass c where
  forall :: c -> ()

instance MyClass () where
  forall = id
