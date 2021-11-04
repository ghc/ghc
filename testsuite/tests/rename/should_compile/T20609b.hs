module T20609b where

-- Triggers the warning (definition/binding sites):
-- ------------------------------------------------

class MyClass c where
  forall :: c -> ()

-- Does not trigger the warning (use sites):
-- -----------------------------------------

instance MyClass () where
  forall = id
