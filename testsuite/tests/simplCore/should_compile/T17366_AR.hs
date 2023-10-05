{-# LANGUAGE DataKinds #-}
module T17366_AR where

import T17366_ARa

--{-# SPECIALIZE test :: Eff es () #-}

--testSpec :: Eff '[] () -- Specialization of 'test' works.
testSpec :: Eff es () -- Specialization of 'test' doesn't work.
testSpec = do
  test
  test
  test

-- Specialization of 'smallTest' works only if the INLINABLE pragma for 'smallTest'
-- is commented out (!!!).
smallTestSpec :: Eff es ()
smallTestSpec = do
  smallTest
  smallTest
  smallTest
