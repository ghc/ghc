-- This test killed GHC 6.0.2 when it read the interface file for
-- Tc170_Aux, because there was a 
--	forall a. (# ... #)
-- in the default method for 'position'
--
-- NB: only fails when compiled in batch mode. In --make mode, GHC
--     doesn't read the interface file, so all is well.

module ShouldCompile where

import Tc170_Aux

data Bitmap = Bitmap

instance Dimensions Bitmap where
  frame = error "urk"