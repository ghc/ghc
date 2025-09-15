-- !!! Monomorphism restriction
-- This one should work fine, despite the monomorphism restriction
-- Fails with GHC 5.00.1

module Test where
import Control.Monad.ST
import Data.STRef

-- Should get
-- apa :: forall s. ST s ()
apa = newSTRef () >> return  ()

foo1 = runST apa
