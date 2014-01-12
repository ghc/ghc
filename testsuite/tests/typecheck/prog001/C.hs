module C where

-- !!! a test for missing instances w/ functional dependencies 
-- (failed in GHC 5.00.2)

import A
import B

ct0a = row [True,False,True,False]
