{-# OPTIONS_GHC -fwarn-unused-imports  #-}

import GHC.Prim (coerce)
import Data.Monoid (First(First)) -- check whether the implicit use of First is noted

main = print (coerce $ Just (1::Int)  :: First Int)



