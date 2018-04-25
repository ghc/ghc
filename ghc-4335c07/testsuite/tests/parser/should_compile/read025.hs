-- !!! Check the handling of 'qualified' and 'as' clauses
module ShouldCompile where

import Data.List as L ( intersperse ) 

x = L.intersperse

y = intersperse

