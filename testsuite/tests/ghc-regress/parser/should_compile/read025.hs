-- !!! Check the handling of 'qualified' and 'as' clauses
module ShouldCompile where

import List as L ( intersperse ) 

x = L.intersperse

y = intersperse

