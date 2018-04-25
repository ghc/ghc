-- !!! Check that 'qualified' doesn't bring the unqual'ed name into scope.
module ShouldFail where

import qualified Data.List as L ( intersperse )

x = L.intersperse

y = intersperse

