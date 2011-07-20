-- !!! hiding Prelude method names
-- (based on bug report by Iavor Diatchi:
--   http://haskell.org/pipermail/hugs-bugs/2003-October/001369.html
-- )
module M where

import Prelude hiding ( negate, enumFrom, 
		        enumFromThen, enumFromTo,
		        enumFromThenTo )
import Data.Ix hiding ( rangeSize )
negate = undefined
enumFrom = undefined
enumFromThen = undefined
enumFromTo = undefined
enumFromThenTo = undefined
rangeSize = undefined

x = [negate,enumFrom, enumFromThen, enumFromTo, enumFromThenTo, rangeSize]


