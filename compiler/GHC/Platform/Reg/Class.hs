-- | An architecture independent description of a register's class.
module GHC.Platform.Reg.Class
        ( RegClass (..) )

where

import GHC.Prelude

import GHC.Utils.Outputable as Outputable
import GHC.Types.Unique
import GHC.Builtin.Uniques


-- | The class of a register.
--      Used in the register allocator.
--      We treat all registers in a class as being interchangeable.
--
data RegClass
        = RcInteger
        | RcFloat
        | RcDouble
        deriving Eq


instance Uniquable RegClass where
    getUnique RcInteger = mkRegClassUnique 0
    getUnique RcFloat   = mkRegClassUnique 1
    getUnique RcDouble  = mkRegClassUnique 2

instance Outputable RegClass where
    ppr RcInteger       = Outputable.text "I"
    ppr RcFloat         = Outputable.text "F"
    ppr RcDouble        = Outputable.text "D"
