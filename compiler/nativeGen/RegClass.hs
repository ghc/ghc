-- | An architecture independent description of a register's class.
module RegClass
        ( VecRegWidth(..), vecRegWidthBits
        , RegClass(..)
        ) where

import GhcPrelude

import  Outputable
import  Unique

-- | The width of a vector register.
data VecRegWidth = Vec128 | Vec256 | Vec512
                 deriving (Eq, Ord, Show)

-- | The width of a vector register in bits.
vecRegWidthBits :: VecRegWidth -> Int
vecRegWidthBits Vec128 = 128
vecRegWidthBits Vec256 = 256
vecRegWidthBits Vec512 = 512

-- | The class of a register.
--      Used in the register allocator.
--      We treat all registers in a class as being interchangable.
--
data RegClass
        = RcInteger
        | RcFloat
        | RcDouble
        | RcVector VecRegWidth
        deriving Eq


instance Uniquable RegClass where
    getUnique RcInteger = mkRegClassUnique 0
    getUnique RcFloat   = mkRegClassUnique 1
    getUnique RcDouble  = mkRegClassUnique 2
    getUnique (RcVector Vec128)  = mkRegClassUnique 3
    getUnique (RcVector Vec256)  = mkRegClassUnique 4
    getUnique (RcVector Vec512)  = mkRegClassUnique 5

instance Outputable RegClass where
    ppr RcInteger         = Outputable.text "I"
    ppr RcFloat           = Outputable.text "F"
    ppr RcDouble          = Outputable.text "D"
    ppr (RcVector Vec128) = Outputable.text "V128"
    ppr (RcVector Vec256) = Outputable.text "V256"
    ppr (RcVector Vec512) = Outputable.text "V512"
