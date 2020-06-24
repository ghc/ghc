module GHC.CmmToAsm.Reg.Utils
    ( toRegMap
    , toVRegMap
    )
where

import GHC.Types.Unique.FM

toRegMap :: UniqFM anyKey -> UniqFM Reg elt
toRegMap = unsafeCastUFMKey

-- toVRegMap