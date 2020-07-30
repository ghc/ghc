{-# LANGUAGE MagicHash #-}

import GHC.Num.BigNat
import GHC.Num.Integer

main :: IO ()
main =
    let b = integerToBigNatClamp# 251943445928310882947152017889649234
        e = integerToBigNatClamp# 503886891856621765894304035779298468
        m = integerToBigNatClamp# 503886891856621765894304035779298469
        r = integerFromBigNat# (bigNatPowMod b e m)
     in print r
