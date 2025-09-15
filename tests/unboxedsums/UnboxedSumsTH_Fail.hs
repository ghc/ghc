{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}

module UnboxedSumsTH_Fail where

import Data.Proxy
import Language.Haskell.TH

-- (# | #) is not a valid data constructor,
-- as it doesn't indicate which alternative we are taking.
testDC :: (# Integer | Bool #)
testDC = $( conE '(# | #) `appE` litE (IntegerL 77) )
