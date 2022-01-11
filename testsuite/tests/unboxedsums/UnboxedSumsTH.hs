{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}

module UnboxedSumsTH where

import Data.Proxy
import Language.Haskell.TH

-- Check that we can quote the type constructor (# | #).
testTC :: Proxy (# | #)
testTC = $( conE 'Proxy `appTypeE` conT ''(# | #) )
