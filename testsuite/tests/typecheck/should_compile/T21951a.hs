{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MagicHash #-}

module Wibble where

import Data.Kind
import GHC.Exts

data UA = UA ~(Array# Int)
