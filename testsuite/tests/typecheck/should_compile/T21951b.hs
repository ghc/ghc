{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE MagicHash #-}

module Wibble where

import Data.Kind
import GHC.Exts

type U :: UnliftedType
data U = MkU Int

data T = T ~U
