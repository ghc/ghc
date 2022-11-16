{-# LANGUAGE TypeData #-}
module TH_typedata where

import Language.Haskell.TH

-- See #22500
ds1 :: Q [Dec]
ds1 = [d| type data T |]
