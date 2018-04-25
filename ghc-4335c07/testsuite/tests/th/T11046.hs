{-# LANGUAGE TemplateHaskell #-}
module T11046 where

import T11046_helper
import GHC.TypeLits
import Control.Monad(unless)

$(check "GHC.TypeLits.*")
$(check "GHC.TypeLits.+")
$(check "GHC.TypeLits.Nat")
