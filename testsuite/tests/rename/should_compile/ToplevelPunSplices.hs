{-# LANGUAGE TemplateHaskell #-}
module ToplevelPunSplices where

import Data.List (List)
import Language.Haskell.TH (Dec, Q)
import Prelude (Bool, pure)

data Light = MkLight
$(pure [])
data First = Light

data Second = Shade
$(pure [])
data Shade = MkShade

data QuotedType = MkQuotedType
$(pure [])
data NoConflict = Bool
$(pure [])

quoted :: Q (List Dec)
quoted = [d| data Some = QuotedType |]
