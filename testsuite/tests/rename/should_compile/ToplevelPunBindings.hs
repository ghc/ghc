{-# LANGUAGE Haskell2010 #-}
module ToplevelPunBindings where

import Prelude (Bool)

data Same = Same
data Split = MakeSplit
data Other = Split
data ShadowsImport = Bool

observe :: Same -> Split
observe Same = MakeSplit
