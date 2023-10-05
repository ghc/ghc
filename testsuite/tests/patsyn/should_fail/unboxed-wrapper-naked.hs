{-# LANGUAGE PatternSynonyms, MagicHash #-}
module ShouldFail where

import GHC.Base

pattern P1 = 42#

x = P1
