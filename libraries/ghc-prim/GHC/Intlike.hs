{-# LANGUAGE MagicHash, NoImplicitPrelude #-}

module GHC.Intlike where

import GHC.Prim

-- It seems the definition below triggers loading of the GHC.Types .hi file
-- We make sure that's compiled first by importing it here.
import GHC.Types ()

-- See Note [INTLIKE closures] in StcMiscClosures.cmm
-- for more details.
data StaticIntLike = ConstILike# Word#
