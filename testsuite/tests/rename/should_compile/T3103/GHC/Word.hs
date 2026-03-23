{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module GHC.Word (
    Word(..),
    ) where

import GHC.Base
import GHC.Types
import GHC.Num ()
import qualified GHC.Internal.Num  as Rebindable
import qualified GHC.Internal.Base as Rebindable

import {-# SOURCE #-} GHC.Unicode ()

instance Num Word where
    signum 0               = 0
    signum _               = 1
