{-# LANGUAGE DataKinds, ExperimentalDependentTypes, RequiredTypeArguments #-}
module BarPack where

import Filter
import GHC.Exists
import Vector

bad :: Hidden Int
bad = Pack (type 'Z) Nil
