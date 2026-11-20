{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Escape where

import GHC.Exists
import Provider

bad = case supplied of Pack (type a) x -> x
