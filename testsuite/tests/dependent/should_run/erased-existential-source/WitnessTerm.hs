{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
module WitnessTerm where

import GHC.Exists
import Provider

bad = case supplied of Pack (type a) _ -> a
