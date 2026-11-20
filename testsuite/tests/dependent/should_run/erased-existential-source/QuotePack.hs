{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
module QuotePack where

import GHC.Exists
import Provider

quoted :: Hidden
quoted = $([| Pack (type Bool) (Just True) |])

opened :: Bool
opened = $([| case quoted of Pack (type a) _ -> True |])
