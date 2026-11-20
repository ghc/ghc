{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
module Scope where

import Data.Proxy
import GHC.Exists
import Provider

bad = (case supplied of Pack (type a) _ -> (), Proxy @a)
