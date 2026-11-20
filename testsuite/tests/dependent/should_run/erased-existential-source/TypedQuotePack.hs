{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module TypedQuotePack where

import GHC.Exists
import Provider

quoted :: Hidden
quoted = $$([|| Pack @Hidden (type Bool) (Just True) ||])
