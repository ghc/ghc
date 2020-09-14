{-# LANGUAGE TypeFamilies #-}

module GHC.Plugins.Frontend
   ( defaultFrontendPlugin
   )
where

import GHC.Prelude

import GHC.Plugins.Types

import GHC.Driver.Monad
import GHC.Driver.Phases

type instance GHC.Plugins.Types.TFrontendPluginAction
   = [String] -> [(String, Maybe Phase)] -> Ghc ()

defaultFrontendPlugin :: FrontendPlugin
defaultFrontendPlugin = FrontendPlugin { frontend = \_ _ -> return () }
