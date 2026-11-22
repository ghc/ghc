{-# LANGUAGE GADTs, NoRequiredTypeArguments, TemplateHaskell #-}
module GadtEmptyRequiredOff where

import Language.Haskell.TH (Dec, Q)

quoted :: Q [Dec]
quoted = [d| data Quoted where
               Quoted :: Bool -> (forall -> Quoted)
           |]
