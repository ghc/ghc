{-# LANGUAGE DataKinds, GADTs, LinearTypes, RequiredTypeArguments, TemplateHaskell #-}
module TerminalGadtThBadMultiplicity where

$([d| data G a b where
        After :: forall a. !Int %True -> forall b -> G a b
    |])
