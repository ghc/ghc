{-# LANGUAGE TemplateHaskell #-}

module T25127_fail_th_quote where

import System.IO
import Language.Haskell.TH

-- Splicing a constructor with a required binder still needs RequiredTypeArguments.
$([d| data VisProxy a where
        VP :: forall a -> VisProxy a
    |])
