{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T18740c where

import Data.Proxy
import Language.Haskell.TH.Syntax

[d| f (Proxy :: Proxy a) = a |] >>= addTopDecls >> return []
