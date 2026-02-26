{-# LANGUAGE TemplateHaskell #-}

module TH_EmptyMultiIf where

import Language.Haskell.TH

f = $( multiIfE [] )