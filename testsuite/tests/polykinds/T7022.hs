{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds, TemplateHaskell #-}

module T7022 where

import T7022b

foo :: SList a -> Bool
foo = undefined

