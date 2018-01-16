{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds, TemplateHaskell #-}

module T7022b where

import T7022a

data family Sing (a :: k)

$( makeSList )
