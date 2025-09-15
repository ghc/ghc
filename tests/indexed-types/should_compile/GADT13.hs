{-# LANGUAGE TypeFamilies, GADTs #-}

module GADT13 where

import Data.Kind (Type)

data family HiThere a :: Type

data instance HiThere () where
    HiThere :: HiThere ()
