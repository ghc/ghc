{-# LANGUAGE TypeFamilies, GADTs #-}

module GADT13 where

data family HiThere a :: *

data instance HiThere () where
    HiThere :: HiThere ()
